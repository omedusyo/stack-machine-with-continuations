module Main exposing (..)

import ArithmeticEvaluator
import Browser
import Browser.Events as BE
import Css
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode as Decode
import List.Extra as List
import Return exposing (Return)



-- helpers


type Either a b
    = Left a
    | Right b


removeLast : List a -> List a
removeLast xs0 =
    case xs0 of
        [] ->
            []

        [ x ] ->
            []

        x :: xs1 ->
            x :: removeLast xs1



-- ===Stack Machine===


type alias VarName =
    String


type alias Env =
    -- I use `List Value` instead of `Value` because I worried about shadowing.
    -- But since I don't actually remove any variable bindings explicitely but just restore to a previous environment, the `List` doesn't seem to be necessary.
    Dict String (List Value)


insertEnv : VarName -> Value -> Env -> Env
insertEnv varName val env =
    env
        |> Dict.update varName
            (\maybeValues ->
                case maybeValues of
                    Just values ->
                        Just (val :: values)

                    Nothing ->
                        Just [ val ]
            )


getEnv : VarName -> Env -> Maybe Value
getEnv varName env =
    env |> Dict.get varName |> Maybe.andThen List.head


type Constant
    = -- Primitive Integers
      IntConst Int
      -- Bool
    | TrueConst
    | FalseConst


type Value
    = ConstantValue Constant
      -- Closure
    | ClosureValue Env { var : VarName, body : Computation }
      -- Stack/Continuation
    | StackValue Env Stack


type PrimitiveIntOperation2
    = Add
    | Mul
    | LessThan


type Computation
    = ConstantComputation Constant
      -- Variable use
    | VarUse VarName
      -- Primitive arithmetic
    | PrimitiveIntOperation2 PrimitiveIntOperation2 Computation Computation
      -- Bool type
    | IfThenElse Computation { body : Computation } { body : Computation }
      -- Function type
    | Lambda { var : VarName, body : Computation }
    | Application Computation Computation
      -- Stack/Current Continuation
    | SaveStack { var : VarName, body : Computation }
    | RestoreStackWith Computation Computation -- This is like function application, but we're "applying" a frozen stack `restoreStack k v`


type CurrentComputation
    = Value Value
    | Computation Computation


type StackElement
    = PrimitiveIntOperation2LeftHole PrimitiveIntOperation2 Computation -- op(_, M)
    | PrimitiveIntOperation2RightHole PrimitiveIntOperation2 Value -- op(V, _)
      -- Bool
    | IfThenElseHole { body : Computation } { body : Computation } -- if _ then M else M'
      -- Application
    | ApplicationLeftHole Computation -- [_ M]
    | ApplicationRightHole Value -- [Value _]
      -- Stack/Current Continuation
    | RestoreStackWithLeftHole Computation -- restoreStack _ M
    | RestoreStackWithRightHole Value -- restoreStack V _
      -- Environment restoration after closure application/stack save is done
    | RestoreEnv Env


type alias Stack =
    List StackElement


type alias State =
    { env : Env
    , stack : Stack
    , currentComputation : CurrentComputation
    }


type RunTimeError
    = ExpectedBoolean
    | ExpectedInteger
    | ExpectedClosure
    | ExpectedStack
    | UnboundVarName VarName



-- Right () means the computation has terminated


smallStepEval : State -> Result RunTimeError (Either State ())
smallStepEval ({ env, stack, currentComputation } as state) =
    case currentComputation of
        Value val ->
            case stack of
                [] ->
                    -- Current computation is a value, but the stack is empty, so we terminate
                    Ok (Right ())

                stackEl :: stack1 ->
                    -- Current computation is a value and we have work to do on the stack.
                    -- Based on what's on the stack we either fail or actually do some non-trivial computing
                    -- In either case the top of the stack is consumed
                    combineValWithTopOfStack env stack1 val stackEl
                        |> Result.map Left

        Computation comp ->
            -- Current computation is not a value, and so it will be decomposed into smaller pieces.
            -- Either additional work will be pushed onto the stack or the current computation will be transformed into a value
            decompose env stack comp
                |> Result.map Left


combineValWithTopOfStack : Env -> Stack -> Value -> StackElement -> Result RunTimeError State
combineValWithTopOfStack env stack val stackEl =
    -- The only interesting cases are `PrimitiveIntOperation2RightHole`, `IfThenElseHole`, `ApplicationRightHole`, `RestoreStackWithRightHole`, and `RestoreEnv`
    case stackEl of
        PrimitiveIntOperation2LeftHole op computation1 ->
            Ok { env = env, stack = PrimitiveIntOperation2RightHole op val :: stack, currentComputation = Computation computation1 }

        PrimitiveIntOperation2RightHole op value0 ->
            case ( value0, val ) of
                ( ConstantValue (IntConst x), ConstantValue (IntConst y) ) ->
                    Ok { env = env, stack = stack, currentComputation = Value (applyPrimitiveOp2 op x y) }

                _ ->
                    Err ExpectedInteger

        -- Bool
        IfThenElseHole leftBranch rightBranch ->
            (case val of
                ConstantValue TrueConst ->
                    Ok leftBranch

                ConstantValue FalseConst ->
                    Ok rightBranch

                _ ->
                    Err ExpectedBoolean
            )
                |> Result.map (\branch -> { env = env, stack = stack, currentComputation = Computation branch.body })

        -- Application
        ApplicationLeftHole computation1 ->
            Ok { env = env, stack = ApplicationRightHole val :: stack, currentComputation = Computation computation1 }

        ApplicationRightHole value0 ->
            -- This is applying a closure to a value case (`value0` is the closure, `val` is the argument)
            case value0 of
                ClosureValue frozenEnv { var, body } ->
                    -- Evaluate the body of the closure in the closure's environment extended with the argument bound to the closure's parameter.
                    -- Also don't forget to push environment cleanup after the closure is evaluated.
                    Ok { env = frozenEnv |> insertEnv var val, stack = RestoreEnv env :: stack, currentComputation = Computation body }

                _ ->
                    Err ExpectedClosure

        -- Stack/Current Continuation
        RestoreStackWithLeftHole computation1 ->
            Ok { env = env, stack = RestoreStackWithRightHole val :: stack, currentComputation = Computation computation1 }

        RestoreStackWithRightHole value0 ->
            -- This is restoring the stack `value0` with current computation := val
            case value0 of
                StackValue frozenEnv frozenStack ->
                    Ok { env = frozenEnv, stack = frozenStack, currentComputation = Value val }

                _ ->
                    Err ExpectedClosure

        -- Environment restoration after closure application/stack save is done
        RestoreEnv envToBeRestored ->
            Ok { env = envToBeRestored, stack = stack, currentComputation = Value val }


boolToConstant : Bool -> Constant
boolToConstant b =
    if b then
        TrueConst

    else
        FalseConst


applyPrimitiveOp2 : PrimitiveIntOperation2 -> Int -> Int -> Value
applyPrimitiveOp2 op x y =
    ConstantValue
        (case op of
            Add ->
                IntConst (x + y)

            Mul ->
                IntConst (x * y)

            LessThan ->
                boolToConstant (x < y)
        )


decompose : Env -> Stack -> Computation -> Result RunTimeError State
decompose env stack comp =
    -- The only interesting cases are `VarUse`, `Lambda`, and `SaveStack`
    case comp of
        ConstantComputation constant ->
            Ok { env = env, stack = stack, currentComputation = Value (ConstantValue constant) }

        -- Variable use
        VarUse varName ->
            case env |> getEnv varName of
                Just val ->
                    Ok { env = env, stack = stack, currentComputation = Value val }

                Nothing ->
                    Err (UnboundVarName varName)

        -- Primitive arithmetic
        PrimitiveIntOperation2 op computation0 computation1 ->
            Ok { env = env, stack = PrimitiveIntOperation2LeftHole op computation1 :: stack, currentComputation = Computation computation0 }

        -- Bool
        IfThenElse computation leftBranch rightBranch ->
            Ok { env = env, stack = IfThenElseHole leftBranch rightBranch :: stack, currentComputation = Computation computation }

        -- Function type introduction
        Lambda { var, body } ->
            -- The current environment will get captured in the closure
            Ok { env = env, stack = stack, currentComputation = Value (ClosureValue env { var = var, body = body }) }

        -- Function type elimination
        Application computation0 computation1 ->
            Ok { env = env, stack = ApplicationLeftHole computation1 :: stack, currentComputation = Computation computation0 }

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            -- Environment is extended with the current stack (continuation)
            -- Also if the computation ever finishes (without a jump via the continuation), we need to remember to delete the continuation binding
            Ok { env = env |> insertEnv var (StackValue env stack), stack = RestoreEnv env :: stack, currentComputation = Computation body }

        RestoreStackWith computation0 computation1 ->
            Ok { env = env, stack = RestoreStackWithLeftHole computation1 :: stack, currentComputation = Computation computation0 }



-- ===EXAMPLES===


c : Int -> Computation
c x =
    ConstantComputation (IntConst x)


cv : Int -> Value
cv x =
    ConstantValue (IntConst x)


true : Computation
true =
    ConstantComputation TrueConst


false : Computation
false =
    ConstantComputation FalseConst


add : Computation -> Computation -> Computation
add c0 c1 =
    PrimitiveIntOperation2 Add c0 c1


mul : Computation -> Computation -> Computation
mul c0 c1 =
    PrimitiveIntOperation2 Mul c0 c1


lt : Computation -> Computation -> Computation
lt c0 c1 =
    PrimitiveIntOperation2 LessThan c0 c1


type alias Example =
    { name : String, env : Env, comp : Computation }


example : String -> Computation -> Example
example name comp =
    { name = name, env = Dict.empty, comp = comp }


withBinding : VarName -> Value -> Example -> Example
withBinding varName val ex =
    { ex
        | env = ex.env |> insertEnv varName val
    }


example0 : Example
example0 =
    -- 3 + 2
    example "arithmetic-0"
        (add (c 3) (c 2))


example1 : Example
example1 =
    -- (3 + 2) * (6 + 7)
    example "arithmetic-1"
        (mul
            (add (c 3) (c 2))
            (add (c 7) (c 7))
        )


example2 : Example
example2 =
    -- x + 7
    example "variable"
        (add (VarUse "x") (c 7))
        |> withBinding "x" (cv 5)


example3 : Example
example3 =
    -- if x < 3 then 1 else 5
    example "if-the-else"
        (IfThenElse
            (lt (VarUse "x") (c 3))
            { body = c 7 }
            { body = c 5 }
        )
        |> withBinding "x" (cv 2)


example4 : Example
example4 =
    -- [(fn x -> x + 1) 5]
    example "function application"
        (Application
            (Lambda
                { var = "x"
                , body = add (VarUse "x") (c 1)
                }
            )
            (c 5)
        )


example5 : Example
example5 =
    -- 1 + (saveStack k -> if x < 3 then 5 else restoreStack k 6)
    example "call/cc-0"
        (add
            (c 1)
            (SaveStack
                { var = "k"
                , body =
                    add
                        (IfThenElse
                            (lt (VarUse "x") (c 3))
                            { body = c 5 }
                            { body = RestoreStackWith (VarUse "k") (c 6) }
                        )
                        (c 17)
                }
            )
        )
        |> withBinding "x" (cv 3)


example6 : Example
example6 =
    example "runtime error"
        (add (c 1) true)


defaultExample : Example
defaultExample =
    example0


examples : List Example
examples =
    [ example0, example1, example2, example3, example4, example5, example6 ]



-- ===MODEL===


type alias Model =
    { currentExample : Example
    , currentState : Result RunTimeError { isTerminated : Bool, state : State }

    -- going backwards
    , savedStates : List State

    -- in a valid model `numOfCurrentlySaved == List.length previousStates` invariant is maintained
    , numOfCurrentlySaved : Int
    , numOfMaxSaved : Int
    }


modelFromExample : Example -> Return Msg Model
modelFromExample ex =
    { currentExample = ex
    , currentState =
        Ok
            { isTerminated = False
            , state =
                { env = ex.env
                , stack = []
                , currentComputation = Computation ex.comp
                }
            }
    , savedStates = []
    , numOfCurrentlySaved = 0
    , numOfMaxSaved = 40
    }
        |> Return.singleton


initModel : Return Msg Model
initModel =
    modelFromExample defaultExample


type Msg
    = StepForward
    | StepBack
    | Reset
    | ExampleSelected Example


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        StepForward ->
            model
                |> modelSaveCurrentState
                |> Return.andThen modelStepForward

        StepBack ->
            model |> modelStepBack

        Reset ->
            model |> modelReset

        ExampleSelected ex ->
            modelFromExample ex


modelReset : Model -> Return Msg Model
modelReset model =
    modelFromExample model.currentExample


modelStepForward : Model -> Return Msg Model
modelStepForward ({ currentState } as model) =
    { model
        | currentState =
            currentState
                |> Result.andThen
                    (\decoratedState ->
                        smallStepEval decoratedState.state
                            |> Result.map
                                (\newStateOrTerminal ->
                                    case newStateOrTerminal of
                                        Left state ->
                                            { isTerminated = False, state = state }

                                        Right () ->
                                            { isTerminated = True, state = decoratedState.state }
                                )
                    )
    }
        |> Return.singleton


modelSaveCurrentState : Model -> Return Msg Model
modelSaveCurrentState ({ currentState, savedStates } as model) =
    case currentState of
        Ok { isTerminated, state } ->
            if isTerminated then
                -- do not save
                model |> Return.singleton

            else
                -- 1. push the current state onto `savedStates`
                -- 2. if you exceeded tha `numOfMaxSaved`, then you need to remove the oldest state
                --    otherwise just increment `numOfCurrentlySaved`
                { model
                    | savedStates =
                        if model.numOfCurrentlySaved >= model.numOfMaxSaved then
                            state :: removeLast model.savedStates

                        else
                            state :: model.savedStates
                    , numOfCurrentlySaved = min (model.numOfCurrentlySaved + 1) model.numOfMaxSaved
                }
                    |> Return.singleton

        Err err ->
            -- do not save
            model |> Return.singleton


modelStepBack : Model -> Return Msg Model
modelStepBack ({ savedStates, numOfCurrentlySaved } as model) =
    case savedStates of
        [] ->
            model |> Return.singleton

        state :: savedStates1 ->
            { model
                | currentState = Ok { isTerminated = False, state = state }
                , savedStates = savedStates1
                , numOfCurrentlySaved = numOfCurrentlySaved - 1 -- If the model is valid, then we should have `numOfCurrentlySaved > 1` in this case, so this is fine
            }
                |> Return.singleton



-- ===VIEW===
--helpers


row attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "row" ] ++ attrs)
        html


alignedRow attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "align-items" "center" ] ++ attrs)
        html


col attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "column" ] ++ attrs)
        html


color =
    { red = Css.rgb 255 0 0
    , black = Css.rgb 0 0 0
    , blue = Css.rgb 0 102 255
    , lightBlue = Css.rgb 179 198 255
    , darkBlue = Css.rgb 3 0 56
    , orange = Css.rgb 227 148 0
    , green = Css.rgb 2 199 87
    , pink = Css.rgb 255 0 102
    , grey = Css.rgb 153 153 153
    }


gapX w =
    H.div [ HA.css [ Css.marginLeft (Css.px w) ] ] []


gapY h =
    H.div [ HA.css [ Css.marginTop (Css.px h) ] ] []


view : Model -> Html Msg
view model =
    col []
        [ H.div [] [ H.text "See ", H.a [ HA.href "https://github.com/omedusyo/stack-machine-with-continuations" ] [ H.text "github" ] ]
        , gapY 10
        , H.select [ HA.css [ Css.width (Css.px 200) ] ]
            (examples
                |> List.map
                    (\ex ->
                        H.option [ HE.onClick (ExampleSelected ex) ] [ H.text ex.name ]
                    )
            )
        , gapY 20
        , H.div [] [ H.text ("You can use the left/right arrow keys. I save max " ++ String.fromInt model.numOfMaxSaved ++ " states of the machine.") ]
        , row []
            [ H.button
                [ HE.onClick StepBack
                , HA.disabled (model.numOfCurrentlySaved == 0)
                ]
                [ H.text ("step back (" ++ String.fromInt model.numOfCurrentlySaved ++ ") <-") ]
            , H.button [ HE.onClick StepForward ] [ H.text "step forward ->" ]
            , H.button [ HE.onClick Reset ] [ H.text "Reset" ]
            ]
        , gapY 10
        , case model.currentState of
            Ok { isTerminated, state } ->
                viewState state isTerminated

            Err err ->
                viewRunTimeError err
        ]



-- viewing terms


intOperation2ToString : PrimitiveIntOperation2 -> String
intOperation2ToString op =
    case op of
        Add ->
            "+"

        Mul ->
            "*"

        LessThan ->
            "<"


viewKeyword : String -> Html Msg
viewKeyword keyword =
    H.div [ HA.css [ Css.fontWeight Css.bolder ] ] [ H.text keyword ]


viewConstant : Constant -> Html Msg
viewConstant const =
    H.div [ HA.css [ Css.color color.green ] ]
        [ H.text
            (case const of
                IntConst x ->
                    String.fromInt x

                TrueConst ->
                    "True"

                FalseConst ->
                    "False"
            )
        ]


viewValue : Value -> Html Msg
viewValue val =
    case val of
        ConstantValue const ->
            viewConstant const

        ClosureValue _ _ ->
            viewHiddenValue "closure"

        StackValue _ _ ->
            viewHiddenValue "stack"


viewHiddenValue : String -> Html Msg
viewHiddenValue str =
    alignedRow [] [ H.text "#", brackets (H.text str) ]


viewVariable : String -> Html Msg
viewVariable varName =
    H.div [ HA.css [ Css.color color.darkBlue ] ] [ H.text varName ]


viewVariableUse : String -> Html Msg
viewVariableUse varName =
    viewVariable varName


parens : Html Msg -> Html Msg
parens html =
    alignedRow [] [ H.div [ HA.css [ Css.color color.pink ] ] [ H.text "(" ], html, H.div [ HA.css [ Css.color color.pink ] ] [ H.text ")" ] ]


brackets : Html Msg -> Html Msg
brackets html =
    alignedRow [] [ H.div [ HA.css [ Css.color color.orange ] ] [ H.text "[" ], html, H.div [ HA.css [ Css.color color.pink ] ] [ H.text "]" ] ]


viewIntOperation2Application : PrimitiveIntOperation2 -> Html Msg -> Html Msg -> Html Msg
viewIntOperation2Application op arg0_html arg1_html =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ arg0_html
            , gapX w
            , H.text (intOperation2ToString op)
            , gapX w
            , arg1_html
            ]
        )


viewIfThenElse : Html Msg -> Html Msg -> Html Msg -> Html Msg
viewIfThenElse html html1 html2 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "if", gapX w, html, gapX w, viewKeyword "then", gapX w, html1, gapX w, viewKeyword "else", gapX w, html2 ]
        )


viewApplication : Html Msg -> Html Msg -> Html Msg
viewApplication html0 html1 =
    let
        w =
            5
    in
    brackets (alignedRow [] [ html0, gapX w, html1 ])


viewLambda : String -> Html Msg -> Html Msg
viewLambda var htmlBody =
    let
        w =
            5
    in
    parens (alignedRow [] [ viewKeyword "fn", gapX w, viewVariable var, gapX w, viewKeyword "->", gapX w, htmlBody ])


viewRestoreStack : Html Msg -> Html Msg -> Html Msg
viewRestoreStack html0 html1 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "restoreStack"
            , gapX w
            , html0
            , gapX w
            , html1
            ]
        )


viewComputation : Computation -> Html Msg
viewComputation comp =
    case comp of
        ConstantComputation const ->
            viewConstant const

        -- Variable use
        VarUse varName ->
            viewVariableUse varName

        -- Primitive arithmetic
        PrimitiveIntOperation2 op computation0 computation1 ->
            viewIntOperation2Application op (viewComputation computation0) (viewComputation computation1)

        -- Bool type
        IfThenElse computation leftBranch rightBranch ->
            viewIfThenElse (viewComputation computation) (viewComputation leftBranch.body) (viewComputation rightBranch.body)

        -- Function type
        Lambda { var, body } ->
            viewLambda var (viewComputation body)

        Application computation0 computation1 ->
            viewApplication (viewComputation computation0) (viewComputation computation1)

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            let
                w =
                    5
            in
            parens (alignedRow [] [ viewKeyword "saveStack", gapX w, viewVariable var, gapX w, viewKeyword "->", gapX w, viewComputation body ])

        RestoreStackWith computation0 computation1 ->
            viewRestoreStack (viewComputation computation0) (viewComputation computation1)


viewEnv : Env -> Html Msg
viewEnv env =
    alignedRow []
        (env
            |> Dict.toList
            |> List.map
                (\( varName, values ) ->
                    case values of
                        [] ->
                            H.text "???"

                        val :: _ ->
                            let
                                w =
                                    7
                            in
                            alignedRow [] [ viewVariable varName, gapX w, H.text ":=", gapX w, viewValue val ]
                )
            |> List.intersperse (alignedRow [] [ H.text ",", gapX 15 ])
        )


viewHole : Html Msg
viewHole =
    let
        r =
            5

        d =
            2 * r
    in
    H.div
        [ HA.css
            [ Css.width (Css.px d)
            , Css.height (Css.px d)
            , Css.borderRadius (Css.px r)
            , Css.backgroundColor color.grey
            ]
        ]
        []


viewStackElement : StackElement -> Html Msg
viewStackElement stackEl =
    case stackEl of
        PrimitiveIntOperation2LeftHole op computation1 ->
            -- op(_, M)
            viewIntOperation2Application op viewHole (viewComputation computation1)

        PrimitiveIntOperation2RightHole op value0 ->
            -- op(V, _)
            viewIntOperation2Application op (viewValue value0) viewHole

        -- Bool
        IfThenElseHole leftBranch rightBranch ->
            -- if _ then M else M'
            viewIfThenElse viewHole (viewComputation leftBranch.body) (viewComputation rightBranch.body)

        -- Application
        ApplicationLeftHole computation1 ->
            -- [_ M]
            viewApplication viewHole (viewComputation computation1)

        ApplicationRightHole value0 ->
            -- [Value _]
            viewApplication (viewValue value0) viewHole

        -- Stack/Current Continuation
        RestoreStackWithLeftHole computation1 ->
            -- restoreStack _ M
            viewRestoreStack viewHole (viewComputation computation1)

        RestoreStackWithRightHole value0 ->
            -- restoreStack V _
            viewRestoreStack (viewValue value0) viewHole

        -- Environment restoration after closure application/stack save is done
        RestoreEnv _ ->
            let
                w =
                    5
            in
            parens (alignedRow [] [ viewKeyword "restoreEnv", gapX w, viewHiddenValue "env" ])


viewEmptyStack : Html Msg
viewEmptyStack =
    -- TODO: How to write &epsilon? `H.text` escapes it
    viewKeyword "empty-stack"


viewStack : Stack -> Html Msg
viewStack stack =
    let
        separator =
            H.div [ HA.css [ Css.color color.blue, Css.margin2 (Css.px 0) (Css.px 7) ] ] [ H.text "::" ]
    in
    row []
        (List.concat
            [ ((stack
                    |> List.map
                        (\stackEl ->
                            viewStackElement stackEl
                        )
               )
                ++ [ viewEmptyStack ]
              )
                |> List.intersperse separator
            ]
        )


viewTypeOfCurrentComputation : String -> Html Msg
viewTypeOfCurrentComputation keyword =
    H.div [ HA.css [ Css.fontWeight Css.bold, Css.color color.blue ] ] [ H.text keyword ]


viewState : State -> Bool -> Html Msg
viewState { env, stack, currentComputation } isTerminated =
    let
        w =
            Css.px 120
    in
    col []
        [ row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Focused on" ]
            , case currentComputation of
                Computation comp ->
                    row [] [ viewComputation comp, gapX 20, viewTypeOfCurrentComputation "Computation" ]

                Value val ->
                    row []
                        [ viewValue val
                        , gapX 20
                        , viewTypeOfCurrentComputation
                            (if isTerminated then
                                "Value (terminated)"

                             else
                                "Value"
                            )
                        ]
            ]
        , row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Env" ]
            , viewEnv env
            ]
        , row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Stack" ]
            , viewStack stack
            ]
        ]


viewRunTimeError : RunTimeError -> Html Msg
viewRunTimeError err =
    H.div [ HA.css [ Css.color color.red ] ]
        [ H.text
            (case err of
                ExpectedBoolean ->
                    "Expected Boolean value"

                ExpectedInteger ->
                    "Expected Integer value"

                ExpectedClosure ->
                    "Expected Closure"

                ExpectedStack ->
                    "Expected Stack"

                UnboundVarName varName ->
                    "Unbound variable name"
            )
        ]



-- ===MAIN===


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initModel
        , update = update
        , subscriptions = subscriptions
        , view = \model -> view model |> H.toUnstyled
        }



-- ===SUBSCRIPTIONS===


subscriptions : Model -> Sub Msg
subscriptions model =
    BE.onKeyUp
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\keyCode ->
                    if keyCode == "ArrowRight" then
                        Decode.succeed StepForward

                    else if keyCode == "ArrowLeft" then
                        Decode.succeed StepBack

                    else
                        Decode.fail ""
                )
        )
