module Main exposing (..)

import Browser
import Browser.Events as BE
import Css
import Dict exposing (Dict)
import Example exposing (Example, defaultExample, examples)
import ExplicitEvaluator exposing (..)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode as Decode
import List.Extra as List
import Return exposing (Return)



-- ===EXAMPLES===
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

        TupleValue _ values ->
            viewTuple (values |> List.map viewValue)

        StackValue _ _ ->
            viewHiddenValue "stack"

        EnvValue _ ->
            viewHiddenValue "env"


viewHiddenValue : String -> Html Msg
viewHiddenValue str =
    alignedRow [] [ H.text "#", brackets (H.text str) ]


viewVariable : String -> Html Msg
viewVariable varName =
    H.div [ HA.css [ Css.color color.darkBlue ] ] [ H.text varName ]


viewVariableUse : String -> Html Msg
viewVariableUse varName =
    viewVariable varName


viewTuple : List (Html Msg) -> Html Msg
viewTuple htmlValues =
    let
        w =
            10
    in
    parens
        (alignedRow [] (htmlValues |> List.intersperse (row [] [ H.text ",", gapX w ])))


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


viewProject : Html Msg -> Int -> Html Msg
viewProject html0 k =
    alignedRow [] [ html0, viewKeyword ".", H.text (String.fromInt k) ]


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


viewLet : Html Msg -> VarName -> Html Msg -> Html Msg
viewLet html0 var bodyHtml =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "let"
            , gapX w
            , viewVariable var
            , gapX w
            , viewKeyword "="
            , gapX w
            , html0
            , gapX w
            , viewKeyword "in"
            , gapX w
            , bodyHtml
            ]
        )


viewWithIn : Html Msg -> Html Msg -> Html Msg
viewWithIn html0 html1 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "with"
            , gapX w
            , html0
            , gapX w
            , viewKeyword "in"
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

        Tuple _ computations ->
            viewTuple (computations |> List.map viewComputation)

        Project computation k ->
            viewProject (viewComputation computation) k

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            let
                w =
                    5
            in
            parens (alignedRow [] [ viewKeyword "saveStack", gapX w, viewVariable var, gapX w, viewKeyword "->", gapX w, viewComputation body ])

        RestoreStackWith computation0 computation1 ->
            viewRestoreStack (viewComputation computation0) (viewComputation computation1)

        Let computation { var, body } ->
            viewLet (viewComputation computation) var (viewComputation body)

        GetEnv ->
            viewKeyword "get-env"

        WithIn computation0 computation1 ->
            viewWithIn (viewComputation computation0) (viewComputation computation1)


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

        -- Tuple
        TupleWithHole _ reversedValues computations ->
            viewTuple
                ((List.reverse reversedValues |> List.map viewValue)
                    ++ viewHole
                    :: (computations
                            |> List.map viewComputation
                       )
                )

        ProjectWithHole k ->
            viewProject viewHole k

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

        -- Let binding
        LetWithLeftHole { var, body } ->
            viewLet viewHole var (viewComputation body)

        WithInLeftHole computation1 ->
            viewWithIn viewHole (viewComputation computation1)


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

                ExpectedTuple ->
                    "Expected Tuple"

                ExpectedStack ->
                    "Expected Stack"

                ExpectedEnv ->
                    "Expected Environment"

                TupleArityMismatch { expected, got } ->
                    String.concat
                        [ "Ill-formed tuple: The tuple is supposed to be of size "
                        , String.fromInt expected
                        , " but in it we have "
                        , String.fromInt got
                        , " elements"
                        ]

                CantProject { tupleSize, index } ->
                    Debug.todo ""

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
