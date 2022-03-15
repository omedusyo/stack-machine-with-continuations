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
import Queue exposing (Queue)
import Return exposing (Return)



-- ===EXAMPLES===
-- ===MODEL===


type alias Model =
    { currentExample : Example
    , currentState : State

    -- going backwards
    , savedStates : List State

    -- in a valid model `numOfCurrentlySaved == List.length previousActors` invariant is maintained
    , numOfCurrentlySaved : Int
    , numOfMaxSaved : Int
    }


modelFromExample : Example -> Return Msg Model
modelFromExample ex =
    { currentExample = ex
    , currentState =
        { actors =
            ex.actors
                |> Dict.map
                    (\address exActor ->
                        ActiveActor
                            { env = exActor.env
                            , stack = emptyStack
                            , currentComputation = Computation exActor.comp
                            , console = []
                            , mailbox = exActor.mailbox
                            }
                    )
        , currentlySelectedActor = 0
        , nextAddress = ex.nextAddress
        , messagesInTransit = Dict.fromList []
        , nextMessageId = 0
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
    = StepForward ActorId
    | StepBack
    | ResetState
    | ExampleSelected Example
    | MessageInTransitClicked MessageInTransit


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        StepForward actorId ->
            if isActorActive actorId model.currentState then
                model
                    |> modelSaveCurrentState
                    |> Return.andThen (modelStepForward actorId)

            else
                Return.singleton model

        StepBack ->
            model |> modelStepBack

        ResetState ->
            model |> modelReset

        ExampleSelected ex ->
            modelFromExample ex

        MessageInTransitClicked message ->
            Return.singleton { model | currentState = model.currentState |> deliverMessage message }


modelReset : Model -> Return Msg Model
modelReset model =
    modelFromExample model.currentExample


modelStepForward : ActorId -> Model -> Return Msg Model
modelStepForward address ({ currentState } as model) =
    { model
        | currentState = currentState |> selectActor address |> stepState
    }
        |> Return.singleton


modelSaveCurrentState : Model -> Return Msg Model
modelSaveCurrentState ({ currentState, savedStates } as model) =
    -- 1. push the current state onto `savedStates`
    -- 2. if you exceeded tha `numOfMaxSaved`, then you need to remove the oldest state
    --    otherwise just increment `numOfCurrentlySaved`
    { model
        | savedStates =
            if model.numOfCurrentlySaved >= model.numOfMaxSaved then
                currentState :: removeLast model.savedStates

            else
                currentState :: model.savedStates
        , numOfCurrentlySaved = min (model.numOfCurrentlySaved + 1) model.numOfMaxSaved
    }
        |> Return.singleton


modelStepBack : Model -> Return Msg Model
modelStepBack ({ savedStates, numOfCurrentlySaved } as model) =
    case savedStates of
        [] ->
            model |> Return.singleton

        state :: savedStates1 ->
            { model
                | currentState = state
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
        , viewState model
        ]


viewState : Model -> Html Msg
viewState model =
    let
        w =
            Css.px 120
    in
    col []
        [ row []
            [ H.button
                [ HE.onClick StepBack
                , HA.disabled (model.numOfCurrentlySaved == 0)
                ]
                [ H.text ("step back (" ++ String.fromInt model.numOfCurrentlySaved ++ ") <-") ]
            , H.button [ HE.onClick ResetState ] [ H.text "Reset" ]
            ]
        , col []
            (model.currentState.actors
                |> Dict.toList
                |> List.map
                    (\( address, actor ) ->
                        col [ HA.css [ Css.border3 (Css.px 1) Css.solid color.black ] ]
                            [ H.strong [] [ H.text (String.concat [ "#Actor(", String.fromInt address, ")" ]) ]
                            , row []
                                [ H.button [ HE.onClick (StepForward address), HA.disabled (not (isActive actor)) ] [ H.text "step forward ->" ] ]
                            , gapY 10
                            , viewActor actor
                            ]
                    )
            )
        , row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Messages" ]
            , row [] (model.currentState.messagesInTransit |> Dict.values |> List.map viewMessage)
            ]
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


predicateToString : Predicate -> String
predicateToString pred =
    case pred of
        IsInt ->
            "isInt"

        IsBool ->
            "isBool"

        IsString ->
            "isString"

        IsClosure ->
            "isClosure"

        IsTagged ->
            "isTagged"

        IsTuple ->
            "isTuple"

        IsStack ->
            "isStack"

        IsDelimitedStack ->
            "isDelimitedStack"

        IsEnv ->
            "isEnv"

        IsAddress ->
            "isAddress"


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

                StringConst s ->
                    String.concat [ "\"", s, "\"" ]
            )
        ]


viewValue : Value -> Html Msg
viewValue val =
    case val of
        ConstantValue const ->
            viewConstant const

        ClosureValue _ _ ->
            viewHiddenValue "closure"

        TaggedValue tag _ values ->
            viewTagged tag (values |> List.map viewValue)

        TupleValue _ values ->
            viewTuple (values |> List.map viewValue)

        StackValue _ _ ->
            viewHiddenValue "stack"

        DelimitedStackValue _ _ ->
            viewHiddenValue "delimited-stack"

        EnvValue _ ->
            viewHiddenValue "env"

        Address _ ->
            viewHiddenValue "address"


viewHiddenValue : String -> Html Msg
viewHiddenValue str =
    alignedRow [] [ H.text "#", brackets (H.text str) ]


viewVariable : String -> Html Msg
viewVariable varName =
    H.div [ HA.css [ Css.color color.darkBlue ] ] [ H.text varName ]


viewVariableUse : String -> Html Msg
viewVariableUse varName =
    viewVariable varName


viewTag : Tag -> Html Msg
viewTag tag =
    H.div [ HA.css [ Css.color color.green ] ] [ H.text (":" ++ tag) ]


viewTagged : Tag -> List (Html Msg) -> Html Msg
viewTagged tag htmlValues =
    let
        w =
            10
    in
    case htmlValues of
        [] ->
            viewTag tag

        _ ->
            parens
                (alignedRow []
                    [ viewTag tag
                    , gapX w
                    , alignedRow [] (htmlValues |> List.intersperse (gapX w))
                    ]
                )


viewMatchTagged : Html Msg -> List { pattern : Html Msg, body : Html Msg } -> Html Msg
viewMatchTagged html0 branchesHtml =
    let
        w =
            5

        viewBranch : Html Msg -> Html Msg -> Html Msg
        viewBranch patternHtml bodyHtml =
            alignedRow []
                [ patternHtml
                , gapX w
                , viewKeyword "->"
                , gapX w
                , bodyHtml
                ]
    in
    parens
        (alignedRow []
            (List.concat
                [ [ viewKeyword "case", gapX w, html0, gapX w, viewKeyword "of", gapX w ]
                , branchesHtml
                    |> List.map (\{ pattern, body } -> viewBranch pattern body)
                    |> List.intersperse (row [] [ gapX w, H.text "|", gapX w ])
                ]
            )
        )


viewPattern : Pattern -> Html Msg
viewPattern pattern =
    case pattern of
        TagPattern tag n vars ->
            viewTagged tag (vars |> List.map viewVariable)

        AnyPattern ->
            viewVariable "_"


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
    alignedRow [] [ H.div [ HA.css [ Css.color color.orange ] ] [ H.text "[" ], html, H.div [ HA.css [ Css.color color.orange ] ] [ H.text "]" ] ]


braces : Html Msg -> Html Msg
braces html =
    alignedRow [] [ H.div [ HA.css [ Css.color color.pink ] ] [ H.text "{" ], html, H.div [ HA.css [ Css.color color.pink ] ] [ H.text "}" ] ]


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


viewPredicateApplication : Predicate -> Html Msg -> Html Msg
viewPredicateApplication pred html0 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ H.text (predicateToString pred)
            , gapX w
            , html0
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


viewRestoreDelimitedStack : Html Msg -> Html Msg -> Html Msg
viewRestoreDelimitedStack html0 html1 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "restoreDelimStack"
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


viewLog : Html Msg -> Html Msg -> Html Msg
viewLog html0 html1 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ viewKeyword "log"
            , gapX w
            , html0
            , gapX w
            , viewKeyword "; "
            , gapX w
            , html1
            ]
        )


viewSend : Html Msg -> Html Msg -> Html Msg -> Html Msg
viewSend html0 html1 html2 =
    let
        w =
            5
    in
    parens
        (alignedRow []
            [ html0
            , gapX w
            , viewKeyword "<-"
            , gapX w
            , html1
            , viewKeyword "; "
            , gapX w
            , html2
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

        PredicateApplication pred computation0 ->
            viewPredicateApplication pred (viewComputation computation0)

        -- Bool type
        IfThenElse computation leftBranch rightBranch ->
            viewIfThenElse (viewComputation computation) (viewComputation leftBranch.body) (viewComputation rightBranch.body)

        -- Function type
        Lambda { var, body } ->
            viewLambda var (viewComputation body)

        Application computation0 computation1 ->
            viewApplication (viewComputation computation0) (viewComputation computation1)

        -- Tagged Computations
        Tagged tag n computations ->
            viewTagged tag (computations |> List.map viewComputation)

        MatchTagged computation branches ->
            viewMatchTagged (viewComputation computation)
                (branches
                    |> List.map
                        (\{ pattern, body } ->
                            { pattern = viewPattern pattern, body = viewComputation body }
                        )
                )

        -- Tuple Type
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

        Reset { body } ->
            alignedRow []
                [ viewKeyword "reset", gapX 5, braces (viewComputation body) ]

        Shift { var, body } ->
            let
                w =
                    5
            in
            parens (alignedRow [] [ viewKeyword "shift", gapX w, viewVariable var, gapX w, viewKeyword "->", gapX w, viewComputation body ])

        RestoreDelimitedStackWith computation0 computation1 ->
            viewRestoreDelimitedStack (viewComputation computation0) (viewComputation computation1)

        Let computation { var, body } ->
            viewLet (viewComputation computation) var (viewComputation body)

        GetEnv ->
            viewKeyword "get-env"

        WithIn computation0 computation1 ->
            viewWithIn (viewComputation computation0) (viewComputation computation1)

        Log computation0 computation1 ->
            viewLog (viewComputation computation0) (viewComputation computation1)

        Receive ->
            viewKeyword "receive"

        Send addressComputation messageComputation computation1 ->
            viewSend (viewComputation addressComputation) (viewComputation messageComputation) (viewComputation computation1)


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

        PredicateApplicationHole pred ->
            viewPredicateApplication pred viewHole

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

        -- TaggedComputation
        TaggedWithHole tag n reversedValues computations ->
            viewTagged tag
                ((List.reverse reversedValues |> List.map viewValue)
                    ++ viewHole
                    :: (computations
                            |> List.map viewComputation
                       )
                )

        MatchTaggedWithHole branches ->
            viewMatchTagged viewHole
                (branches
                    |> List.map
                        (\{ pattern, body } ->
                            { pattern = viewPattern pattern, body = viewComputation body }
                        )
                )

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

        -- Delimited Continuations
        RestoreDelimitedStackWithLeftHole computation1 ->
            -- restoreStack _ M
            viewRestoreDelimitedStack viewHole (viewComputation computation1)

        RestoreDelimitedStackWithRightHole value0 ->
            -- restoreStack V _
            viewRestoreDelimitedStack (viewValue value0) viewHole

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

        -- Console
        LogLeftHole computation1 ->
            viewLog viewHole (viewComputation computation1)

        SendLeftHole messageComputation computation1 ->
            viewSend viewHole (viewComputation messageComputation) (viewComputation computation1)

        SendRightHole address computation1 ->
            viewSend (viewHiddenValue "address") viewHole (viewComputation computation1)


viewEmptyStack : Html Msg
viewEmptyStack =
    -- TODO: How to write &epsilon? `H.text` escapes it
    viewKeyword "empty-stack"


viewDelimitedStack : DelimitedStack -> Html Msg
viewDelimitedStack delimitedStack =
    let
        separator =
            H.div [ HA.css [ Css.color color.blue, Css.margin2 (Css.px 0) (Css.px 7) ] ] [ H.text "::" ]
    in
    row []
        (List.concat
            [ ((delimitedStack
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


viewStack : Stack -> Html Msg
viewStack stack =
    let
        separator =
            H.div [ HA.css [ Css.color color.blue, Css.margin2 (Css.px 0) (Css.px 7), Css.fontWeight Css.bold ] ] [ H.text "||" ]
    in
    alignedRow []
        (viewDelimitedStack stack.currentDelimitedStack
            :: (stack.savedDelimitedStacks
                    |> List.andThen (\delimitedStack -> [ separator, viewDelimitedStack delimitedStack ])
               )
        )


viewConsole : Console -> Html Msg
viewConsole console =
    col []
        (console
            |> List.reverse
            |> List.map
                (\val ->
                    viewValue val
                )
        )


viewMailbox : Mailbox -> Html Msg
viewMailbox q =
    alignedRow []
        (q
            |> Queue.toList
            |> List.map viewValue
            |> List.intersperse (row [] [ H.text ",", gapX 15 ])
        )


viewTypeOfCurrentComputation : String -> Html Msg
viewTypeOfCurrentComputation keyword =
    H.div [ HA.css [ Css.fontWeight Css.bold, Css.color color.blue ] ] [ H.text keyword ]


viewActorState : ActorState -> Html Msg
viewActorState { env, stack, currentComputation, console, mailbox } =
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
                        , viewTypeOfCurrentComputation "Value"
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
        , row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Console" ]
            , viewConsole console
            ]
        , row []
            [ H.div [ HA.css [ Css.width w ] ] [ H.text "Mailbox" ]
            , viewMailbox mailbox
            ]
        ]


viewActor : Actor -> Html Msg
viewActor actor =
    case actor of
        ActiveActor actorState ->
            viewActorState actorState

        BlockedActor actorState ->
            viewActorState actorState

        TerminatedActor { env, terminalValue, console, mailbox } ->
            let
                w =
                    Css.px 120
            in
            col []
                [ row []
                    [ H.div [ HA.css [ Css.width w ] ] [ H.text "Result" ]
                    , viewValue terminalValue
                    , gapX 20
                    , viewTypeOfCurrentComputation "Value (Terminated)"
                    ]
                , row []
                    [ H.div [ HA.css [ Css.width w ] ] [ H.text "Env" ]
                    , viewEnv env
                    ]
                , row []
                    [ H.div [ HA.css [ Css.width w ] ] [ H.text "Console" ]
                    , viewConsole console
                    ]
                , row []
                    [ H.div [ HA.css [ Css.width w ] ] [ H.text "Mailbox" ]
                    , viewMailbox mailbox
                    ]
                ]

        FailedActor err ->
            viewRunTimeError err


viewMessage : MessageInTransit -> Html Msg
viewMessage ({ destination, payload } as message) =
    row
        [ HA.css
            [ Css.border3 (Css.px 1) Css.solid color.blue
            , Css.padding2 (Css.px 5) (Css.px 8)
            , Css.backgroundColor color.lightBlue
            ]
        , HE.onClick (MessageInTransitClicked message)
        ]
        [ viewHiddenValue "address"
        , gapX 20
        , viewValue payload
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

                ExpectedTaggedValue ->
                    "Expected Tagged Value"

                MatchNotFound ->
                    "Match not found"

                ExpectedTuple ->
                    "Expected Tuple"

                ExpectedStack ->
                    "Expected Stack"

                ExpectedDelimitedStack ->
                    "Expected Delimited Stack"

                ExpectedEnv ->
                    "Expected Environment"

                ExpectedAddress ->
                    "Expected Address"

                TaggedComputationArityMismatch { expected, got } ->
                    String.concat
                        [ "Ill-formed tagged value: The tuple is supposed to be of size "
                        , String.fromInt expected
                        , " but in it we have "
                        , String.fromInt got
                        , " elements"
                        ]

                TupleArityMismatch { expected, got } ->
                    String.concat
                        [ "Ill-formed tuple: The tuple is supposed to be of size "
                        , String.fromInt expected
                        , " but in it we have "
                        , String.fromInt got
                        , " elements"
                        ]

                CantProject { tupleSize, index } ->
                    String.concat
                        [ "Out of bounds of a tuple: The tupple is supposed to be of size "
                        , String.fromInt tupleSize
                        , " but you're trying to get its "
                        , String.fromInt index
                        , "th element (0-based indexing)"
                        ]

                ShiftingWithoutReset ->
                    "Shifting without reset"

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
                        Decode.succeed (StepForward model.currentState.currentlySelectedActor)

                    else if keyCode == "ArrowLeft" then
                        Decode.succeed StepBack

                    else
                        Decode.fail ""
                )
        )
