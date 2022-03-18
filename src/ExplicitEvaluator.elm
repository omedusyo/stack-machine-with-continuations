module ExplicitEvaluator exposing (..)

import Dict exposing (..)
import List.Extra as List
import Queue exposing (Queue)



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


find : (a -> Maybe b) -> List a -> Maybe b
find f xs0 =
    case xs0 of
        [] ->
            Nothing

        x :: xs1 ->
            case f x of
                Just y ->
                    Just y

                Nothing ->
                    find f xs1



-- ===Stack Machine===


type alias VarName =
    String


type alias Env =
    -- I use `List Value` instead of `Value` because I worried about shadowing.
    -- But since I don't actually remove any variable bindings explicitely but just restore to a previous environment, the `List` doesn't seem to be necessary.
    Dict String (List Value)


emptyEnv : Env
emptyEnv =
    Dict.empty


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


insertsEnv : List ( VarName, Value ) -> Env -> Env
insertsEnv bindings env =
    List.foldl (\( varName, val ) envState -> insertEnv varName val envState)
        env
        bindings


getEnv : VarName -> Env -> Maybe Value
getEnv varName env =
    env |> Dict.get varName |> Maybe.andThen List.head


type Constant
    = -- Primitive Integers
      IntConst Int
      -- Bool
    | TrueConst
    | FalseConst
      -- String
    | StringConst String


type alias Closure =
    { env : Env
    , var : VarName
    , body : Computation
    }


type Value
    = ConstantValue Constant
      -- Closure
    | ClosureValue Closure
      -- Tagged Value
    | TaggedValue Tag Int (List Value)
      -- Tuple
    | TupleValue Int (List Value) -- invariant: for `TupleValue n values` we require `n == List.length values`
      -- Stack/Continuation
    | StackValue Env Stack
    | DelimitedStackValue Env DelimitedStack
      -- First class environments
    | EnvValue Env
      -- Address
    | Address ActorId


type PrimitiveIntOperation2
    = Add
    | Mul
    | LessThan


type Predicate
    = IsInt
    | IsBool
    | IsString
    | IsClosure
    | IsTagged
    | IsTuple
    | IsStack
    | IsDelimitedStack
    | IsEnv
    | IsAddress


type alias Tag =
    String


type Pattern
    = TagPattern Tag Int (List VarName)
    | AnyPattern


type Computation
    = ConstantComputation Constant
      -- Variable use
    | VarUse VarName
      -- Primitive arithmetic
    | PrimitiveIntOperation2 PrimitiveIntOperation2 Computation Computation
      -- Primitive predicates
    | PredicateApplication Predicate Computation
      -- Bool type
    | IfThenElse Computation { body : Computation } { body : Computation }
      -- Function type
    | Lambda { var : VarName, body : Computation } -- fn x -> body
    | Application Computation Computation
      -- Tagged Computations
    | Tagged Tag Int (List Computation)
    | MatchTagged Computation (List { pattern : Pattern, body : Computation })
      -- Tuple Type
      -- (M0, M1, M2)   <-->   Tuple 3 [M0, M1, M2]
    | Tuple Int (List Computation) -- invariant: for `Tuple n computations` we require `n == List.length compuations`
      -- (M0, M1, M2).1 ~> M1
    | Project Computation Int
      -- Stack/Current Continuation
    | SaveStack { var : VarName, body : Computation }
    | RestoreStackWith Computation Computation -- `restore-stack-with k` comp This is like function application, but we're "applying" a frozen stack `restoreStack k v`
      -- Delimited Continuations
    | Reset { body : Computation }
    | Shift { var : VarName, body : Computation }
    | RestoreDelimitedStackWith Computation Computation
      -- First class environments
    | Let Computation { var : VarName, body : Computation } -- let x = e; body
      -- What's the point of having
      --   save-env e; body
      -- if you have the let-binding?
      --   let e = get-env; body
    | GetEnv
    | WithIn Computation Computation -- with env in body
      -- Console
    | Log Computation Computation
      -- Actor Model
    | Receive
    | Send Computation Computation Computation -- send(address, msg); comp  ~>  Send address msg comp
    | Spawn Computation -- spawn { comp }
    | Self
      -- Loop
    | Halt Computation


type CurrentComputation
    = Value Value
    | Computation Computation


type StackElement
    = PrimitiveIntOperation2LeftHole PrimitiveIntOperation2 Computation -- op(_, M)
    | PrimitiveIntOperation2RightHole PrimitiveIntOperation2 Value -- op(V, _)
    | PredicateApplicationHole Predicate -- predicate _
      -- Bool
    | IfThenElseHole { body : Computation } { body : Computation } -- if _ then M else M'
      -- Application
    | ApplicationLeftHole Computation -- [_ M]
    | ApplicationRightHole Value -- [Value _]
      -- TaggedComputation
    | TaggedWithHole Tag Int (List Value) (List Computation)
    | MatchTaggedWithHole (List { pattern : Pattern, body : Computation })
      -- Tuple
    | TupleWithHole Int (List Value) (List Computation) -- invariant: for `TupleWithHole n reversedValues computations` we require `n == 1 + List.length reversedValues + List.length computations
    | ProjectWithHole Int
      -- Stack/Current Continuation
    | RestoreStackWithLeftHole Computation -- restoreStack _ M
    | RestoreStackWithRightHole Value -- restoreStack V _
      -- Delimited Continuations
    | RestoreDelimitedStackWithLeftHole Computation
    | RestoreDelimitedStackWithRightHole Value
      -- Environment restoration after closure application/stack save is done
    | RestoreEnv Env
      -- First class environments
    | LetWithLeftHole { var : VarName, body : Computation } -- let x = _ in body
    | WithInLeftHole Computation -- with _ in M
      -- Console
    | LogLeftHole Computation -- log _ M
      -- Actor Model
    | SendLeftHole Computation Computation -- send(_, msg); comp
    | SendRightHole ActorId Computation -- send(address, _); comp
      -- Loops
    | HaltHole


type alias DelimitedStack =
    List StackElement


type alias Stack =
    { currentDelimitedStack : DelimitedStack, savedDelimitedStacks : List DelimitedStack }


emptyStack : Stack
emptyStack =
    { currentDelimitedStack = [], savedDelimitedStacks = [] }


pushStack : StackElement -> Stack -> Stack
pushStack stackEl ({ currentDelimitedStack } as stack) =
    { stack | currentDelimitedStack = stackEl :: currentDelimitedStack }


popStack : Stack -> Maybe ( StackElement, Stack )
popStack ({ currentDelimitedStack, savedDelimitedStacks } as stack) =
    case currentDelimitedStack of
        [] ->
            case savedDelimitedStacks of
                [] ->
                    Nothing

                delimitedStack :: savedDelimitedStacks1 ->
                    case delimitedStack of
                        [] ->
                            -- Can I assume this branch will never occur?
                            -- So that I don't need to pop recursively until I find a non-empty saved delimited stack?
                            Nothing

                        el :: delimitedStack1 ->
                            Just ( el, { stack | currentDelimitedStack = delimitedStack1, savedDelimitedStacks = savedDelimitedStacks1 } )

        el :: currentDelimitedStack1 ->
            Just ( el, { stack | currentDelimitedStack = currentDelimitedStack1 } )


delimitStack : Stack -> Stack
delimitStack ({ currentDelimitedStack, savedDelimitedStacks } as stack) =
    { currentDelimitedStack = [], savedDelimitedStacks = currentDelimitedStack :: savedDelimitedStacks }


resetToDelimitedStack : DelimitedStack -> Stack -> Stack
resetToDelimitedStack delimitedStack ({ currentDelimitedStack, savedDelimitedStacks } as stack) =
    { currentDelimitedStack = delimitedStack, savedDelimitedStacks = currentDelimitedStack :: savedDelimitedStacks }


shiftStack : Stack -> Maybe ( DelimitedStack, Stack )
shiftStack ({ currentDelimitedStack, savedDelimitedStacks } as stack) =
    case savedDelimitedStacks of
        [] ->
            Nothing

        delimitedStack :: savedDelimitedStacks1 ->
            Just ( currentDelimitedStack, { stack | currentDelimitedStack = delimitedStack, savedDelimitedStacks = savedDelimitedStacks1 } )


type alias Console =
    List Value


emptyConsole : Console
emptyConsole =
    []


type alias ActorState =
    { env : Env
    , stack : Stack
    , currentComputation : CurrentComputation
    , console : Console
    , mailbox : Mailbox
    }


type alias TerminatedActorState =
    { env : Env
    , terminalValue : Value
    , console : Console
    , mailbox : Mailbox
    }


type Actor
    = ActiveActor ActorState
    | BlockedActor ActorState
    | TerminatedActor TerminatedActorState
    | FailedActor RunTimeError


mapActor : (ActorState -> ActorState) -> Actor -> Actor
mapActor f actor =
    case actor of
        ActiveActor actorState ->
            ActiveActor (f actorState)

        BlockedActor actorState ->
            BlockedActor (f actorState)

        TerminatedActor val ->
            TerminatedActor val

        FailedActor err ->
            FailedActor err


isActive : Actor -> Bool
isActive actor =
    case actor of
        ActiveActor _ ->
            True

        _ ->
            False


type alias Mailbox =
    Queue Value


emptyMailbox : Mailbox
emptyMailbox =
    Queue.empty


type alias ActorId =
    Int


type alias MessageId =
    Int


type alias MessageInTransit =
    { messageId : MessageId
    , destination : ActorId
    , payload : Value
    }


type alias State =
    { actors : Dict ActorId Actor
    , currentlySelectedActor : ActorId
    , messagesInTransit : Dict MessageId MessageInTransit
    , nextAddress : ActorId
    , nextMessageId : MessageId
    }


isCurrentlySelectedActorActive : State -> Bool
isCurrentlySelectedActorActive { actors, currentlySelectedActor } =
    case Dict.get currentlySelectedActor actors of
        Just actor ->
            isActive actor

        Nothing ->
            False


isActorActive : ActorId -> State -> Bool
isActorActive address { actors, currentlySelectedActor } =
    case Dict.get address actors of
        Just actor ->
            isActive actor

        Nothing ->
            False


updateActor : ActorId -> Actor -> State -> State
updateActor address actor state =
    { state
        | actors = state.actors |> Dict.insert address actor
    }


selectActor : ActorId -> State -> State
selectActor address state =
    { state | currentlySelectedActor = address }


do : CurrentComputation -> ActorState -> ActorState
do currentComputation actorState =
    { actorState | currentComputation = currentComputation }


push : StackElement -> ActorState -> ActorState
push stackEl actorState =
    { actorState | stack = pushStack stackEl actorState.stack }


reset : ActorState -> ActorState
reset actorState =
    { actorState | stack = delimitStack actorState.stack }


resetTo : DelimitedStack -> ActorState -> ActorState
resetTo delimitedStack actorState =
    { actorState | stack = resetToDelimitedStack delimitedStack actorState.stack }


shift : ActorState -> ( Maybe DelimitedStack, Actor )
shift ({ stack } as actorState) =
    case shiftStack stack of
        Just ( delimitedStack, stack1 ) ->
            ( Just delimitedStack, ActiveActor { actorState | stack = stack1 } )

        Nothing ->
            ( Nothing, FailedActor ShiftingWithoutReset )


setStack : Stack -> ActorState -> ActorState
setStack stack actorState =
    { actorState | stack = stack }


getStack : (Stack -> ActorState -> ActorState) -> ActorState -> ActorState
getStack f actorState =
    f actorState.stack actorState


setEnvironment : Env -> ActorState -> ActorState
setEnvironment env actorState =
    { actorState | env = env }


getEnvironment : (Env -> ActorState -> ActorState) -> ActorState -> ActorState
getEnvironment f actorState =
    f actorState.env actorState


bind : VarName -> Value -> ActorState -> ActorState
bind varName value actorState =
    { actorState | env = actorState.env |> insertEnv varName value }


log : Value -> ActorState -> ActorState
log value actorState =
    { actorState | console = value :: actorState.console }


isMailboxEmpty : ActorState -> Bool
isMailboxEmpty actorState =
    Queue.isEmpty actorState.mailbox


receive : ActorState -> ( Maybe Value, Actor )
receive actorState =
    case Queue.dequeue actorState.mailbox of
        Just ( val, newQueue ) ->
            ( Just val, ActiveActor { actorState | mailbox = newQueue } )

        Nothing ->
            ( Nothing, BlockedActor actorState )


sendMessage : MessageInTransit -> State -> State
sendMessage message ({ messagesInTransit } as state) =
    { state | messagesInTransit = messagesInTransit |> Dict.insert message.messageId message }
        |> updateNextMessageId


deliverPayloadToActor : Value -> Actor -> Actor
deliverPayloadToActor val actor =
    let
        updateActorState actorState =
            { actorState | mailbox = actorState.mailbox |> Queue.enqueue val }
    in
    case actor of
        ActiveActor actorState ->
            ActiveActor (updateActorState actorState)

        BlockedActor actorState ->
            ActiveActor (updateActorState actorState)

        TerminatedActor terminatedActorState ->
            TerminatedActor terminatedActorState

        FailedActor err ->
            FailedActor err


deliverMessage : MessageInTransit -> State -> State
deliverMessage message state =
    case Dict.get message.destination state.actors of
        Just actor ->
            state
                |> updateActor message.destination (deliverPayloadToActor message.payload actor)
                |> removeMessage message

        Nothing ->
            state
                |> removeMessage message


removeMessage : MessageInTransit -> State -> State
removeMessage { messageId } ({ messagesInTransit } as state) =
    { state | messagesInTransit = messagesInTransit |> Dict.remove messageId }


updateNextMessageId : State -> State
updateNextMessageId state =
    { state | nextMessageId = state.nextMessageId + 1 }


updateNextAddress : State -> State
updateNextAddress state =
    { state | nextAddress = state.nextAddress + 1 }


spawnActor : Env -> Computation -> State -> State
spawnActor env computation ({ nextAddress, actors } as state) =
    { state
        | actors =
            actors
                |> Dict.insert nextAddress
                    (ActiveActor
                        { env = env
                        , stack = emptyStack
                        , currentComputation = Computation computation
                        , console = emptyConsole
                        , mailbox = emptyMailbox
                        }
                    )
    }
        |> updateNextAddress


type RunTimeError
    = ExpectedBoolean
    | ExpectedInteger
    | ExpectedClosure
    | ExpectedTaggedValue
    | MatchNotFound
    | ExpectedTuple
    | ExpectedStack
    | ExpectedDelimitedStack
    | ExpectedEnv
    | ExpectedAddress
    | TaggedComputationArityMismatch { expected : Int, got : Int }
    | TupleArityMismatch { expected : Int, got : Int }
    | CantProject { tupleSize : Int, index : Int }
    | ShiftingWithoutReset
    | UnboundVarName VarName


stepState : State -> State
stepState ({ actors, currentlySelectedActor, nextAddress, nextMessageId } as state) =
    case Dict.get currentlySelectedActor actors of
        Just actor ->
            case actor of
                ActiveActor actorState ->
                    case stepActor nextAddress currentlySelectedActor actorState of
                        Return newActor ->
                            state
                                |> updateActor currentlySelectedActor newActor

                        EmitMessage { destination, payload } newActor ->
                            state
                                |> updateActor currentlySelectedActor newActor
                                |> sendMessage { messageId = state.nextMessageId, destination = destination, payload = payload }

                        SpawnActor env computation newActor ->
                            state
                                |> updateActor currentlySelectedActor newActor
                                |> spawnActor env computation

                _ ->
                    state

        Nothing ->
            state


type StepActorResult
    = Return Actor
    | EmitMessage { destination : ActorId, payload : Value } Actor
    | SpawnActor Env Computation Actor


stepActor : ActorId -> ActorId -> ActorState -> StepActorResult
stepActor nextAddress currentAddress actorState =
    case actorState.currentComputation of
        Value val ->
            case popStack actorState.stack of
                Nothing ->
                    -- Current computation is a value, but the stack is empty, so we terminate
                    Return (TerminatedActor { env = actorState.env, terminalValue = val, console = actorState.console, mailbox = actorState.mailbox })

                Just ( stackEl, stack1 ) ->
                    -- Current computation is a value and we have work to do on the stack.
                    -- Based on what's on the stack we either fail or actually do some non-trivial computing
                    -- In either case the top of the stack is consumed
                    combine val stackEl (actorState |> setStack stack1)

        Computation comp ->
            -- Current computation is not a value, and so it will be decomposed into smaller pieces.
            -- Either additional work will be pushed onto the stack or the current computation will be transformed into a value
            decompose nextAddress actorState.env comp currentAddress actorState


combine : Value -> StackElement -> ActorState -> StepActorResult
combine val stackEl actorState =
    -- The only interesting cases are `PrimitiveIntOperation2RightHole`, `IfThenElseHole`, `ApplicationRightHole`, `RestoreStackWithRightHole`, and `RestoreEnv`
    case stackEl of
        PrimitiveIntOperation2LeftHole op computation1 ->
            -- Ok { env = env, stack = pushStack (PrimitiveIntOperation2RightHole op val) stack, currentComputation = Computation computation1, console = console }
            Return
                (ActiveActor
                    (actorState
                        |> do (Computation computation1)
                        |> push (PrimitiveIntOperation2RightHole op val)
                    )
                )

        PrimitiveIntOperation2RightHole op value0 ->
            Return <|
                case ( value0, val ) of
                    ( ConstantValue (IntConst x), ConstantValue (IntConst y) ) ->
                        ActiveActor (actorState |> do (Value (applyPrimitiveOp2 op x y)))

                    _ ->
                        FailedActor ExpectedInteger

        PredicateApplicationHole pred ->
            Return <|
                ActiveActor (actorState |> do (Value (applyPredicate pred val)))

        -- Bool
        IfThenElseHole leftBranch rightBranch ->
            Return <|
                case val of
                    ConstantValue TrueConst ->
                        ActiveActor (actorState |> do (Computation leftBranch.body))

                    ConstantValue FalseConst ->
                        ActiveActor (actorState |> do (Computation rightBranch.body))

                    _ ->
                        FailedActor ExpectedBoolean

        -- Application
        ApplicationLeftHole computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation1)
                        |> push (ApplicationRightHole val)
                    )

        ApplicationRightHole value0 ->
            Return <|
                -- This is applying a closure to a value case (`value0` is the closure, `val` is the argument)
                case value0 of
                    ClosureValue closure ->
                        ActiveActor
                            (actorState
                                |> application val closure
                            )

                    _ ->
                        FailedActor ExpectedClosure

        -- TaggedComputation
        TaggedWithHole tag n reversedValues computations0 ->
            Return <|
                case computations0 of
                    [] ->
                        ActiveActor (actorState |> do (Value (TaggedValue tag n (List.reverse (val :: reversedValues)))))

                    computation0 :: computations1 ->
                        ActiveActor
                            (actorState
                                |> do (Computation computation0)
                                |> push (TaggedWithHole tag n (val :: reversedValues) computations1)
                            )

        MatchTaggedWithHole branches ->
            Return <|
                case val of
                    TaggedValue tag n values ->
                        case matchPatternWithBranch tag n values branches of
                            Just { bindings, body } ->
                                ActiveActor
                                    (actorState
                                        |> do (Computation body)
                                        |> getEnvironment (\env -> push (RestoreEnv env))
                                        |> getEnvironment (\env -> setEnvironment (env |> insertsEnv bindings))
                                    )

                            Nothing ->
                                FailedActor MatchNotFound

                    _ ->
                        FailedActor ExpectedTaggedValue

        -- Tuples
        TupleWithHole n reversedValues computations0 ->
            Return <|
                case computations0 of
                    [] ->
                        ActiveActor (actorState |> do (Value (TupleValue n (List.reverse (val :: reversedValues)))))

                    computation0 :: computations1 ->
                        ActiveActor
                            (actorState
                                |> do (Computation computation0)
                                |> push (TupleWithHole n (val :: reversedValues) computations1)
                            )

        ProjectWithHole k ->
            Return <|
                case val of
                    TupleValue n values ->
                        case List.getAt k values of
                            Just val0 ->
                                ActiveActor (actorState |> do (Value val0))

                            Nothing ->
                                FailedActor (CantProject { tupleSize = n, index = k })

                    _ ->
                        FailedActor ExpectedTuple

        -- Stack/Current Continuation
        RestoreStackWithLeftHole computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation1)
                        |> push (RestoreStackWithRightHole val)
                    )

        RestoreStackWithRightHole value0 ->
            -- This is restoring the stack `value0` with current computation := val
            Return <|
                case value0 of
                    StackValue frozenEnv frozenStack ->
                        ActiveActor
                            (actorState
                                |> do (Value val)
                                |> setStack frozenStack
                                |> setEnvironment frozenEnv
                            )

                    _ ->
                        FailedActor ExpectedStack

        -- Delimited Continuations
        RestoreDelimitedStackWithLeftHole computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation1)
                        |> push (RestoreDelimitedStackWithRightHole val)
                    )

        RestoreDelimitedStackWithRightHole value0 ->
            Return <|
                case value0 of
                    DelimitedStackValue frozenEnv frozenDelimitedStack ->
                        -- 1. Push the current env to be restored
                        -- 2. Reset the current delimited stack to the frozen delim stack and same for the env
                        ActiveActor
                            (actorState
                                |> do (Value val)
                                |> getEnvironment (\env -> push (RestoreEnv env))
                                |> resetTo frozenDelimitedStack
                                |> setEnvironment frozenEnv
                            )

                    _ ->
                        FailedActor ExpectedDelimitedStack

        -- Environment restoration after closure application/stack save is done
        RestoreEnv envToBeRestored ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Value val)
                        |> setEnvironment envToBeRestored
                    )

        -- First class environents
        -- let-binding
        LetWithLeftHole { var, body } ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation body)
                        |> getEnvironment (\env -> push (RestoreEnv env))
                        |> bind var val
                    )

        WithInLeftHole computation1 ->
            Return <|
                case val of
                    EnvValue env0 ->
                        ActiveActor
                            (actorState
                                |> do (Computation computation1)
                                |> getEnvironment (\env -> push (RestoreEnv env))
                                |> setEnvironment env0
                            )

                    _ ->
                        FailedActor ExpectedEnv

        -- Console
        LogLeftHole computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation1)
                        |> log val
                    )

        -- Actor Model
        SendLeftHole messageComputation computation1 ->
            Return <|
                case val of
                    Address addressValue ->
                        ActiveActor
                            (actorState
                                |> do (Computation messageComputation)
                                |> push (SendRightHole addressValue computation1)
                            )

                    _ ->
                        FailedActor ExpectedAddress

        SendRightHole address computation1 ->
            EmitMessage { destination = address, payload = val } <|
                ActiveActor
                    (actorState
                        |> do (Computation computation1)
                    )

        -- Loops
        HaltHole ->
            Return <|
                TerminatedActor { env = actorState.env, terminalValue = val, console = actorState.console, mailbox = actorState.mailbox }


application : Value -> Closure -> ActorState -> ActorState
application val closure actorState =
    -- Evaluate the body of the closure in the closure's environment extended with the argument bound to the closure's parameter.
    -- Also don't forget to push environment cleanup after the closure is evaluated.
    actorState
        |> do (Computation closure.body)
        |> getEnvironment (\env -> push (RestoreEnv env))
        |> setEnvironment closure.env
        |> bind closure.var val


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


decompose : ActorId -> Env -> Computation -> ActorId -> ActorState -> StepActorResult
decompose nextAddress env comp currentAddress actorState =
    case comp of
        ConstantComputation constant ->
            Return <|
                ActiveActor (actorState |> do (Value (ConstantValue constant)))

        -- Variable use
        VarUse varName ->
            Return <|
                case env |> getEnv varName of
                    Just val ->
                        ActiveActor (actorState |> do (Value val))

                    Nothing ->
                        FailedActor (UnboundVarName varName)

        -- Primitive arithmetic
        PrimitiveIntOperation2 op computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (PrimitiveIntOperation2LeftHole op computation1)
                    )

        PredicateApplication pred computation0 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (PredicateApplicationHole pred)
                    )

        -- Bool
        IfThenElse computation leftBranch rightBranch ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation)
                        |> push (IfThenElseHole leftBranch rightBranch)
                    )

        -- Function type introduction
        Lambda { var, body } ->
            Return <|
                -- The current environment will get captured in the closure
                ActiveActor (actorState |> do (Value (ClosureValue { env = env, var = var, body = body })))

        -- Function type elimination
        Application computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (ApplicationLeftHole computation1)
                    )

        -- Tagged Computations
        Tagged tag n computations0 ->
            Return <|
                let
                    numOfComputations =
                        List.length computations0
                in
                if n /= numOfComputations then
                    FailedActor (TaggedComputationArityMismatch { expected = n, got = numOfComputations })

                else
                    case computations0 of
                        [] ->
                            ActiveActor (actorState |> do (Value (TaggedValue tag 0 [])))

                        computation0 :: computations1 ->
                            ActiveActor
                                (actorState
                                    |> do (Computation computation0)
                                    |> push (TaggedWithHole tag n [] computations1)
                                )

        MatchTagged computation0 branches ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (MatchTaggedWithHole branches)
                    )

        -- Tuple Construction
        Tuple n computations0 ->
            Return <|
                let
                    numOfComputations =
                        List.length computations0
                in
                if n /= numOfComputations then
                    FailedActor (TupleArityMismatch { expected = n, got = numOfComputations })

                else
                    case computations0 of
                        [] ->
                            ActiveActor (actorState |> do (Value (TupleValue 0 [])))

                        computation0 :: computations1 ->
                            ActiveActor
                                (actorState
                                    |> do (Computation computation0)
                                    |> push (TupleWithHole n [] computations1)
                                )

        Project computation0 k ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (ProjectWithHole k)
                    )

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            Return <|
                -- Environment is extended with the current stack (continuation)
                -- Also if the computation ever finishes (without a jump via the continuation), we need to remember to delete the continuation binding
                ActiveActor
                    (actorState
                        |> do (Computation body)
                        |> push (RestoreEnv env)
                        |> getStack (\stack -> bind var (StackValue env stack))
                    )

        RestoreStackWith computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (RestoreStackWithLeftHole computation1)
                    )

        -- Delimited Continuations
        Reset { body } ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation body)
                        |> push (RestoreEnv env)
                        |> reset
                    )

        Shift { var, body } ->
            Return <|
                -- capture currently delimited stack and execute `body` with `var := current delimited stack`
                -- and the restored stack
                let
                    ( maybeDelimitedStack, newActor ) =
                        actorState
                            |> do (Computation body)
                            |> shift
                in
                case maybeDelimitedStack of
                    Just delimitedStack ->
                        newActor
                            |> mapActor
                                (bind var (DelimitedStackValue env delimitedStack)
                                    >> push (RestoreEnv env)
                                )

                    Nothing ->
                        newActor

        RestoreDelimitedStackWith computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (RestoreDelimitedStackWithLeftHole computation1)
                    )

        -- Environments
        Let computation0 { var, body } ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (LetWithLeftHole { var = var, body = body })
                    )

        GetEnv ->
            Return <|
                ActiveActor (actorState |> do (Value (EnvValue env)))

        WithIn computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (WithInLeftHole computation1)
                    )

        -- Console
        Log computation0 computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push (LogLeftHole computation1)
                    )

        -- Actor Model
        Receive ->
            Return <|
                let
                    ( maybeVal, newActor ) =
                        receive actorState
                in
                case maybeVal of
                    Nothing ->
                        newActor

                    Just val ->
                        newActor
                            |> mapActor (do (Value val))

        Send addressComputation messageComputation computation1 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation addressComputation)
                        |> push (SendLeftHole messageComputation computation1)
                    )

        Spawn computation0 ->
            SpawnActor env computation0 <|
                ActiveActor
                    (actorState
                        |> do (Value (Address nextAddress))
                    )

        Self ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Value (Address currentAddress))
                    )

        -- Loops
        Halt computation0 ->
            Return <|
                ActiveActor
                    (actorState
                        |> do (Computation computation0)
                        |> push HaltHole
                    )


applyPredicate : Predicate -> Value -> Value
applyPredicate pred val =
    (ConstantValue << boolToConstant)
        (case val of
            ConstantValue constant ->
                case constant of
                    IntConst _ ->
                        case pred of
                            IsInt ->
                                True

                            _ ->
                                False

                    -- Bool
                    TrueConst ->
                        case pred of
                            IsBool ->
                                True

                            _ ->
                                False

                    FalseConst ->
                        case pred of
                            IsBool ->
                                True

                            _ ->
                                False

                    -- String
                    StringConst _ ->
                        case pred of
                            IsString ->
                                True

                            _ ->
                                False

            -- Closure
            ClosureValue _ ->
                case pred of
                    IsClosure ->
                        True

                    _ ->
                        False

            TaggedValue _ _ _ ->
                case pred of
                    IsTagged ->
                        True

                    _ ->
                        False

            -- Tuple
            TupleValue _ _ ->
                case pred of
                    IsTuple ->
                        True

                    _ ->
                        False

            -- Stack/Continuation
            StackValue _ _ ->
                case pred of
                    IsStack ->
                        True

                    _ ->
                        False

            DelimitedStackValue _ _ ->
                case pred of
                    IsDelimitedStack ->
                        True

                    _ ->
                        False

            -- First class environments
            EnvValue _ ->
                case pred of
                    IsEnv ->
                        True

                    _ ->
                        False

            Address _ ->
                case pred of
                    IsAddress ->
                        True

                    _ ->
                        False
        )


matchPatternWithBranch : Tag -> Int -> List Value -> List { pattern : Pattern, body : Computation } -> Maybe { bindings : List ( VarName, Value ), body : Computation }
matchPatternWithBranch tag arity values branches =
    branches
        |> find
            (\{ pattern, body } ->
                pattern
                    |> matchPattern tag arity values
                    |> Maybe.map (\bindings -> { bindings = bindings, body = body })
            )


matchPattern : Tag -> Int -> List Value -> Pattern -> Maybe (List ( VarName, Value ))
matchPattern tag arity values pattern =
    case pattern of
        TagPattern patternTag patternArity vars ->
            if tag == patternTag && arity == patternArity then
                Just (List.zip vars values)

            else
                Nothing

        AnyPattern ->
            Just []
