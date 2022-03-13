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


type Value
    = ConstantValue Constant
      -- Closure
    | ClosureValue Env { var : VarName, body : Computation }
      -- Tagged Value
    | TaggedValue Tag Int (List Value)
      -- Tuple
    | TupleValue Int (List Value) -- invariant: for `TupleValue n values` we require `n == List.length values`
      -- Stack/Continuation
    | StackValue Env Stack
    | DelimitedStackValue Env DelimitedStack
      -- First class environments
    | EnvValue Env


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


type alias Actor =
    { env : Env
    , stack : Stack
    , currentComputation : CurrentComputation
    , console : Console
    , mailbox : Mailbox
    , isBlocked : Bool
    }


type alias Mailbox =
    Queue Value


type alias ActorId =
    Int


type alias ActorMsg =
    { destination : ActorId
    , payload : Value
    }


do : CurrentComputation -> Actor -> Actor
do currentComputation state =
    { state | currentComputation = currentComputation }


push : StackElement -> Actor -> Actor
push stackEl state =
    { state | stack = pushStack stackEl state.stack }


reset : Actor -> Actor
reset state =
    { state | stack = delimitStack state.stack }


resetTo : DelimitedStack -> Actor -> Actor
resetTo delimitedStack state =
    { state | stack = resetToDelimitedStack delimitedStack state.stack }


shift : Actor -> Result RunTimeError ( DelimitedStack, Actor )
shift ({ stack } as state) =
    case shiftStack stack of
        Just ( delimitedStack, stack1 ) ->
            Ok ( delimitedStack, { state | stack = stack1 } )

        Nothing ->
            Err ShiftingWithoutReset


setStack : Stack -> Actor -> Actor
setStack stack state =
    { state | stack = stack }


getStack : (Stack -> Actor -> Actor) -> Actor -> Actor
getStack f state =
    f state.stack state


setEnvironment : Env -> Actor -> Actor
setEnvironment env state =
    { state | env = env }


getEnvironment : (Env -> Actor -> Actor) -> Actor -> Actor
getEnvironment f state =
    f state.env state


bind : VarName -> Value -> Actor -> Actor
bind varName value state =
    { state | env = state.env |> insertEnv varName value }


log : Value -> Actor -> Actor
log value state =
    { state | console = value :: state.console }


isMailboxEmpty : Actor -> Bool
isMailboxEmpty actor =
    Queue.isEmpty actor.mailbox


receive : Actor -> ( Maybe Value, Actor )
receive actor =
    case Queue.dequeue actor.mailbox of
        Nothing ->
            ( Nothing, { actor | isBlocked = True } )

        Just ( val, newQueue ) ->
            ( Just val, { actor | mailbox = newQueue, isBlocked = False } )


send : Value -> Actor -> Actor
send val actor =
    { actor | mailbox = actor.mailbox |> Queue.enqueue val }


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
    | TaggedComputationArityMismatch { expected : Int, got : Int }
    | TupleArityMismatch { expected : Int, got : Int }
    | CantProject { tupleSize : Int, index : Int }
    | ShiftingWithoutReset
    | UnboundVarName VarName



-- Right () means the computation has terminated


smallStepEval : Actor -> Result RunTimeError (Either Actor ())
smallStepEval actor =
    case actor.currentComputation of
        Value val ->
            case popStack actor.stack of
                Nothing ->
                    -- Current computation is a value, but the stack is empty, so we terminate
                    -- TODO: What about shifting the stack?
                    Ok (Right ())

                Just ( stackEl, stack1 ) ->
                    -- Current computation is a value and we have work to do on the stack.
                    -- Based on what's on the stack we either fail or actually do some non-trivial computing
                    -- In either case the top of the stack is consumed
                    combine
                        val
                        stackEl
                        (actor |> setStack stack1)
                        |> Result.map Left

        Computation comp ->
            -- Current computation is not a value, and so it will be decomposed into smaller pieces.
            -- Either additional work will be pushed onto the stack or the current computation will be transformed into a value
            decompose actor.env
                comp
                actor
                |> Result.map Left


combine : Value -> StackElement -> Actor -> Result RunTimeError Actor
combine val stackEl actor =
    -- The only interesting cases are `PrimitiveIntOperation2RightHole`, `IfThenElseHole`, `ApplicationRightHole`, `RestoreStackWithRightHole`, and `RestoreEnv`
    case stackEl of
        PrimitiveIntOperation2LeftHole op computation1 ->
            -- Ok { env = env, stack = pushStack (PrimitiveIntOperation2RightHole op val) stack, currentComputation = Computation computation1, console = console }
            Ok
                (actor
                    |> do (Computation computation1)
                    |> push (PrimitiveIntOperation2RightHole op val)
                )

        PrimitiveIntOperation2RightHole op value0 ->
            case ( value0, val ) of
                ( ConstantValue (IntConst x), ConstantValue (IntConst y) ) ->
                    Ok (actor |> do (Value (applyPrimitiveOp2 op x y)))

                _ ->
                    Err ExpectedInteger

        PredicateApplicationHole pred ->
            Ok (actor |> do (Value (applyPredicate pred val)))

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
                |> Result.map
                    (\branch -> actor |> do (Computation branch.body))

        -- Application
        ApplicationLeftHole computation1 ->
            Ok
                (actor
                    |> do (Computation computation1)
                    |> push (ApplicationRightHole val)
                )

        ApplicationRightHole value0 ->
            -- This is applying a closure to a value case (`value0` is the closure, `val` is the argument)
            case value0 of
                ClosureValue frozenEnv { var, body } ->
                    -- Evaluate the body of the closure in the closure's environment extended with the argument bound to the closure's parameter.
                    -- Also don't forget to push environment cleanup after the closure is evaluated.
                    Ok
                        (actor
                            |> do (Computation body)
                            |> getEnvironment (\env -> push (RestoreEnv env))
                            |> bind var val
                        )

                _ ->
                    Err ExpectedClosure

        -- TaggedComputation
        TaggedWithHole tag n reversedValues computations0 ->
            case computations0 of
                [] ->
                    Ok (actor |> do (Value (TaggedValue tag n (List.reverse (val :: reversedValues)))))

                computation0 :: computations1 ->
                    Ok
                        (actor
                            |> do (Computation computation0)
                            |> push (TaggedWithHole tag n (val :: reversedValues) computations1)
                        )

        MatchTaggedWithHole branches ->
            case val of
                TaggedValue tag n values ->
                    case matchPatternWithBranch tag n values branches of
                        Just { bindings, body } ->
                            Ok
                                (actor
                                    |> do (Computation body)
                                    |> getEnvironment (\env -> push (RestoreEnv env))
                                    |> getEnvironment (\env -> setEnvironment (env |> insertsEnv bindings))
                                )

                        Nothing ->
                            Err MatchNotFound

                _ ->
                    Err ExpectedTaggedValue

        -- Tuples
        TupleWithHole n reversedValues computations0 ->
            case computations0 of
                [] ->
                    Ok (actor |> do (Value (TupleValue n (List.reverse (val :: reversedValues)))))

                computation0 :: computations1 ->
                    Ok
                        (actor
                            |> do (Computation computation0)
                            |> push (TupleWithHole n (val :: reversedValues) computations1)
                        )

        ProjectWithHole k ->
            case val of
                TupleValue n values ->
                    case List.getAt k values of
                        Just val0 ->
                            Ok (actor |> do (Value val0))

                        Nothing ->
                            Err (CantProject { tupleSize = n, index = k })

                _ ->
                    Err ExpectedTuple

        -- Stack/Current Continuation
        RestoreStackWithLeftHole computation1 ->
            Ok
                (actor
                    |> do (Computation computation1)
                    |> push (RestoreStackWithRightHole val)
                )

        RestoreStackWithRightHole value0 ->
            -- This is restoring the stack `value0` with current computation := val
            case value0 of
                StackValue frozenEnv frozenStack ->
                    Ok
                        (actor
                            |> do (Value val)
                            |> setStack frozenStack
                            |> setEnvironment frozenEnv
                        )

                _ ->
                    Err ExpectedStack

        -- Delimited Continuations
        RestoreDelimitedStackWithLeftHole computation1 ->
            Ok
                (actor
                    |> do (Computation computation1)
                    |> push (RestoreDelimitedStackWithRightHole val)
                )

        RestoreDelimitedStackWithRightHole value0 ->
            case value0 of
                DelimitedStackValue frozenEnv frozenDelimitedStack ->
                    -- 1. Push the current env to be restored
                    -- 2. Reset the current delimited stack to the frozen delim stack and same for the env
                    Ok
                        (actor
                            |> do (Value val)
                            |> getEnvironment (\env -> push (RestoreEnv env))
                            |> resetTo frozenDelimitedStack
                            |> setEnvironment frozenEnv
                        )

                _ ->
                    Err ExpectedDelimitedStack

        -- Environment restoration after closure application/stack save is done
        RestoreEnv envToBeRestored ->
            Ok
                (actor
                    |> do (Value val)
                    |> setEnvironment envToBeRestored
                )

        -- First class environents
        -- let-binding
        LetWithLeftHole { var, body } ->
            Ok
                (actor
                    |> do (Computation body)
                    |> getEnvironment (\env -> push (RestoreEnv env))
                    |> bind var val
                )

        WithInLeftHole computation1 ->
            case val of
                EnvValue env0 ->
                    Ok
                        (actor
                            |> do (Computation computation1)
                            |> getEnvironment (\env -> push (RestoreEnv env))
                            |> setEnvironment env0
                        )

                _ ->
                    Err ExpectedEnv

        -- Console
        LogLeftHole computation1 ->
            Ok
                (actor
                    |> do (Computation computation1)
                    |> log val
                )


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


decompose : Env -> Computation -> Actor -> Result RunTimeError Actor
decompose env comp actor =
    -- The only interesting cases are `VarUse`, `Lambda`, and `SaveStack`
    case comp of
        ConstantComputation constant ->
            Ok (actor |> do (Value (ConstantValue constant)))

        -- Variable use
        VarUse varName ->
            case env |> getEnv varName of
                Just val ->
                    Ok (actor |> do (Value val))

                Nothing ->
                    Err (UnboundVarName varName)

        -- Primitive arithmetic
        PrimitiveIntOperation2 op computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (PrimitiveIntOperation2LeftHole op computation1)
                )

        PredicateApplication pred computation0 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (PredicateApplicationHole pred)
                )

        -- Bool
        IfThenElse computation leftBranch rightBranch ->
            Ok
                (actor
                    |> do (Computation computation)
                    |> push (IfThenElseHole leftBranch rightBranch)
                )

        -- Function type introduction
        Lambda { var, body } ->
            -- The current environment will get captured in the closure
            Ok (actor |> do (Value (ClosureValue env { var = var, body = body })))

        -- Function type elimination
        Application computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (ApplicationLeftHole computation1)
                )

        -- Tagged Computations
        Tagged tag n computations0 ->
            let
                numOfComputations =
                    List.length computations0
            in
            if n /= numOfComputations then
                Err (TaggedComputationArityMismatch { expected = n, got = numOfComputations })

            else
                case computations0 of
                    [] ->
                        Ok (actor |> do (Value (TaggedValue tag 0 [])))

                    computation0 :: computations1 ->
                        Ok
                            (actor
                                |> do (Computation computation0)
                                |> push (TaggedWithHole tag n [] computations1)
                            )

        MatchTagged computation0 branches ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (MatchTaggedWithHole branches)
                )

        -- Tuple Construction
        Tuple n computations0 ->
            let
                numOfComputations =
                    List.length computations0
            in
            if n /= numOfComputations then
                Err (TupleArityMismatch { expected = n, got = numOfComputations })

            else
                case computations0 of
                    [] ->
                        Ok (actor |> do (Value (TupleValue 0 [])))

                    computation0 :: computations1 ->
                        Ok
                            (actor
                                |> do (Computation computation0)
                                |> push (TupleWithHole n [] computations1)
                            )

        Project computation0 k ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (ProjectWithHole k)
                )

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            -- Environment is extended with the current stack (continuation)
            -- Also if the computation ever finishes (without a jump via the continuation), we need to remember to delete the continuation binding
            Ok
                (actor
                    |> do (Computation body)
                    |> push (RestoreEnv env)
                    |> getStack (\stack -> bind var (StackValue env stack))
                )

        RestoreStackWith computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (RestoreStackWithLeftHole computation1)
                )

        -- Delimited Continuations
        Reset { body } ->
            Ok
                (actor
                    |> do (Computation body)
                    |> push (RestoreEnv env)
                    |> reset
                )

        Shift { var, body } ->
            -- capture currently delimited stack and execute `body` with `var := current delimited stack`
            -- and the restored stack
            actor
                |> do (Computation body)
                |> shift
                |> Result.map
                    (\( delimitedStack, newActor ) ->
                        newActor
                            |> bind var (DelimitedStackValue env delimitedStack)
                    )
                |> Result.map (push (RestoreEnv env))

        RestoreDelimitedStackWith computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (RestoreDelimitedStackWithLeftHole computation1)
                )

        -- Environments
        Let computation0 { var, body } ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (LetWithLeftHole { var = var, body = body })
                )

        GetEnv ->
            Ok (actor |> do (Value (EnvValue env)))

        WithIn computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (WithInLeftHole computation1)
                )

        -- Console
        Log computation0 computation1 ->
            Ok
                (actor
                    |> do (Computation computation0)
                    |> push (LogLeftHole computation1)
                )

        -- Actor Model
        Receive ->
            Ok
                (let
                    ( maybeVal, newActor ) =
                        receive actor
                 in
                 case maybeVal of
                    Nothing ->
                        newActor

                    Just val ->
                        newActor
                            |> do (Value val)
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
            ClosureValue _ _ ->
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
