module ExplicitEvaluator exposing (..)

import Dict exposing (..)
import List.Extra as List



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
      -- String
    | StringConst String


type Value
    = ConstantValue Constant
      -- Closure
    | ClosureValue Env { var : VarName, body : Computation }
      -- Tuple
    | TupleValue Int (List Value) -- invariant: for `TupleValue n values` we require `n == List.length values`
      -- Stack/Continuation
    | StackValue Env Stack
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
    | IsTuple
    | IsStack
    | IsEnv


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
      -- Tuple Type
      -- (M0, M1, M2)   <-->   Tuple 3 [M0, M1, M2]
    | Tuple Int (List Computation) -- invariant: for `Tuple n computations` we require `n == List.length compuations`
      -- (M0, M1, M2).1 ~> M1
    | Project Computation Int
      -- Stack/Current Continuation
    | SaveStack { var : VarName, body : Computation }
    | RestoreStackWith Computation Computation -- `restore-stack-with k` comp This is like function application, but we're "applying" a frozen stack `restoreStack k v`
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
      -- Tuple
    | TupleWithHole Int (List Value) (List Computation) -- invariant: for `TupleWithHole n reversedValues computations` we require `n == 1 + List.length reversedValues + List.length computations
    | ProjectWithHole Int
      -- Stack/Current Continuation
    | RestoreStackWithLeftHole Computation -- restoreStack _ M
    | RestoreStackWithRightHole Value -- restoreStack V _
      -- Environment restoration after closure application/stack save is done
    | RestoreEnv Env
      -- First class environments
    | LetWithLeftHole { var : VarName, body : Computation } -- let x = _ in body
    | WithInLeftHole Computation -- with _ in M
      -- Console
    | LogLeftHole Computation -- log _ M


type alias Stack =
    List StackElement


type alias Console =
    List Value


type alias State =
    { env : Env
    , stack : Stack
    , currentComputation : CurrentComputation
    , console : Console
    }


type RunTimeError
    = ExpectedBoolean
    | ExpectedInteger
    | ExpectedClosure
    | ExpectedTuple
    | ExpectedStack
    | ExpectedEnv
    | TupleArityMismatch { expected : Int, got : Int }
    | CantProject { tupleSize : Int, index : Int }
    | UnboundVarName VarName



-- Right () means the computation has terminated


smallStepEval : State -> Result RunTimeError (Either State ())
smallStepEval ({ env, stack, currentComputation, console } as state) =
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
                    combineValWithTopOfStack env stack1 console val stackEl
                        |> Result.map Left

        Computation comp ->
            -- Current computation is not a value, and so it will be decomposed into smaller pieces.
            -- Either additional work will be pushed onto the stack or the current computation will be transformed into a value
            decompose env stack console comp
                |> Result.map Left


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

            -- Tuple
            TupleValue _ _ ->
                case pred of
                    IsTuple ->
                        True

                    _ ->
                        False

            -- Stack/Continuation
            StackValue _ _ ->
                case IsStack of
                    IsStack ->
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


combineValWithTopOfStack : Env -> Stack -> Console -> Value -> StackElement -> Result RunTimeError State
combineValWithTopOfStack env stack console val stackEl =
    -- The only interesting cases are `PrimitiveIntOperation2RightHole`, `IfThenElseHole`, `ApplicationRightHole`, `RestoreStackWithRightHole`, and `RestoreEnv`
    case stackEl of
        PrimitiveIntOperation2LeftHole op computation1 ->
            Ok { env = env, stack = PrimitiveIntOperation2RightHole op val :: stack, currentComputation = Computation computation1, console = console }

        PrimitiveIntOperation2RightHole op value0 ->
            case ( value0, val ) of
                ( ConstantValue (IntConst x), ConstantValue (IntConst y) ) ->
                    Ok { env = env, stack = stack, currentComputation = Value (applyPrimitiveOp2 op x y), console = console }

                _ ->
                    Err ExpectedInteger

        PredicateApplicationHole pred ->
            Ok { env = env, stack = stack, currentComputation = Value (applyPredicate pred val), console = console }

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
                |> Result.map (\branch -> { env = env, stack = stack, currentComputation = Computation branch.body, console = console })

        -- Application
        ApplicationLeftHole computation1 ->
            Ok { env = env, stack = ApplicationRightHole val :: stack, currentComputation = Computation computation1, console = console }

        ApplicationRightHole value0 ->
            -- This is applying a closure to a value case (`value0` is the closure, `val` is the argument)
            case value0 of
                ClosureValue frozenEnv { var, body } ->
                    -- Evaluate the body of the closure in the closure's environment extended with the argument bound to the closure's parameter.
                    -- Also don't forget to push environment cleanup after the closure is evaluated.
                    Ok { env = frozenEnv |> insertEnv var val, stack = RestoreEnv env :: stack, currentComputation = Computation body, console = console }

                _ ->
                    Err ExpectedClosure

        -- Tuples
        TupleWithHole n reversedValues computations0 ->
            case computations0 of
                [] ->
                    Ok { env = env, stack = stack, currentComputation = Value (TupleValue n (List.reverse (val :: reversedValues))), console = console }

                computation0 :: computations1 ->
                    Ok { env = env, stack = TupleWithHole n (val :: reversedValues) computations1 :: stack, currentComputation = Computation computation0, console = console }

        ProjectWithHole k ->
            case val of
                TupleValue n values ->
                    case List.getAt k values of
                        Just val0 ->
                            Ok { env = env, stack = stack, currentComputation = Value val0, console = console }

                        Nothing ->
                            Err (CantProject { tupleSize = n, index = k })

                _ ->
                    Err ExpectedTuple

        -- Stack/Current Continuation
        RestoreStackWithLeftHole computation1 ->
            Ok { env = env, stack = RestoreStackWithRightHole val :: stack, currentComputation = Computation computation1, console = console }

        RestoreStackWithRightHole value0 ->
            -- This is restoring the stack `value0` with current computation := val
            case value0 of
                StackValue frozenEnv frozenStack ->
                    Ok { env = frozenEnv, stack = frozenStack, currentComputation = Value val, console = console }

                _ ->
                    Err ExpectedClosure

        -- Environment restoration after closure application/stack save is done
        RestoreEnv envToBeRestored ->
            Ok { env = envToBeRestored, stack = stack, currentComputation = Value val, console = console }

        -- First class environents
        -- let-binding
        LetWithLeftHole { var, body } ->
            Ok { env = env |> insertEnv var val, stack = RestoreEnv env :: stack, currentComputation = Computation body, console = console }

        WithInLeftHole computation1 ->
            case val of
                EnvValue env0 ->
                    Ok { env = env0, stack = RestoreEnv env :: stack, currentComputation = Computation computation1, console = console }

                _ ->
                    Err ExpectedEnv

        -- Console
        LogLeftHole computation1 ->
            Ok { env = env, stack = stack, currentComputation = Computation computation1, console = val :: console }


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


decompose : Env -> Stack -> Console -> Computation -> Result RunTimeError State
decompose env stack console comp =
    -- The only interesting cases are `VarUse`, `Lambda`, and `SaveStack`
    case comp of
        ConstantComputation constant ->
            Ok { env = env, stack = stack, currentComputation = Value (ConstantValue constant), console = console }

        -- Variable use
        VarUse varName ->
            case env |> getEnv varName of
                Just val ->
                    Ok { env = env, stack = stack, currentComputation = Value val, console = console }

                Nothing ->
                    Err (UnboundVarName varName)

        -- Primitive arithmetic
        PrimitiveIntOperation2 op computation0 computation1 ->
            Ok { env = env, stack = PrimitiveIntOperation2LeftHole op computation1 :: stack, currentComputation = Computation computation0, console = console }

        PredicateApplication pred computation0 ->
            Ok { env = env, stack = PredicateApplicationHole pred :: stack, currentComputation = Computation computation0, console = console }

        -- Bool
        IfThenElse computation leftBranch rightBranch ->
            Ok { env = env, stack = IfThenElseHole leftBranch rightBranch :: stack, currentComputation = Computation computation, console = console }

        -- Function type introduction
        Lambda { var, body } ->
            -- The current environment will get captured in the closure
            Ok { env = env, stack = stack, currentComputation = Value (ClosureValue env { var = var, body = body }), console = console }

        -- Function type elimination
        Application computation0 computation1 ->
            Ok { env = env, stack = ApplicationLeftHole computation1 :: stack, currentComputation = Computation computation0, console = console }

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
                        Ok { env = env, stack = stack, currentComputation = Value (TupleValue 0 []), console = console }

                    computation0 :: computations1 ->
                        Ok
                            { env = env
                            , stack =
                                TupleWithHole n [] computations1
                                    :: stack
                            , currentComputation = Computation computation0
                            , console = console
                            }

        Project computation0 k ->
            Ok { env = env, stack = ProjectWithHole k :: stack, currentComputation = Computation computation0, console = console }

        -- Stack/Current Continuation
        SaveStack { var, body } ->
            -- Environment is extended with the current stack (continuation)
            -- Also if the computation ever finishes (without a jump via the continuation), we need to remember to delete the continuation binding
            Ok { env = env |> insertEnv var (StackValue env stack), stack = RestoreEnv env :: stack, currentComputation = Computation body, console = console }

        RestoreStackWith computation0 computation1 ->
            Ok { env = env, stack = RestoreStackWithLeftHole computation1 :: stack, currentComputation = Computation computation0, console = console }

        -- Environments
        Let computation0 { var, body } ->
            Ok { env = env, stack = LetWithLeftHole { var = var, body = body } :: stack, currentComputation = Computation computation0, console = console }

        GetEnv ->
            Ok { env = env, stack = stack, currentComputation = Value (EnvValue env), console = console }

        WithIn computation0 computation1 ->
            Ok { env = env, stack = WithInLeftHole computation1 :: stack, currentComputation = Computation computation0, console = console }

        -- Console
        Log computation0 computation1 ->
            Ok { env = env, stack = LogLeftHole computation1 :: stack, currentComputation = Computation computation0, console = console }
