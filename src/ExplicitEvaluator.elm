module ExplicitEvaluator exposing (..)

import Dict exposing (..)



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
      -- First class environments
    | EnvValue Env


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
    | Lambda { var : VarName, body : Computation } -- fn x -> body
    | Application Computation Computation
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
      -- First class environments
    | LetWithLeftHole { var : VarName, body : Computation } -- let x = _ in body
    | WithInLeftHole Computation -- with _ in M


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
    | ExpectedEnv
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

        -- First class environents
        -- let-binding
        LetWithLeftHole { var, body } ->
            Ok { env = env |> insertEnv var val, stack = RestoreEnv env :: stack, currentComputation = Computation body }

        WithInLeftHole computation1 ->
            case val of
                EnvValue env0 ->
                    Ok { env = env0, stack = RestoreEnv env :: stack, currentComputation = Computation computation1 }

                _ ->
                    Err ExpectedEnv


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

        --Environments
        Let computation0 { var, body } ->
            Ok { env = env, stack = LetWithLeftHole { var = var, body = body } :: stack, currentComputation = Computation computation0 }

        GetEnv ->
            Ok { env = env, stack = stack, currentComputation = Value (EnvValue env) }

        WithIn computation0 computation1 ->
            Ok { env = env, stack = WithInLeftHole computation1 :: stack, currentComputation = Computation computation0 }
