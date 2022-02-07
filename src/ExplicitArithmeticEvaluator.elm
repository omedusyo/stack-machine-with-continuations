module ExplicitArithmeticEvaluator exposing (..)

-- helpers


type Either a b
    = Left a
    | Right b



-- ===Stack Machine===


type Computation
    = ReturnInteger Int
    | Add Computation Computation
    | Multiply Computation Computation


type Value
    = IntegerValue Int


type StackElement
    = AddWithLeftHole Computation -- models `_ + computation`
    | AddWithRightHole Value -- models `value + _`
    | MultiplyWithLeftHole Computation -- models `_ * computation`
    | MultiplyWithRightHole Value -- models `value * _`


type alias Stack =
    List StackElement


type CurrentComputation
    = Value Value
    | Computation Computation


type alias State =
    { stack : Stack
    , currentComputation : CurrentComputation
    }


smallStepEval : State -> Either State ()
smallStepEval ({ stack, currentComputation } as state) =
    case currentComputation of
        Value val ->
            -- (Remembering work)
            case stack of
                [] ->
                    -- Here we are in (value, no-work-to-be-done) case so we terminate
                    Right ()

                work :: stack1 ->
                    -- Current computation is a value and we have work to do on the stack
                    -- Whatever the value/work, the work stack will get popped.
                    case work of
                        AddWithLeftHole comp1 ->
                            -- (Remembering work)
                            -- val, _ + comp1 :: ... ~> comp1, val + _ :: ...
                            Left { stack = AddWithRightHole val :: stack1, currentComputation = Computation comp1 }

                        AddWithRightHole (IntegerValue x) ->
                            case val of
                                IntegerValue y ->
                                    -- (Actual work)
                                    -- y, x + _ :: ... ~> actual result of x + y, ...
                                    Left { stack = stack1, currentComputation = Value (IntegerValue (x + y)) }

                        -- Note that the following two cases are basically the same code as the previous two cases
                        MultiplyWithLeftHole comp1 ->
                            -- (Remembering work)
                            -- val, _ + comp1 :: ... ~> comp1, val + _ :: ...
                            Left { stack = MultiplyWithRightHole val :: stack1, currentComputation = Computation comp1 }

                        MultiplyWithRightHole (IntegerValue x) ->
                            case val of
                                IntegerValue y ->
                                    -- (Actual work)
                                    -- y, x + _ :: ... ~> actual result of x + y, ...
                                    Left { stack = stack1, currentComputation = Value (IntegerValue (x * y)) }

        Computation comp ->
            -- Current computation is a complex computation, so we need to somehow decompose it into something smaller and push new work that should be done afterwards
            case comp of
                ReturnInteger x ->
                    -- (Simplify) This is a constant!
                    Left { stack = stack, currentComputation = Value (IntegerValue x) }

                Add comp0 comp1 ->
                    -- (Simplify)
                    -- comp0 + comp1, stack ~> comp0, _ + comp1 :: stack
                    Left { stack = AddWithLeftHole comp1 :: stack, currentComputation = Computation comp0 }

                Multiply comp0 comp1 ->
                    -- (Simplify)
                    -- comp0 + comp1, stack ~> comp0, _ * comp1 :: stack
                    Left { stack = MultiplyWithLeftHole comp1 :: stack, currentComputation = Computation comp0 }
