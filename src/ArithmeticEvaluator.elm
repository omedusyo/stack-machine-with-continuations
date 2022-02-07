module ArithmeticEvaluator exposing (..)


type Computation
    = ReturnInteger Int
    | Add Computation Computation


type Value
    = IntegerValue Int


eval : Computation -> Value
eval comp =
    case comp of
        ReturnInteger x ->
            IntegerValue x

        Add comp0 comp1 ->
            case ( eval comp0, eval comp1 ) of
                ( IntegerValue x, IntegerValue y ) ->
                    IntegerValue (x + y)
