module Example exposing (..)

import Dict exposing (Dict)
import ExplicitEvaluator exposing (..)


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


example7 : Example
example7 =
    -- (let x = 3 in x + 1) + 5
    example "let binding"
        (add
            (Let (c 3) { var = "x", body = add (VarUse "x") (c 1) })
            (c 5)
        )


example8 : Example
example8 =
    -- let x = 3 in
    --   get-env
    example "get environment"
        (Let (c 3)
            { var = "x"
            , body = GetEnv
            }
        )


example9 : Example
example9 =
    -- let x = 3;
    -- let e = get-env;
    -- let x = 5;
    -- (with e; x) + x
    example "evaluating with saved environment 0"
        (Let (c 3)
            { var = "x"
            , body =
                Let GetEnv
                    { var = "e"
                    , body =
                        Let (c 5)
                            { var = "x"
                            , body =
                                add (WithIn (VarUse "e") (VarUse "x")) (VarUse "x")
                            }
                    }
            }
        )


defaultExample : Example
defaultExample =
    example9


examples : List Example
examples =
    [ example0, example1, example2, example3, example4, example5, example6, example7 ]
