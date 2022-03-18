module Example exposing (..)

import Dict exposing (Dict)
import ExplicitEvaluator exposing (..)
import Queue exposing (Queue)


type alias Example =
    { name : String
    , nextAddress : ActorId
    , actors :
        Dict ActorId ExampleActor
    }


type alias ExampleActor =
    { env : Env, comp : Computation, mailbox : Mailbox }


updateExampleActor : ActorId -> (ExampleActor -> ExampleActor) -> Example -> Example
updateExampleActor actorId f ({ actors } as ex) =
    { ex
        | actors =
            actors
                |> Dict.update actorId (Maybe.map f)
    }


example : String -> Example
example name =
    { name = name
    , nextAddress = 0
    , actors = Dict.fromList []
    }


exampleActor : Computation -> ExampleActor
exampleActor comp =
    { env = Dict.empty, comp = comp, mailbox = Queue.empty }


addActor : ExampleActor -> Example -> Example
addActor exActor ex =
    { ex
        | nextAddress = ex.nextAddress + 1
        , actors = ex.actors |> Dict.insert ex.nextAddress exActor
    }


withBinding : VarName -> Value -> ExampleActor -> ExampleActor
withBinding varName val exActor =
    { exActor | env = exActor.env |> insertEnv varName val }


queueMessage : ActorId -> Value -> ExampleActor -> ExampleActor
queueMessage address val exActor =
    { exActor | mailbox = exActor.mailbox |> Queue.enqueue val }


c : Int -> Computation
c x =
    ConstantComputation (IntConst x)


str : String -> Computation
str s =
    ConstantComputation (StringConst s)


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


empty =
    Tuple 0 []


pair a b =
    Tuple 2 [ a, b ]


triple a0 a1 a2 =
    Tuple 3 [ a0, a1, a2 ]


example0 : Example
example0 =
    -- 3 + 2
    example "arithmetic-0"
        |> addActor (exampleActor (add (c 3) (c 2)))


example1 : Example
example1 =
    -- (3 + 2) * (6 + 7)
    example "arithmetic-1"
        |> addActor
            (exampleActor
                (mul
                    (add (c 3) (c 2))
                    (add (c 7) (c 7))
                )
            )


example2 : Example
example2 =
    -- x + 7
    example "variable"
        |> addActor
            (exampleActor
                (add (VarUse "x") (c 7))
                |> withBinding "x" (cv 5)
            )


example3 : Example
example3 =
    -- if x < 3 then 1 else 5
    example "if-the-else"
        |> addActor
            (exampleActor
                (IfThenElse
                    (lt (VarUse "x") (c 3))
                    { body = c 7 }
                    { body = c 5 }
                )
                |> withBinding "x" (cv 2)
            )


example4 : Example
example4 =
    -- [(fn x -> x + 1) 5]
    example "function application"
        |> addActor
            (exampleActor
                (Application
                    (Lambda
                        { var = "x"
                        , body = add (VarUse "x") (c 1)
                        }
                    )
                    (c 5)
                )
            )


example5 : Example
example5 =
    -- 1 + (saveStack k -> if x < 3 then 5 else restoreStack k 6)
    example "call/cc-0"
        |> addActor
            (exampleActor
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
            )


example6 : Example
example6 =
    example "runtime error"
        |> addActor (exampleActor (add (c 1) true))


example7 : Example
example7 =
    -- (let x = 3 in x + 1) + 5
    example "let binding"
        |> addActor
            (exampleActor
                (add
                    (Let (c 3) { var = "x", body = add (VarUse "x") (c 1) })
                    (c 5)
                )
            )


example8 : Example
example8 =
    -- let x = 3 in
    --   get-env
    example "get environment"
        |> addActor
            (exampleActor
                (Let (c 3)
                    { var = "x"
                    , body = GetEnv
                    }
                )
            )


example9 : Example
example9 =
    -- let x = 3;
    -- let e = get-env;
    -- let x = 5;
    -- (with e; x) + x
    example "evaluating with saved environment 0"
        |> addActor
            (exampleActor
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
            )


example10 : Example
example10 =
    example "tuples 0"
        |> addActor (exampleActor (Tuple 3 [ c 0, add (c 1) (c 2), c 5 ]))


example11 : Example
example11 =
    example "tuples 2"
        |> addActor
            (exampleActor
                (Project
                    (Tuple 3 [ c 16, c 3, c 5 ])
                    1
                )
            )


example12 : Example
example12 =
    example "console"
        |> addActor
            (exampleActor
                (Log (str "hello")
                    (Log (c 1)
                        (c 2)
                    )
                )
            )


example13 : Example
example13 =
    -- let val =
    --   save-stack k ->
    --     log "Executed"
    --     (restore-stack k 5;
    --     , log "Not executed" ()
    --     ).1;
    --   log-ln val;
    --   ()
    example "call/cc with printing"
        |> addActor
            (exampleActor
                (Let
                    (SaveStack
                        { var = "k"
                        , body =
                            Log (str "Executed")
                                (Project
                                    (pair
                                        (RestoreStackWith (VarUse "k") (c 5))
                                        (Log (str "Not executed") empty)
                                    )
                                    1
                                )
                        }
                    )
                    { var = "val"
                    , body =
                        Log (VarUse "val") empty
                    }
                )
            )


example14 : Example
example14 =
    -- if isInt 5 then 3 else 5
    example "predicates"
        |> addActor (exampleActor (IfThenElse (PredicateApplication IsInt (str "hello")) { body = c 3 } { body = c 5 }))


example15 : Example
example15 =
    -- let val = :first (save-stack k k);
    -- case val of
    --   :first k ->
    --     log "First";
    --     log k;
    --     restore-stack k (:second 5)
    --   :second x
    --     log "Second";
    --     log x;
    --     ()
    example "Binding a stack to a variable"
        |> addActor
            (exampleActor
                (Let
                    (SaveStack { var = "k", body = Tagged "first" 1 [ VarUse "k" ] })
                    { var = "val"
                    , body =
                        MatchTagged
                            (VarUse "val")
                            [ { pattern = TagPattern "first" 1 [ "k" ]
                              , body =
                                    Log (VarUse "val")
                                        (RestoreStackWith (VarUse "k") (Tagged "second" 1 [ c 5 ]))
                              }
                            , { pattern = TagPattern "second" 1 [ "x" ]
                              , body =
                                    Log (VarUse "val")
                                        empty
                              }
                            ]
                    }
                )
            )


example16 : Example
example16 =
    -- :just(3)
    example "Tagged values 0"
        |> addActor (exampleActor (Tagged "just" 1 [ c 3 ]))


example17 : Example
example17 =
    -- :nothing
    example "Tagged values 1"
        |> addActor (exampleActor (Tagged "nothing" 0 []))


example18 : Example
example18 =
    -- :cons(73, :cons(30, :empty))
    example "Tagged values 2"
        |> addActor (exampleActor (Tagged "cons" 2 [ c 73, Tagged "cons" 2 [ c 30, Tagged "empty" 0 [] ] ]))


example19 : Example
example19 =
    -- case :just(1) of
    --   :nothing -> 0
    --   :just(x) -> x + 20
    example "Tagged values 3"
        |> addActor
            (exampleActor
                (MatchTagged
                    (Tagged "just" 1 [ c 5 ])
                    [ { pattern = TagPattern "nothing" 0 []
                      , body = c 5
                      }
                    , { pattern = TagPattern "just" 1 [ "x" ]
                      , body = add (VarUse "x") (c 20)
                      }
                    ]
                )
            )


example20 : Example
example20 =
    example "Delimited continuations 0"
        |> addActor
            (exampleActor
                (add
                    (Reset
                        { body =
                            add (c 34) (Shift { var = "k", body = c 12 })
                        }
                    )
                    (c 1)
                )
            )


example21 : Example
example21 =
    example "Delimited continuations 1"
        |> addActor
            (exampleActor
                (add
                    (Reset
                        { body =
                            add (c 34)
                                (Shift
                                    { var = "k"
                                    , body =
                                        add (RestoreDelimitedStackWith (VarUse "k") (c 3))
                                            (RestoreDelimitedStackWith (VarUse "k") (c 4))
                                    }
                                )
                        }
                    )
                    (c 1)
                )
            )


example22 : Example
example22 =
    -- let a = 3;
    -- let b = 4;
    -- reset {
    --   let x = 34;
    --   x + shift { pair(k -> k 3 + k 4, x) }
    -- }
    example "Delimited continuations 2"
        |> addActor
            (exampleActor
                (Let (c 3)
                    { var = "a"
                    , body =
                        Let (c 4)
                            { var = "b"
                            , body =
                                Reset
                                    { body =
                                        Let (c 34)
                                            { var = "x"
                                            , body =
                                                add (VarUse "x")
                                                    (Shift
                                                        { var = "k"
                                                        , body =
                                                            Tagged "pair"
                                                                2
                                                                [ add
                                                                    (RestoreDelimitedStackWith (VarUse "k") (VarUse "a"))
                                                                    (RestoreDelimitedStackWith (VarUse "k") (VarUse "b"))
                                                                , VarUse "x"
                                                                ]
                                                        }
                                                    )
                                            }
                                    }
                            }
                    }
                )
            )


example23 : Example
example23 =
    example "Actors: receive from mailbox"
        |> addActor
            (exampleActor Receive
                |> queueMessage 0 (cv 5)
                |> queueMessage 0 (cv 3)
            )


example24 : Example
example24 =
    example "Actors: receive from empty mailbox and get blocked"
        |> addActor (exampleActor Receive)


example25 : Example
example25 =
    example "Actors: multiple independent actors"
        |> addActor
            (exampleActor (add (c 3) (c 2)))
        |> addActor
            (exampleActor
                (mul
                    (add (c 3) (c 2))
                    (add (c 7) (c 7))
                )
            )
        |> addActor
            (exampleActor
                (add (c 3) (c 18))
            )


example26 : Example
example26 =
    example "Actors: communication"
        |> addActor
            (exampleActor
                (Send (VarUse "actorBelow") (c 6) (add (c 3) (c 2)))
                |> withBinding "actorBelow" (Address 1)
            )
        |> addActor
            (exampleActor
                Receive
            )


example27 : Example
example27 =
    example "Actors: spawning"
        |> addActor
            (exampleActor
                (Let
                    (c 1)
                    { var = "x"
                    , body =
                        Let
                            (Spawn
                                (add Receive (VarUse "x"))
                            )
                            { var = "child"
                            , body = Send (VarUse "child") (c 5) (add (c 3) (c 5))
                            }
                    }
                )
            )


example28 : Example
example28 =
    example "Actors: self"
        |> addActor
            (exampleActor (Send Self (c 123) (add Receive (c 1))))


example29 : Example
example29 =
    example "Loop"
        |> addActor
            (exampleActor
                (Repeat (Lambda { var = "x", body = add (VarUse "x") (c 1) })
                    (c 0)
                )
            )


defaultExample : Example
defaultExample =
    example29


examples : List Example
examples =
    [ example0, example1, example2, example3, example4, example5, example6, example7, example8, example9, example10, example11, example12, example13, example14, example15, example16, example17, example18, example19, example20, example21, example22, example23, example24, example25, example26, example27, example28, example29 ]
