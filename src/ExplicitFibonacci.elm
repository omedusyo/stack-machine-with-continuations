module ExplicitFibonacci exposing (..)

-- Fibonacci sequence
--   0, 1, 2, 3, 5, 8, 13, 21, ...
-- i.e. first two are 0, 1, otherwise each element is the sum of previous two
-- ===Recursive definition===


fibRecursive : Int -> Int
fibRecursive x =
    if x < 2 then
        x

    else
        fibRecursive (x - 1) + fibRecursive (x - 2)



-- ===Iterative definition===
-- To compute `fib 4` we can apply the transformation `(a, b) ~> (b, a + b)` 5 times starting with `(0, 1)`
--   (0, 1)
--   (1, 1)
--   (1, 2)
--   (2, 3)
--   (3, 5)
-- then look at the first component (in this case 3)


fibIterative : Int -> Int
fibIterative x =
    let
        loop n a b =
            if n < 2 then
                a

            else
                loop (n - 1) b (a + b)
    in
    loop x 0 1



-- ===Explicit Incremental definition===
-- Let us first look what the recursive definition does for `x == 4`. The angle braces denote the currently focused computation
--   <fib 4>
--   <fib 3> + fib 2
--   (<fib 2> + fib 1) + fib 2
--   ((<fib 1> + fib 0) + fib 1) + fib 2
--   ((<1> + fib 0) + fib 1) + fib 2
--   ((1 + <fib 0>) + fib 1) + fib 2
--   ((<1 + 0>) + fib 1) + fib 2
--   (<1> + fib 1) + fib 2
--   (1 + <fib 1>) + fib 2
--   (1 + <1>) + fib 2
--   (<1 + 1>) + fib 2
--   <2> + fib 2
--   2 + <fib 2>
--   2 + (<fib 1> + fib 0)
--   2 + (<1> + fib 0)
--   2 + (1 + <fib 0>)
--   2 + (1 + <0>)
--   2 + (<1 + 0>)
--   2 + <1>
--   <2 + 1>
--   <3>
--
-- How could we represent each line as data in Elm (we'll call it the State of the computation)?
-- We could model the state as a pair
-- of (currently focused computation, stack of work) e.g.
--
--   <fib 4>                              ~>  (Call 4, [])
--   <fib 3> + fib 2                      ~>  (Call 3, RememberToCall 2 :: [])
--   (<fib 2> + fib 1) + fib 2            ~>  (Call 2, RememberToCall 1 :: RememberToCall 2 :: [])
--   ((<fib 1> + fib 0) + fib 1) + fib 2  ~>  (Call 1, RememberToCall 0 :: RememberToCall 1 :: RememberToCall 2 :: [])
--   ((<1> + fib 0) + fib 1) + fib 2      ~>  (Done 1, RememberToCall 0 :: RememberToCall 1 :: RememberToCall 2 :: [])
--   ((1 + <fib 0>) + fib 1) + fib 2      ~>  (Call 0, RememberToAdd  1 :: RememberToCall 1 :: RememberToCall 2 :: [])
--   ((1 + <0>) + fib 1) + fib 2          ~>  (Done 0, RememberToAdd  1 :: RememberToCall 1 :: RememberToCall 2 :: [])
--   (<1> + fib 1) + fib 2                ~>  (Done 1, RememberToCall 1 :: RememberToCall 2 :: [])
--   (1 + <fib 1>) + fib 2                ~>  (Call 1, RememberToAdd  1 :: RememberToCall 2 :: [])
--   (1 + <1>) + fib 2                    ~>  (Done 1, RememberToAdd  1 :: RememberToCall 2 :: []
--   <2> + fib 2                          ~>  (Done 2, RememberToCall 2 :: [])
--   2 + <fib 2>                          ~>  (Call 2, RememberToAdd  2 :: [])
--   2 + (<fib 1> + fib 0)                ~>  (Call 1, RememberToCall 0 :: RememberToAdd  2 :: [])
--   2 + (<1> + fib 0)                    ~>  (Done 1, RememberToCall 0 :: RememberToAdd  2 :: [])
--   2 + (1 + <fib 0>)                    ~>  (Call 0, RememberToAdd  1 :: RememberToAdd  2 :: [])
--   2 + (1 + <0>)                        ~>  (Done 0, RememberToAdd  1 :: RememberToAdd  2 :: [])
--   2 + <1>                              ~>  (Done 1, RememberToAdd  2 :: [])
--   <3>                                  ~>  (Done 3, [])


type CurrentComputation
    = Call Int
    | Done Int


type Work
    = RememberToCall Int
    | RememberToAdd Int


type alias Stack =
    List Work


type alias State =
    ( CurrentComputation, Stack )


type Either a b
    = Left a
    | Right b


fibIncrementalExplicit : State -> Either State ()
fibIncrementalExplicit state =
    -- Note that there's no recursion here
    case state of
        ( Call x, stack ) ->
            if x < 2 then
                Left ( Done x, stack )

            else
                Left ( Call (x - 1), RememberToCall (x - 2) :: stack )

        ( Done y0, [] ) ->
            -- terminate
            Right ()

        ( Done y0, (RememberToCall x) :: stack ) ->
            Left ( Call x, RememberToAdd y0 :: stack )

        ( Done y0, (RememberToAdd y1) :: stack ) ->
            Left ( Done (y0 + y1), stack )



-- Iterative `fibIncrementalExplicit` with argument `x`, `n`-times


iterate : Int -> a -> (a -> Either a ()) -> Either a ()
iterate n a0 f =
    if n == 0 then
        Left a0

    else
        case f a0 of
            Left a1 ->
                iterate (n - 1) a1 f

            Right () ->
                Right ()


fibIterateState : Int -> Int -> Either State ()
fibIterateState n x =
    iterate n ( Call x, [] ) fibIncrementalExplicit
