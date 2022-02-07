WORK IN PROGRESS

# Incremental Computation

See [visualization](https://omedusyo.github.io/stack-machine-with-continuations/index.html).
This is a tutorial that ends with an explanation of how
[continuations](https://en.wikipedia.org/wiki/Continuation)/[call/cc](https://en.wikipedia.org/wiki/Call-with-current-continuation)
(in languages like [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))) work by actually implementing them.


# Part 1: How do we compute? - the Stack

Consider the expression `(3 + (4/2))*(6 + 7)`. How do we evaluate it? One way is to evaluate the subexpression `(3 + (4/2))`, and remember that afterwards we need to do `_ * (6 + 7)`. The blank `_` is waiting for the result. In this way we dive deeper and deeper into the expression while remembering more and more until we encounter a constant. In that case we backtrack and look at our todo list and do the work. Eventually we'll be forced to evaluate `constant operation constant`. Let's write down how such a thought process might look in detail
* **(Simplify)** Evaluate `(3 + (4/2))*(6 + 7)` and afterwards remember to do ... `nothing`. Looks complicated, but don't worry and just focus on the subexpression to the left of `*`.
* **(Simplify)** Evaluate `(3 + (4/2))` and afterwards remember to do `_ * (6 + 7) :: nothing`. Just focus on the subexpression to the left of `+`.
* **(Simplify)** Evaluate `3` and afterwards remember to do `_ + (4/2) :: _ * (6 + 7) :: nothing`. Ah, this is a constant, we can't simplify it further.
* **(Remember work)** Let's focus on the work we were supposed to do aftwerwards. Ah yes, we were supposed to do `_ + (4/2)` so let's focus on the right side of `+`. But we must not forget to `3 + _` afterwards!
* **(Simplify)** Evaluate `(4/2)` and afterwards remember to do `(3 + _) :: _ * (6 + 7) :: nothing`. Focus on LHS of `/`.
* **(Simplify)** Evaluate `4` and afterwards remember to do `_ / 2 :: (3 + _) :: _ * (6 + 7) :: nothing`. This is a constant.
* **(Remember work)** Afterwards we were supposed to do `_ / 2`. Focus on RHS of `/`.
* **(Simplify)** Evaluate `2` and afterwards remember to do `4 / _ :: (3 + _) :: _ * (6 + 7) :: nothing`. This is a constant.
* **(Remember work)** We need to divide `4` by `2`.
* **(Actual work)** We know that this is `2`. Finally we did some work.
* **(Simplify)** Evaluate `2` and afterwards remember to do `(3 + _) :: _ * (6 + 7) :: nothing`.
* **(Remember work)** We need to add `3` to `2`.
* **(Actual work)** We know this is `5`.
* **(Simplify)** Evalute `5` and afterwards remember to do `_ * (6 + 7) :: nothing`.
* **(Remember work)** Afterwards we were supposed to do `_ * (6 + 7)`. Focus on RHS of `*`.
* **(Simplify)** Evaluate `(6 + 7)` and afterwards remember to do `5 * _ :: nothing`. Focus on LHS of `+`.
* **(Simplify)** Evaluate `6` and afterwards remember to do `_ + 7 :: 5 * _ :: nothing`. This is a constant.
* **(Remember work)** Afterwards we were supposed to do `_ + 7`. Focus on RHS of `*`.
* **(Simplify)** Evaluate `7` and afterwards remember to do `6 + _ :: 5 * _ :: nothing`. This is a constant.
* **(Remember work)** We need to add `6` to `7`.
* **(Actual work)** We know this is `13`.
* **(Simplify)** Evaluate `13` and afterwards remember to do `5 * _ :: nothing`. This is a constant.
* **(Remember work)** We need to multiply `13` by `5`.
* **(Actual work)** We know this is `65`.
* **(Simplify)** Evaluate `65` and afterwards remember to do `nothing`. This is a constant.
* **(Terminate)** We don't need to do more work so we stop and the result of our computation is `65`.

Note that when evaluating `x operation y`, we arbitrarily chose to always focus on `x` first then `y`. We needed to remember two things. First our current computation (like `(6 + 7)`) and second what work we have to do afterwards (like `(3 + _) :: _ * (6 + 7) :: nothing`).  The second component of the state is basically a stack onto which we push simple work that we have to do. Given a state of thought `(current-computation-0, work-afterwards-0)` we transformed it into next state `(current-computation-1, work-afterwards-1)`. We could classify each state into three categories:
* **(Simplify)** in which we decomposed the current computation into something more managable/simpler and we pushed some work to do once we are finished with the current task. 
* **(Remember work)** here we looked at the top of the stack of and prepared for what to do next
* **(Actual work)** here we performed some primitive arithmetic operation of shape `constant operation constant`

### How could we implement this in Elm?
First let's model an expression like `5*(6 + 7)`. We call such complex expressions **computations**
``` 
type Computation
    = ReturnInteger Int
    | Add Computation Computation
    | Multiply Computation Computation
    | ...
```
so the expression becomes `Multiply (Return 5) (Add (Return 6) (Return 7))`.
We also need to model *inert* results of computations, which we call **values**. In our case these are just integers (later we could add strings, booleans, tuples of values etc)
```
type Value
    = IntegerValue Int
```
How can we model the stack of work like `(3 + _) :: _ * (6 + 7) :: nothing`? Let us first focus on the individual work items that contain blanks (**holes**) like `_ * (6 + 7)` or `(3 + _)`. Consider addition operation `x + y`. When looking at the description of the thought process the work that's to be done with addition is either `_ + computation` or `value + _`. We model it via
```
type StackElement
    = AddWithLeftHole Computation -- models `_ + computation`
    | AddWithRightHole Value -- models `value + _`
    | ... -- similarly for Multiply/Divide etc
```
Note that in general if we had an operation of e.g. 3 arguments `Op(x, y, z)` this would generate three new cases in `StackElement` as follows
```
type StackElement
    ...
    | OpWithHoleAt0 Computation Computation -- Op(_, comp1, comp2)
    | OpWithHoleAt1 Value Computation -- Op(val0, _, comp2)
    | OpWithHoleAt2 Value Value -- Op(val0, val1, _)
```
Now we can model the stack simply as a list
```
type alias Stack = List StackElement
```
The work stack `(3 + _) :: _ * (6 + 7) :: nothing` becomes ```[AddWithRightHole (IntegerValue 3), AddWithLeftHole (Add (ReturnInteger 6) (ReturnInteger 7))]```
The state of our thought process consists of the current computation and the work stack.
The current computation can be either an inert value after which we need to **(Remember work)** or a complex computation which we can simplify further. We model it with
```
type CurrentComputation
    = Value Value
    | Computation Computation
```
Then the state of our thought can be model with
```
type alias State =
    { stack : Stack
    , currentComputation : CurrentComputation
    }
```
The only thing that remains is the actual transformation of one state into the next state. We can model it with a function of type `State -> State`. If you're still here, I recommend trying to implement it on your own. First attempt can be something like
```
smallStepEval : State -> State
smallStepEval ({ stack, currentComputation} as state) =
    case currentComputation of
        Value val ->
            case stack of
                [] ->
                    -- Here we are in (value, no-work-to-be-done) case so what do we return?
                    Debug.todo  "we need to indicate that the computation has terminated, so maybe returning just `State` isn't such a great idea"
                work :: stack1 ->
                    -- Current computation is a value and we have work to do on the stack
                    -- Whatever the value/work, the work stack will get popped.
                    Debug.todo "remember or do actual work or something I don't know" 
        Computation comp ->
            -- Current computation is a complex computation, so we need to somehow decompose it into something smaller and push new work that should be done afterwards
            Debug.todo "simplify somehow"
```
From the `(value, no-work-to-be-done)` case we see that we might want to return more than just `State`. We could return `Either State ()` where `type Either a b = Left a | Right b`. So the result of one step either produces a new state `Left newState` or it terminates by resulting in `Right ()` (we could actually return the previous state, in which case we could model it with `{ isTerminal : Bool, state : State }`). Once we add more types such as booleans or we want to deal with division by zero, we might want to have a notion of a runtime error (`3 + True` should result in `ExpectedInteger` error). Then the resulting type of a one step computation could be `Result RunTimeError (Either State ())`. We won't bother with it here. Next attempt
```
smallStepEval : State -> Either State ()
smallStepEval ({ stack, currentComputation} as state) =
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
                        ...
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
                ...
```


### What have we accomplished?
This step-by-step interpreter with an explicit stack is an instance of a more general process. Let me illustrate it on a simpler computation. Recall the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number) `0, 1, 2, 3, 5, 8, 13, ...`
First two are `0, 1`, then each element is the sum of the previous two.
Consider the terribly inefficient recursive implementation of fibonacci 
```
fib : Int -> Int
fib x =
    if x < 2 then
        x
    else
        fib (x - 1) + fib (x - 2)
```
There are much more efficient implementations that use constant stack space,
but the point of example is that the functions calls itself recursively twice.
It will illustrate the call stack nicely.
Let us first look at what kind of a recursive process is generated for `x == 4`.
The angle braces denote the currently focused computation
```
<fib 4>
<fib 3> + fib 2
(<fib 2> + fib 1) + fib 2
((<fib 1> + fib 0) + fib 1) + fib 2
((<1> + fib 0) + fib 1) + fib 2
((1 + <fib 0>) + fib 1) + fib 2
((<1 + 0>) + fib 1) + fib 2
(<1> + fib 1) + fib 2
(1 + <fib 1>) + fib 2
(1 + <1>) + fib 2
(<1 + 1>) + fib 2
<2> + fib 2
2 + <fib 2>
2 + (<fib 1> + fib 0)
2 + (<1> + fib 0)
2 + (1 + <fib 0>)
2 + (1 + <0>)
2 + (<1 + 0>)
2 + <1>
<2 + 1>
<3>
```
How could we represent each line as data in Elm (we'll call it the State of the computation)?
We could model the state as a pair
of `(currently-focused-computation, stack-of-work)` e.g.
```
<fib 4>                              ~>  (Call 4, [])
<fib 3> + fib 2                      ~>  (Call 3, RememberToCall 2 :: [])
(<fib 2> + fib 1) + fib 2            ~>  (Call 2, RememberToCall 1 :: RememberToCall 2 :: [])
((<fib 1> + fib 0) + fib 1) + fib 2  ~>  (Call 1, RememberToCall 0 :: RememberToCall 1 :: RememberToCall 2 :: [])
((<1> + fib 0) + fib 1) + fib 2      ~>  (Done 1, RememberToCall 0 :: RememberToCall 1 :: RememberToCall 2 :: [])
((1 + <fib 0>) + fib 1) + fib 2      ~>  (Call 0, RememberToAdd  1 :: RememberToCall 1 :: RememberToCall 2 :: [])
((1 + <0>) + fib 1) + fib 2          ~>  (Done 0, RememberToAdd  1 :: RememberToCall 1 :: RememberToCall 2 :: [])
(<1> + fib 1) + fib 2                ~>  (Done 1, RememberToCall 1 :: RememberToCall 2 :: [])
(1 + <fib 1>) + fib 2                ~>  (Call 1, RememberToAdd  1 :: RememberToCall 2 :: [])
(1 + <1>) + fib 2                    ~>  (Done 1, RememberToAdd  1 :: RememberToCall 2 :: []
<2> + fib 2                          ~>  (Done 2, RememberToCall 2 :: [])
2 + <fib 2>                          ~>  (Call 2, RememberToAdd  2 :: [])
2 + (<fib 1> + fib 0)                ~>  (Call 1, RememberToCall 0 :: RememberToAdd  2 :: [])
2 + (<1> + fib 0)                    ~>  (Done 1, RememberToCall 0 :: RememberToAdd  2 :: [])
2 + (1 + <fib 0>)                    ~>  (Call 0, RememberToAdd  1 :: RememberToAdd  2 :: [])
2 + (1 + <0>)                        ~>  (Done 0, RememberToAdd  1 :: RememberToAdd  2 :: [])
2 + <1>                              ~>  (Done 1, RememberToAdd  2 :: [])
<3>                                  ~>  (Done 3, [])
```
Let's model the state in Elm as 

```
type alias State =
    ( CurrentComputation, Stack )

type CurrentComputation
    = Call Int
    | Done Int

type Work
    = RememberToCall Int
    | RememberToAdd Int

type alias Stack =
    List Work
```
Then we simply encode the transformation rules
```
(Call 0,  stack) ~> (Done 0, stack)
(Call 1,  stack) ~> (Done 1, stack)
(Call x,  stack) ~> (Call (x - 1), RememberToCall (x - 2) :: stack)
(Done y,  (RememberToCall x) :: stack) ~> (Call x, RememberToAdd y :: stack)
(Done y0, (RememberToAdd  x) :: stack) ~> (Done (y0 + y1), stack)
```
in Elm as follows
```
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
```

So we started with a recursive definition `fib` and transformed it into `fibIncrementalExplicit`
by designing a custom call stack just for `fib`.


When designing `smallStepEval` we were doing basically the same thing, but we applied the process to a
recursively defined evaluator of arithmetic expressions
```
eval : Computation -> Value
eval comp =
    case comp of
        ReturnInteger x ->
            IntegerValue x

        Add comp0 comp1 ->
            case ( eval comp0, eval comp1 ) of
                ( IntegerValue x, IntegerValue y ) ->
                    IntegerValue (x + y)
                ...
        ...
            
```

# Part 2: Can we extend our language with Variables? - Environments

# Part 3: How do first class functions work? - Closures

# Part 4: Can we treat our stacks as just another value in our language? - Continuations

