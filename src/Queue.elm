module Queue exposing (Queue, dequeue, empty, enqueue, fromList, isEmpty, singleton, toList)


type Queue a
    = Queue (List a) (List a)


empty : Queue a
empty =
    Queue [] []


isEmpty : Queue a -> Bool
isEmpty (Queue xs ys) =
    xs == [] && ys == []


singleton : a -> Queue a
singleton x =
    Queue [] [ x ]


enqueue : a -> Queue a -> Queue a
enqueue x (Queue back front) =
    Queue (x :: back) front


dequeue : Queue a -> Maybe ( a, Queue a )
dequeue (Queue back front0) =
    case front0 of
        [] ->
            let
                rev0 : List a
                rev0 =
                    List.reverse back
            in
            case rev0 of
                [] ->
                    Nothing

                x :: rev1 ->
                    Just ( x, Queue [] rev1 )

        x :: front1 ->
            Just ( x, Queue back front1 )


toList : Queue a -> List a
toList q0 =
    case dequeue q0 of
        Nothing ->
            []

        Just ( x, q1 ) ->
            x :: toList q1


fromList : List a -> Queue a
fromList xs =
    Queue [] xs


map : (a -> b) -> Queue a -> Queue b
map f (Queue back front) =
    Queue (List.map f back) (List.map f front)
