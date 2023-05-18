module Queue exposing (..)


type Queue a
    = Empty
    | Queue (QueueData a)


type alias QueueData a =
    { head : a
    , tail : a
    , queue : List a
    }


dequeue : Queue a -> Queue a
dequeue queue =
    case queue of
        Empty ->
            queue

        Queue queueData ->
            Queue queueData
