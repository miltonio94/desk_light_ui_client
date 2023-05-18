module Queue exposing (Queue, dequeu, enqueue, isEmpty, length, peek)


type Queue a
    = Empty
    | Queue (QueueData a)


type alias QueueData a =
    { head : a
    , tail : a
    , queue : List a
    }


isEmpty : Queue a -> Bool
isEmpty queue =
    case queue of
        Empty ->
            True

        _ ->
            False


dequeu : Queue a -> Queue a
dequeu queue =
    case queue of
        Empty ->
            queue

        Queue queueData ->
            case ( headFromQueuDatalist queueData, tailFromQueueDataList queueData ) of
                ( Just head, Just bodyAndTail ) ->
                    Queue
                        { queueData
                            | queue = bodyAndTail
                            , head = head
                        }

                _ ->
                    Empty


enqueue : a -> Queue a -> Queue a
enqueue item queue =
    case queue of
        Empty ->
            Queue
                { head = item
                , tail = item
                , queue = [ item ]
                }

        Queue queueData ->
            Queue
                { queueData
                    | tail = item
                    , queue = queueData.queue ++ [ item ]
                }


length : Queue a -> Int
length queue =
    case queue of
        Empty ->
            0

        Queue queueData ->
            List.length queueData.queue


peek : Queue a -> Maybe a
peek queue =
    case queue of
        Empty ->
            Nothing

        Queue queueData ->
            Just queueData.head


tailFromQueueDataList : QueueData a -> Maybe (List a)
tailFromQueueDataList =
    .queue
        >> List.tail


headFromQueuDatalist : QueueData a -> Maybe a
headFromQueuDatalist =
    .queue >> List.head


empty =
    Empty
