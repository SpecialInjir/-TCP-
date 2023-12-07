type Event =
    | APP_PASSIVE_OPEN
    | APP_ACTIVE_OPEN
    | APP_SEND
    | APP_CLOSE
    | APP_TIMEOUT
    | RCV_SYN
    | RCV_ACK
    | RCV_SYN_ACK
    | RCV_FIN
    | RCV_FIN_ACK

type State =
    | CLOSED
    | LISTEN
    | SYN_SENT
    | SYN_RCVD
    | ESTABLISHED
    | CLOSE_WAIT
    | LAST_ACK
    | FIN_WAIT_1
    | FIN_WAIT_2
    | CLOSING
    | TIME_WAIT

let transition (state: State) (event: Event) =
    match state, event with
    | CLOSED, APP_PASSIVE_OPEN -> LISTEN
    | CLOSED, APP_ACTIVE_OPEN  -> SYN_SENT
    | LISTEN, RCV_SYN          -> SYN_RCVD
    | LISTEN, APP_SEND         -> SYN_SENT
    | LISTEN, APP_CLOSE        -> CLOSED
    | SYN_RCVD, APP_CLOSE      -> FIN_WAIT_1
    | SYN_RCVD, RCV_ACK        -> ESTABLISHED
    | SYN_SENT, RCV_SYN        -> SYN_RCVD
    | SYN_SENT, RCV_SYN_ACK    -> ESTABLISHED
    | SYN_SENT, APP_CLOSE      -> CLOSED
    | ESTABLISHED, APP_CLOSE   -> FIN_WAIT_1
    | ESTABLISHED, RCV_FIN     -> CLOSE_WAIT
    | FIN_WAIT_1, RCV_FIN      -> CLOSING
    | FIN_WAIT_1, RCV_FIN_ACK  -> TIME_WAIT
    | FIN_WAIT_1, RCV_ACK      -> FIN_WAIT_2
    | CLOSING, RCV_ACK         -> TIME_WAIT
    | FIN_WAIT_2, RCV_FIN      -> TIME_WAIT
    | TIME_WAIT, APP_TIMEOUT   -> CLOSED
    | CLOSE_WAIT, APP_CLOSE    -> LAST_ACK
    | LAST_ACK, RCV_ACK        -> CLOSED
    | _ -> failwith "ERROR"

let rec processEvents (state: State) (events: Event list) =
    match events with
    | [] -> state
    | event::rest ->
            let nextState = transition state event
            processEvents nextState rest

[<EntryPoint>]
let main argv =
    printfn "Введите события (разделенные пробелом, например: APP_PASSIVE_OPEN APP_SEND RCV_SYN_ACK):"
    let input = System.Console.ReadLine()
    let events = input.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun s -> match s with 
                                          | "APP_PASSIVE_OPEN" -> APP_PASSIVE_OPEN 
                                          | "APP_ACTIVE_OPEN" -> APP_ACTIVE_OPEN 
                                          | "APP_SEND" -> APP_SEND 
                                          | "APP_CLOSE" -> APP_CLOSE 
                                          | "APP_TIMEOUT" -> APP_TIMEOUT 
                                          | "RCV_SYN" -> RCV_SYN 
                                          | "RCV_ACK" -> RCV_ACK 
                                          | "RCV_SYN_ACK" -> RCV_SYN_ACK 
                                          | "RCV_FIN" -> RCV_FIN 
                                          | "RCV_FIN_ACK" -> RCV_FIN_ACK 
                                          | _ -> failwith "Invalid event")
                    |> Array.toList

    let initialState = CLOSED
    let finalState = processEvents initialState events

    printfn "Финальноее состояние: %A" finalState

    0

