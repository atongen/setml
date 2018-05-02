open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_util

let pubsub_tests pubsub =
    let check (query, expChan, expMsg) =
        (*
            create game
            listen on game channel
            execute query
        *)
        let i = Pubsub.get_notifications pubsub (fun pubsub channel pid payload ->
            ae expChan channel;
            let gotMsg = Server_message_converter.of_json payload in
            ae expMsg gotMsg ~printer:Messages.to_string
        )
        ae 1 i ~printer:string_of_int
        (*
            unlisten on game channel
        *)
    in
    cases_of check [];


let suite =
    let clients = Clients.make () in
    let pubsub = Pubsub.make "user=atongen password=at1234 port=5435 host=localhost dbname=setml_development" clients in
    [
        "pubsub" >::: pubsub_tests pubsub
    ]
