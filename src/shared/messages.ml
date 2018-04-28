open Shared_util

(* START HERE!
    - change `present` -> `presence` throughout
    -
 *)

type presence_t = {
    game_id: int;
    player_id: int;
    player_name: string;
    value: bool;
}

type t =
    | Presence of presence_t

let make_presence game_id player_id player_name value =
    let pt = {
        game_id;
        player_id;
        player_name;
        value;
    } in
    Presence pt

module type CONVERT = sig
    (* ... *)
end
