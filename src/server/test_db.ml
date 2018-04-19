open Lib
open Lwt.Infix

let or_fail = Caqti_lwt.or_fail

let db_create_game_test db =
  Db.delete_all db >>= or_fail >>= function () ->
  Db.create_game db >>= or_fail >>= function game_id ->
  Db.game_exists db game_id >>= or_fail >>= fun game_exists ->
  Lwt.return (assert game_exists)

let _ = Lwt_main.run (begin
  Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test") >>= or_fail >>= fun db ->
  db_create_game_test db
end)
