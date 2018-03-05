module Backend = struct
    include Session.Lift.IO(Lwt)(Session.Memory)
    let create () = Session.Memory.create ()
end
include Session_cohttp_lwt.Make(Backend)