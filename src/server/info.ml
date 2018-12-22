type t = {
    version: string;
    build_time: string;
    build_hash: string;
    ocaml_version: string;
    bug_reports: string;
}

let to_string i = Printf.sprintf "setml %s %s %s ocaml %s"
    i.version i.build_time i.build_hash i.ocaml_version

let get () = {
    version = "unset";
    build_time = "unset";
    build_hash = "unset";
    ocaml_version = "unset";
    bug_reports = "unset";
}
