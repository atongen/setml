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
    version = "0.1.0";
    build_time = "2018-12-02 18:53:01";
    build_hash = "3d27495";
    ocaml_version = "4.06.1";
    bug_reports = "https://github.com/atongen/setml";
}
