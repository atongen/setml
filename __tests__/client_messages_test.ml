open Jest

let _ =

describe "ClientMessages" (fun () ->
  let open Messages in
  let open ClientMessages in

  test "toBe" (fun () ->
    expect (1 + 2) |> toBe 3);
)
