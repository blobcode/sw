let global_env = ref []

let rec repl () =
  match LNoise.linenoise "> " with
  | None -> ()
  | Some line ->
      if String.trim line <> "" then (
        LNoise.history_add line |> ignore;
        try
          let res, new_env = Sw.Main.run_with_env !global_env line in
          global_env := new_env;
          print_endline res
        with Failure msg -> print_endline ("Error: " ^ msg)
      );
      repl ()

let () = 
  LNoise.history_set ~max_length:100 |> ignore;
  repl ()
