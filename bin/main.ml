let global_env = ref []

let run_file filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let res, new_env = Sw.Main.run_with_env !global_env content in
    global_env := new_env;
    print_endline res
  with
  | Sys_error msg -> print_endline ("File error: " ^ msg)
  | Failure msg -> print_endline ("Error: " ^ msg)

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
  if Array.length Sys.argv > 1 then (
    run_file Sys.argv.(1)
  ) else (
    print_endline "swi";
    repl ()
  )
