open Base

let parse_args () =
  let args = Sys.get_argv () in
  match Array.length args with
  | length when length < 2 -> Error "no input file given"
  | _ ->
      let input_file = args.(1) in
      if not @@ Stdlib.Sys.file_exists input_file then
        Error "file does not exist"
      else if Stdlib.Sys.is_directory input_file then
        Error "file is a directory"
      else Ok input_file

let () =
  match parse_args () with
  | Error err -> Stdio.print_endline err
  | Ok f -> (
      let lines = Stdio.In_channel.read_lines f |> List.to_array in
      let game = Game.init lines in
      let total = Result.map game ~f:Game.get_total_numbers in
      match total with
      | Ok n -> Stdio.printf "%d\n" n
      | Error err -> Stdio.print_endline err)
