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

let sum_of_valid_games games =
  let bag = Game.Counts { r = 12; g = 13; b = 14 } in
  List.fold games ~init:0 ~f:(fun sum game ->
      if Game.is_valid_with_handful game bag then sum + game.id else sum)

let sum_of_game_powers games =
  List.fold games ~init:0 ~f:(fun sum game ->
      let power = Game.power @@ Game.minimum_counts_required game in
      sum + power)

let () =
  match parse_args () with
  | Error err ->
      Stdio.print_endline err;
      Stdlib.exit 1
  | Ok f -> (
      let input = Stdio.In_channel.read_all f in
      let parser = Parser.init input in
      let _, games = Parser.parse_input parser in
      match games with
      | Error err ->
          Stdio.print_endline err;
          Stdlib.exit 1
      | Ok games ->
          let part_one = sum_of_valid_games games in
          let part_two = sum_of_game_powers games in
          Stdlib.Format.printf "part one = %d\npart two = %d\n" part_one
            part_two)
