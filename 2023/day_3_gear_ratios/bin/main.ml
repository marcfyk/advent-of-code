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

let sum_of_parts (parts : Game.Part.t list) =
  List.fold parts ~init:0 ~f:(fun sum part ->
      let part_sum = List.fold part.adjacent_numbers ~init:0 ~f:( + ) in
      sum + part_sum)

let sum_of_gear_ratios (parts : Game.Part.t list) =
  List.fold parts ~init:0 ~f:(fun sum part ->
      match Game.Part.gear_ratio part with
      | None -> sum
      | Some ratio -> sum + ratio)

let () =
  match parse_args () with
  | Error err -> Stdio.print_endline err
  | Ok f -> (
      let lines = Stdio.In_channel.read_lines f |> List.to_array in
      match Game.init lines with
      | Error err -> Stdio.print_endline err
      | Ok game ->
          let parts = Game.get_parts game in
          let part_one = sum_of_parts parts in
          let part_two = sum_of_gear_ratios parts in
          Stdio.printf "part_one = %d\npart_two = %d\n" part_one part_two)
