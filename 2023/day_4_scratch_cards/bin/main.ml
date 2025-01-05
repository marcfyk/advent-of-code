open Base
open Stdio

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

let sum_of_points scratch_cards =
  List.fold scratch_cards ~init:0 ~f:(fun sum s -> sum + Scratch_card.points s)

let process_scratch_cards (scratch_cards : Scratch_card.t list) =
  let ids_collected = Map.empty (module Int) in
  let ids_collected =
    List.fold scratch_cards ~init:ids_collected ~f:(fun collected card ->
        let collected =
          Map.update collected card.id ~f:(function
            | Some i -> i + 1
            | None -> 1)
        in
        let count = Option.value (Map.find collected card.id) ~default:1 in
        let ids = Scratch_card.ids_won card in
        List.fold ids ~init:collected
          ~f:(Map.update ~f:(function Some i -> i + count | None -> count)))
  in
  Map.fold ids_collected ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data)

let () =
  match parse_args () with
  | Error err ->
      print_endline err;
      Stdlib.exit 1
  | Ok f -> (
      let data = Stdio.In_channel.read_all f in
      let p = Parser.init data in
      match Parser.parse_all_scratch_cards p with
      | Error err -> print_endline err
      | Ok scratch_cards ->
          let part_one = sum_of_points scratch_cards in
          let part_two = process_scratch_cards scratch_cards in
          printf "part_one = %d\npart_two = %d\n" part_one part_two)
