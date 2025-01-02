open Base

type match_policy =
  | Numeric
  | NumericAndSpelling

type search_order =
  | Forward
  | Reverse

let digits =
  [ "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
;;

let reverse_digits = List.map digits ~f:String.rev

let digit_spellings = function
  | Forward -> digits
  | Reverse -> reverse_digits
;;

let parse_args () =
  let args = Sys.get_argv () in
  match Array.length args with
  | length when length < 2 -> Error "no input file given"
  | _ ->
    let input_file = args.(1) in
    if not @@ Stdlib.Sys.file_exists input_file
    then Error "file does not exist"
    else if Stdlib.Sys.is_directory input_file
    then Error "file is a directory"
    else Ok input_file
;;

let match_digit char =
  if Char.is_digit char
  then (
    let ascii = Char.to_int char in
    let offset = Char.to_int '0' in
    Some (ascii - offset))
  else None
;;

let match_single_spelling spelling index line =
  let spelling_length = String.length spelling in
  let in_range = index >= 0 && index + spelling_length - 1 < String.length line in
  match in_range with
  | false -> false
  | true ->
    List.init spelling_length ~f:(fun i -> Char.equal line.[index + i] spelling.[i])
    |> List.for_all ~f:Fn.id
;;

let match_any_spelling spellings index line =
  List.find_mapi spellings ~f:(fun value spelling ->
    if match_single_spelling spelling index line then Some value else None)
;;

let get_number policy order line =
  let spellings = digit_spellings order in
  let match_number =
    match policy with
    | Numeric -> fun index -> match_digit line.[index]
    | NumericAndSpelling ->
      fun index ->
        (match match_digit line.[index] with
         | Some _ as result -> result
         | None -> match_any_spelling spellings index line)
  in
  Sequence.init (String.length line) ~f:Fn.id |> Sequence.find_map ~f:match_number
;;

let get_calibration_value policy line =
  match get_number policy Forward line with
  | None -> None
  | Some first ->
    (match get_number policy Reverse (String.rev line) with
     | None -> None
     | Some last -> Some ((first * 10) + last))
;;

let calibration_sum policy lines =
  let f acc line =
    get_calibration_value policy line |> Option.value ~default:0 |> ( + ) acc
  in
  List.fold lines ~init:0 ~f
;;

let () =
  match parse_args () with
  | Error err ->
    Stdio.print_endline err;
    Stdlib.exit 1
  | Ok f ->
    let lines = Stdio.In_channel.read_lines f in
    let part_one = calibration_sum Numeric lines in
    let part_two = calibration_sum NumericAndSpelling lines in
    Stdlib.Format.printf "part one = %d\npart two = %d\n" part_one part_two
;;
