open Base

type digit =
  { spelling : string
  ; reverse_spelling : string
  }

let new_digit spelling = { spelling; reverse_spelling = String.rev spelling }
let zero = new_digit "zero"
let one = new_digit "one"
let two = new_digit "two"
let three = new_digit "three"
let four = new_digit "four"
let five = new_digit "five"
let six = new_digit "six"
let seven = new_digit "seven"
let eight = new_digit "eight"
let nine = new_digit "nine"
let all_digits = [| zero; one; two; three; four; five; six; seven; eight; nine |]

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

let does_prefix_match index string prefix =
  let str_length = String.length string in
  let prefix_length = String.length prefix in
  if str_length - index < prefix_length
  then false
  else (
    let str_seq =
      Sequence.init prefix_length ~f:(fun i -> String.get string (index + i))
    in
    let prefix_seq = String.to_sequence prefix in
    Sequence.zip str_seq prefix_seq
    |> Sequence.for_all ~f:(fun (c1, c2) -> Char.equal c1 c2))
;;

let does_suffix_match index string suffix =
  let suffix_length = String.length suffix in
  if index + 1 < suffix_length
  then false
  else (
    let str_seq =
      Sequence.init suffix_length ~f:(fun i -> String.get string (index - i))
    in
    let suffix_seq = String.to_sequence suffix in
    Sequence.zip str_seq suffix_seq
    |> Sequence.for_all ~f:(fun (c1, c2) -> Char.equal c1 c2))
;;

let get_first_number line =
  let rec find index =
    if index >= String.length line
    then None
    else if Char.is_digit line.[index]
    then Some (Char.to_int line.[index] - Char.to_int '0')
    else (
      let check value digit =
        if does_prefix_match index line digit.spelling then Some value else None
      in
      match Array.find_mapi all_digits ~f:check with
      | Some value -> Some value
      | None -> find (index + 1))
  in
  find 0
;;

let get_last_number line =
  let rec find index =
    if index < 0
    then None
    else if Char.is_digit line.[index]
    then Some (Char.to_int line.[index] - Char.to_int '0')
    else (
      let check value digit =
        if does_suffix_match index line digit.reverse_spelling then Some value else None
      in
      match Array.find_mapi all_digits ~f:check with
      | Some value -> Some value
      | None -> find (index - 1))
  in
  find (String.length line - 1)
;;

let get_calibration_value line =
  match get_first_number line with
  | None -> None
  | Some first ->
    (match get_last_number line with
     | None -> None
     | Some last -> Some ((first * 10) + last))
;;

let () =
  match parse_args () with
  | Error err ->
    Stdio.print_endline err;
    Stdlib.exit 1
  | Ok f ->
    let lines = Stdio.In_channel.read_lines f in
    let result =
      List.fold lines ~init:0 ~f:(fun acc line ->
        get_calibration_value line |> Option.value ~default:0 |> ( + ) acc)
    in
    Stdlib.Format.printf "%d\n" result
;;
