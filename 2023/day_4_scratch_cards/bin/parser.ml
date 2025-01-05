open Base

type t = { index : int; data : string }

let init data = { index = 0; data }

let peek t index =
  if index < 0 || index >= String.length t.data then None
  else Some t.data.[index]

let read t index = { t with index }
let current_char t = peek t t.index

let read_whitespace t =
  let rec scan index =
    match peek t index with Some ' ' -> scan (index + 1) | _ -> index
  in
  read t (scan t.index)

let read_number t =
  let rec scan number index =
    match peek t index with
    | Some c when Char.is_digit c ->
        scan ((number * 10) + Char.to_int c - Char.to_int '0') (index + 1)
    | _ -> (number, index)
  in
  let n, new_index = scan 0 t.index in
  if new_index = t.index then None else Some (n, read t new_index)

let expect_number t =
  match read_number t with
  | None -> Error (Printf.sprintf "expected a number")
  | Some (n, t) -> Ok (n, t)

let expect_char t ~char =
  match current_char t with
  | Some c when Char.equal c char -> Ok (read t (t.index + 1))
  | _ -> Error (Printf.sprintf "expected %c" char)

let scan_word t ~word =
  String.fold_result word ~init:t.index ~f:(fun i w ->
      match peek t i with
      | Some c when Char.equal c w -> Ok (i + 1)
      | _ -> Error ())
  |> Result.ok

let expect_word t ~word =
  match scan_word t ~word with
  | Some i -> Ok (read t i)
  | _ -> Error (Printf.sprintf "expected %s" word)

let expect_numbers_until t ~f =
  let rec parse t numbers =
    let t = read_whitespace t in
    match current_char t with
    | Some c when f c -> Ok (numbers, t)
    | None -> Ok (numbers, t)
    | _ -> (
        match expect_number t with
        | Ok (n, t) -> parse t (n :: numbers)
        | Error err -> Error err)
  in
  parse t []

let expect_scratch_card t =
  let parse_result = expect_word t ~word:"Card" in
  let parse_result = Result.map parse_result ~f:read_whitespace in
  match Result.bind parse_result ~f:expect_number with
  | Error err -> Error err
  | Ok (id, t) -> (
      let parse_result = expect_char t ~char:':' in
      let parse_result =
        Result.bind parse_result ~f:(expect_numbers_until ~f:(Char.equal '|'))
      in
      match parse_result with
      | Error err -> Error err
      | Ok (winning, t) -> (
          let parse_result = expect_char t ~char:'|' in
          let parse_result =
            Result.bind parse_result
              ~f:(expect_numbers_until ~f:(Char.equal '\n'))
          in
          match parse_result with
          | Error err -> Error err
          | Ok (actual, t) ->
              let t = read_whitespace t in
              let t =
                match peek t (t.index + 1) with
                | Some '\n' -> read t (t.index + 1)
                | _ -> t
              in
              let winning = Set.of_list (module Int) winning in
              let actual = Set.of_list (module Int) actual in
              Ok (Scratch_card.init id winning actual, read t (t.index + 1))))

let parse_scratch_card t =
  if t.index < 0 || t.index >= String.length t.data then None
  else Some (expect_scratch_card t)

let parse_all_scratch_cards t =
  let rec loop t =
    match parse_scratch_card t with
    | None -> []
    | Some (Error err) -> [ Error err ]
    | Some (Ok (card, t)) -> Ok card :: loop t
  in
  match loop t |> List.partition_result with
  | _, [ err ] -> Error err
  | oks, _ -> Ok oks
