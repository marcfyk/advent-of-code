open Base

type t = { schematics : string array; visited : bool array }

let init schematics =
  let rows = Array.length schematics in
  if rows <= 0 then Error "empty input"
  else
    let columns = String.length schematics.(0) in
    if columns <= 0 then Error "empty line"
    else
      let visited = Array.create ~len:(rows * columns) false in
      Ok { schematics; visited }

let rows t = Array.length t.schematics
let columns t = String.length t.schematics.(0)

let is_valid_coordinates t x y =
  let is_valid_x = 0 <= x && x < rows t in
  let is_valid_y = 0 <= y && y < columns t in
  is_valid_x && is_valid_y

let data_coordinates_to_visited_coordinates t x y = (x * columns t) + y
let get t x y = t.schematics.(x).[y]
let is_visited t x y = t.visited.(data_coordinates_to_visited_coordinates t x y)
let gear_ratio first_part second_part = first_part * second_part

let visit_number t x y length =
  Sequence.range ~start:`inclusive ~stop:`exclusive
    (data_coordinates_to_visited_coordinates t x y)
    (data_coordinates_to_visited_coordinates t x (y + length))
  |> Sequence.iter ~f:(fun i -> t.visited.(i) <- true)

let adjacent_coordinates t x y =
  let offsets = [ -1; 0; 1 ] in
  List.bind offsets ~f:(fun x -> List.map offsets ~f:(fun y -> (x, y)))
  |> List.filter_map ~f:(fun (x', y') ->
         match (x + x', y + y') with
         | invalid_x, _ when invalid_x < 0 || invalid_x >= rows t -> None
         | _, invalid_y when invalid_y < 0 || invalid_y >= columns t -> None
         | curr_x, curr_y when curr_x = x && curr_y = y -> None
         | valid_coordinates -> Some valid_coordinates)

let scan_number t x y length =
  let rec scan offset number =
    if offset >= length then number
    else
      let c = get t x (y + offset) in
      let d = Char.to_int c - Char.to_int '0' in
      scan (offset + 1) ((number * 10) + d)
  in
  scan 0 0

let expand_number t x y =
  let rec expand next offset length =
    let new_y = y + offset in
    if new_y < 0 || new_y >= columns t then length
    else if Char.is_digit (get t x new_y) then
      expand next (next offset) (length + 1)
    else length
  in
  let left = expand (fun x -> x - 1) 0 0 in
  let right = expand (( + ) 1) 0 0 in
  let start_index = y - left + 1 in
  let total_length = left + right - 1 in
  let number = scan_number t x start_index total_length in
  visit_number t x start_index total_length;
  number

let get_adjacent_numbers t x y =
  adjacent_coordinates t x y
  |> List.filter_map ~f:(function
       | x, y when is_visited t x y -> None
       | x, y when not @@ Char.is_digit (get t x y) -> None
       | x, y -> Some (expand_number t x y))

let get_gear_ratio t symbol_x symbol_y =
  match get_adjacent_numbers t symbol_x symbol_y with
  | [ first; second ] -> Some (gear_ratio first second)
  | _ -> None

let get_total_numbers t =
  Array.foldi t.schematics ~init:0 ~f:(fun i acc s ->
      let sum =
        String.foldi s ~init:0 ~f:(fun j acc c ->
            match c with
            | '*' ->
                let ratio = Option.value ~default:0 (get_gear_ratio t i j) in
                acc + ratio
            | _ -> acc)
      in
      acc + sum)
