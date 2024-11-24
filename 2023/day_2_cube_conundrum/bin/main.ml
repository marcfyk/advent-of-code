open Base

module Color = struct
  type t = R | G | B

  let to_string = function R -> "red" | G -> "green" | B -> "blue"
  let all_colors = [| R; G; B |]
end

module Game = struct
  type counts = Counts of { r : int; g : int; b : int }
  type t = { id : int; handfuls : counts list }

  let init id handfuls = { id; handfuls }

  let minimum_counts_required { id = _; handfuls } =
    List.reduce handfuls ~f:(fun (Counts acc) (Counts c) ->
        Counts { r = max acc.r c.r; g = max acc.g c.g; b = max acc.b c.b })
    |> Option.value ~default:(Counts { r = 0; g = 0; b = 0 })

  let power (Counts { r; g; b }) = r * g * b
end

module Parser = struct
  type t = { index : int; data : string }

  let init data = { index = 0; data }

  let peek t offset =
    let target = t.index + offset in
    if target < 0 || target >= String.length t.data then None
    else Some t.data.[target]

  let read t offset =
    let target = t.index + offset in
    { t with index = target }

  let rec consume_whitespace t =
    match peek t 0 with Some ' ' -> consume_whitespace (read t 1) | _ -> t

  let parse_number t =
    let rec parse parser number =
      match peek parser 0 with
      | None -> (parser, number)
      | Some c when not @@ Char.is_digit c -> (parser, number)
      | Some d ->
          let digit = Char.to_int d - Char.to_int '0' in
          parse (read parser 1) ((number * 10) + digit)
    in
    let p, n = parse t 0 in
    if p.index = t.index then (p, None) else (p, Some n)

  let parse_char t char =
    match peek t 0 with
    | None -> (t, false)
    | Some c when Char.equal c char -> (read t 1, true)
    | Some _ -> (t, false)

  let parse_string t string =
    let length = String.length t.data in
    let str_length = String.length string in
    if length - t.index < str_length then (t, false)
    else
      let ok =
        Sequence.init str_length ~f:(fun i ->
            Char.equal t.data.[t.index + i] string.[i])
        |> Sequence.for_all ~f:Fn.id
      in
      if ok then ({ index = t.index + str_length; data = t.data }, true)
      else (t, false)

  let parse_game_id t =
    match parse_string t "Game" with
    | parser, false ->
        (parser, Error "expected \"Game <id>:\" where id is numeric")
    | parser, true -> (
        let parser = consume_whitespace parser in
        match parse_number parser with
        | parser, None ->
            (parser, Error "expected \"Game <id>:\" where id is numeric")
        | parser, Some id -> (
            match parse_char parser ':' with
            | parser, false ->
                (parser, Error "expected \"Game <id>:\" where id is numeric")
            | parser, true -> (parser, Ok id)))

  let parse_color t =
    Array.map ~f:(fun c -> (c, Color.to_string c)) Color.all_colors
    |> Array.fold_until ~init:(t, None)
         ~f:(fun (parser, _) (color, name) ->
           match parse_string parser name with
           | parser, true -> Stop (parser, Some color)
           | parser, false -> Continue (parser, None))
         ~finish:Fn.id

  let parse_game_color_count t =
    let parser = consume_whitespace t in
    match peek parser 0 with
    | None -> (parser, Ok None)
    | Some _ -> (
        match parse_number parser with
        | parser, None -> (parser, Ok None)
        | parser, Some count -> (
            let parser = consume_whitespace parser in
            match parse_color parser with
            | parser, None -> (parser, Error "expected valid color")
            | parser, Some color -> (parser, Ok (Some (color, count)))))

  let parse_game_cube_set t =
    let rec parse parser (Game.Counts counts as game_counts) =
      match parse_game_color_count parser with
      | (_, Error _) as parse_result -> parse_result
      | parser, Ok None -> (parser, Ok game_counts)
      | parser, Ok (Some (color, count)) -> (
          let updated_set =
            match color with
            | R -> Game.Counts { counts with r = counts.r + count }
            | G -> Game.Counts { counts with g = counts.g + count }
            | B -> Game.Counts { counts with b = counts.b + count }
          in
          match peek parser 0 with
          | None | Some ';' | Some '\n' -> (parser, Ok updated_set)
          | Some ',' -> parse (read parser 1) updated_set
          | Some _ ->
              ( parser,
                Error
                  "expected ',' to delimit colors and their counts or ';' to \
                   delimit sets of games" ))
    in
    parse t (Game.Counts { r = 0; b = 0; g = 0 })

  let parse_game_cube_sets t =
    let rec parse parser sets =
      match parse_game_cube_set parser with
      | (_, Error _) as result -> result
      | parser, Ok set -> (
          let parser = consume_whitespace parser in
          let updated_sets = set :: sets in
          match peek parser 0 with
          | None -> (parser, Ok updated_sets)
          | Some ';' -> parse (read parser 1) updated_sets
          | Some '\n' -> (read parser 1, Ok updated_sets)
          | Some _ -> parse parser updated_sets)
    in
    parse t []

  let parse_game t =
    let parser = consume_whitespace t in
    match parse_game_id parser with
    | (_, Error _) as parser_result -> parser_result
    | parser, Ok id -> (
        let parser = consume_whitespace parser in
        match parse_game_cube_sets parser with
        | (_, Error _) as parser_result -> parser_result
        | parser, Ok handfuls -> (parser, Ok (Game.init id handfuls)))

  let parse_input t =
    let rec parse parser games =
      match peek parser 0 with
      | None -> (parser, Ok games)
      | Some _ -> (
          match parse_game parser with
          | parser, Error err -> (parser, Error err)
          | parser, Ok game ->
              let parser = consume_whitespace parser in
              parse parser (game :: games))
    in
    parse t []
end

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
          let score =
            List.fold games ~init:0 ~f:(fun sum game ->
                sum + (Game.power @@ Game.minimum_counts_required game))
          in
          Stdlib.Format.printf "%d\n" score)
