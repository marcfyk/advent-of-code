open Base

module Color = struct
  type t = R | G | B

  let to_string = function R -> "red" | G -> "green" | B -> "blue"
  let all_colors = [| R; G; B |]
end

type counts = Counts of { r : int; g : int; b : int }
type t = { id : int; handfuls : counts list }

let init id handfuls = { id; handfuls }

let minimum_counts_required { id = _; handfuls } =
  List.reduce handfuls ~f:(fun (Counts acc) (Counts c) ->
      Counts { r = max acc.r c.r; g = max acc.g c.g; b = max acc.b c.b })
  |> Option.value ~default:(Counts { r = 0; g = 0; b = 0 })

let power (Counts { r; g; b }) = r * g * b
