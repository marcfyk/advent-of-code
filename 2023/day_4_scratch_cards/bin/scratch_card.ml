open Base
module IntSet = Set.M (Int)

type t = { id : int; winning : IntSet.t; actual : IntSet.t }

let init id winning actual = { id; winning; actual }
let matching_count t = Set.count t.actual ~f:(Set.mem t.winning)
let id t = t.id

let points t =
  let count = matching_count t in
  if count = 0 then 0 else 2 ** (count - 1)

let ids_won t =
  let count = matching_count t in
  List.init count ~f:(fun i -> i + t.id + 1)
