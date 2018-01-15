module SimpleTree (Elmt : Pfds_intf.ORDERED)
  : Cnt.GENTREE with module Elmt = Elmt =
struct
  module Elmt = Elmt
  type 'a tree = Tree of (Elmt.t * Elmt.t tree * Elmt.t tree) | Leaf

  let empty = Leaf

  let is_empty t = t = Leaf

  let rec insert t e = match t with
    | Leaf -> Tree (e, Leaf, Leaf)
    | Tree (e', left, right) ->
      let cmp = Elmt.compare e e' in
      if cmp <= 0 then Tree (e', insert left e, right) else
                       Tree (e', left, insert right e)

  let rec iter t f = match t with
    | Leaf -> ()
    | Tree (e, left, right) -> iter left f ; f e ; iter right f

  let merge t1 t2 =
    let res = ref t1 in
    iter t2 (fun e -> res := insert !res e) ;
    !res

  let rec remove t e = match t with
    | Leaf -> raise Not_found
    | Tree (e', left, right) ->
      if e' == e then merge left right else
      let cmp = Elmt.compare e e' in
      if cmp <= 0 then Tree (e', remove left e, right) else
                       Tree (e', left, remove right e)

  let rec min = function
    | Leaf -> raise Not_found
    | Tree (e, Leaf, _) -> e
    | Tree (_, left, _) -> min left

  let find_before t e =
    let rec aux best = function
      | Leaf -> best
      | Tree (e', left, right) ->
        let cmp = Elmt.compare e e' in
        if cmp >= 0 then
          aux (Some e') right
        else aux best left
    in
    match aux None t with
      | None -> raise Not_found
      | Some x -> x
end
