module SimpleTree (Elmt_: Pfds_intf.ORDERED)
	: Cnt.GENTREE with module Elmt = Elmt_ =
struct
	module Elmt = Elmt_
	type 'a tree = Tree of (Elmt.t * Elmt.t tree * Elmt.t tree) | Leaf

	let empty = Leaf
	let is_empty t = t = Leaf
	let rec insert t e = match t with
		| Leaf -> Tree (e, Leaf, Leaf)
		| Tree (e', left, right) ->
			let cmp = Elmt.compare e e' in
			if cmp < 0 then Tree (e', insert left e, right) else
			if cmp > 0 then Tree (e', left, insert right e)
			else t
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
			let cmp = Elmt.compare e e' in
			if cmp < 0 then Tree (e', remove left e, right) else
			if cmp > 0 then Tree (e', left, remove right e)
			else merge left right
	let rec min = function
		| Leaf -> raise Not_found
		| Tree (e, Leaf, _) -> e
		| Tree (_, left, _) -> min left
	let find_before t e =
		let best = ref None in
		let rec aux = function
			| Leaf -> ()
			| Tree (e', left, right) ->
				let cmp = Elmt.compare e e' in
				if cmp > 0 then (
					best := Some e' ;
					aux right
				) else aux left in
		aux t ;
		match !best with
			| None -> raise Not_found
			| Some x -> x
end
