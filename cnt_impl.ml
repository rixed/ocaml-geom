module GenMap (Ord : Map.OrderedType) : Cnt.GENMAP =
struct
	module M = Map.Make(Ord)
	type key = M.key
	type 'a t = 'a M.t
	let empty = M.empty
	let is_empty = M.is_empty
	let add m k e = M.add k e m
	let remove m k = M.remove k m
	let find m k = M.find k m
	let iter m f = M.iter f m
	let map m f = M.mapi f m
end

module GenSet : Cnt.GENSET =
struct
	type 'a t = 'a list
	let empty = []
	let is_empty s = List.length s = 0
	let size = List.length
	let add s e = e::s
	let iter s f = List.iter f s
	let map s f = List.map f s
	let exists s f = List.exists f s
end

module GenRing (ELMT : sig type t end)
	: Cnt.GENRING with type elmt = ELMT.t =
struct
	type elmt = ELMT.t
	type 'a ring = elmt list * elmt list * int

	let empty = [], [], 0
	let is_empty (_, _, s) = s = 0

	let rewind (prevs, nexts, size) =
		match List.rev_append prevs nexts with
			| [] -> empty
			| fst::rest -> [fst], rest, size

	let get = function
		| curr::_, _, _ -> curr
		| _ -> raise Not_found
	let next = function
		| prevs, curr::nexts, size -> curr::prevs, nexts, size
		| lst -> rewind lst
	let prev = function
		| [curr], nexts, size -> List.rev (curr::nexts), [], size
		| curr::prevs, nexts, size -> prevs, curr::nexts, size
		| [], [], size -> assert(size = 0) ; empty
		| _ -> assert false
	let insert_before lst e = match lst with
		| curr::prevs, nexts, size -> e::prevs, curr::nexts, (size+1)
		| _ -> [e], [], 1
	let insert_after lst e = match lst with
		| curr::prevs, nexts, size -> e::curr::prevs, nexts, (size+1)
		| _ -> [e], [], 1
	let remove lst = match lst with
		| _::prevs, n::nexts, size -> n::prevs, nexts, (size-1)
		| _::prevs, [], size -> rewind (prevs, [], (size-1))
		| _ -> raise Not_found
	let length (_, _, s) = s
	let iter lst f =
		let rec aux l s =
			if s > 0 then (f l ; aux (next l) (s-1)) in
		aux lst (length lst)
	let iter_pairs lst f =
		let rec aux lst f n =
			if n > 0 then (
				f lst ;
				aux (next lst) f (n-1)
			) in
		let len = ref (length lst) in
		iter lst (fun lst0 ->
			decr len ;
			aux (next lst0) (f lst0) !len)
	let fold lst f first =
		let rec aux l a s =
			if s > 0 then aux (next l) (f l a) (s-1) else a in
		aux lst first (length lst)
		
end

module GenSortList : Cnt.GENSORTLIST =
struct
	type 'a t = {
		lst: ('a list * 'a * 'a list) option ;
		cmp: ('a -> 'a -> int)
	}

	let empty f = { lst = None ; cmp = f }
	let is_empty f = f.lst = None

	let step_left { lst=l ; cmp=c } =
		let step_left_tripplet = function
		| Some (e::rest, middle, right) -> Some (rest, e, middle::right)
		| x -> x in
		{ lst = step_left_tripplet l ; cmp = c }
	let step_right { lst=l ; cmp=c } =
		let step_right_tripplet = function
		| Some (left, middle, e::rest) -> Some (middle::left, e, rest)
		| x -> x in
		{ lst = step_right_tripplet l ; cmp = c }

	let rec insert l e = match l.lst with
	| None ->
		{ lst = Some ([], e, []) ; cmp = l.cmp }
	| Some (left, middle, right) ->
		if l.cmp e middle < 0 then (
			if left = [] then { lst = Some ([e], middle, right) ; cmp = l.cmp }
			else insert (step_left l) e
		) else (
			if right = [] then { lst = Some (left, middle, [e]) ; cmp = l.cmp }
			else insert (step_right l) e
		)
	
	let rec remove l e = match l.lst with
	| None -> raise Not_found
	| Some (left, middle, right) ->
		let remove_middle_from_tripplet = function
		| Some (l::rest, _, right) -> Some (rest, l, right)
		| Some ([], _, r::rest) -> Some ([], r, rest)
		| Some ([], _, []) -> None
		| None -> failwith "BUG" in
		match l.cmp e middle with
		| 0 -> { cmp = l.cmp ; lst = remove_middle_from_tripplet l.lst }
		| n when n < 0 ->
			if left = [] then raise Not_found else remove (step_left l) e
		| _ ->
			if right = [] then raise Not_found else remove (step_right l) e

end

module GenBag : Cnt.GENBAG =
struct
	type 'a t = 'a list ref

	let create _n = ref []
	let is_empty b = !b = []

	let put b e = b := e::!b
	let take b = match !b with
	| [] -> raise Not_found
	| first::rest -> b := rest ; first

	let find b f = List.find f !b
end

module SimpleTree (Elmt_: Cnt.ORDERED)
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
