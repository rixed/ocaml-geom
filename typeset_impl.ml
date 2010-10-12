open Typeset_intf

(* TODO: An implementation of a bloc being a glyph *)

module Typeset_raw
	(Poly : Geom.POLYGON)
	(Bloc : BLOC with module K = Poly.Point.K) =
struct
	module Poly = Poly
	module Bloc = Bloc
	module K = Poly.Point.K
	module Vec = Poly.Point
	type k = K.t

	type glue_kind = Stuck | Space of (k * k) (* | Hyphen *)

	type chunk = glue_kind * Bloc.t

	let typeset_line ?(justification=Start) dim tot_space chunks =
		let dist_from_target min max = (K.sub (K.half (K.add min max)) tot_space) in
		(* Count the nth first chunks and min_space, max_space for these chunks,
		 * until chunk list is empty or avg_space no more closer to tot_space *)
		let rec consumed_space prev_b prev_s prev_min prev_max prev_dist = function
			| [] -> prev_b, prev_s, prev_min, prev_max
			| (glue, bloc)::chunks' ->
				let spc = Bloc.size bloc dim in
				let b, s, min, max = match glue with
					| Stuck -> prev_b + 1, prev_s, K.add prev_min spc, K.add prev_max spc
					| Space (min, max) -> prev_b + 1, prev_s + 1, K.add (K.add prev_min min) spc, K.add (K.add prev_max max) spc in
				if min > tot_space then (
					Printf.printf "no more space : min = %f > tot = %f\n%!" (K.to_float min) (K.to_float tot_space) ;
					prev_b, prev_s, prev_min, prev_max
				) else (
					let dist = K.abs (dist_from_target min max) in
					Printf.printf "dist = %f, prev_dist = %f\n%!" (K.to_float dist) (K.to_float prev_dist) ;
					if dist > prev_dist then prev_b, prev_s, prev_min, prev_max
					else consumed_space b s min max dist chunks'
				) in
		let n_blocs, n_spcs, min, max = consumed_space 0 0 K.zero K.zero tot_space chunks in
		assert (n_blocs >= n_spcs) ;
		let dist = dist_from_target min max in
		(* distribute dist evenly in the n chunks *)
		let spc_dist =
			if justification = Justified && n_spcs > 0 then
				K.div dist (K.of_int n_spcs)
			else K.zero in
		Printf.printf "Eat %d chunks, %d spaces, dist = %f (%f per space)\n%!"
			n_blocs n_spcs (K.to_float dist) (K.to_float spc_dist) ;
		let rec add_offset offsets last_pos rem_b rem_s chunks =
			if rem_b = 0 then offsets else (
				let glue, bloc = List.hd chunks in
				let rem_s', offset = match glue with
					| Space (min, max) ->
						rem_s - 1, Vec.add last_pos (Vec.mul (K.sub (K.half (K.add min max)) spc_dist) (Vec.make_unit dim))
					| _ ->
						rem_s, last_pos in
				let disp = Vec.mul (Bloc.size bloc dim) (Vec.make_unit dim) in
				let last_pos' = Vec.add offset disp in
				add_offset (offset::offsets) last_pos' (rem_b-1) rem_s' (List.tl chunks)
			) in
		List.rev (add_offset [] Vec.zero n_blocs n_spcs chunks)

(*
	let chunks_of_string str =
		(* Simple implementation : just pack individual letters *)
		let add_letter chunks n was_space =
			if n >= String.length str then List.rev chunks
			else if str.(n) = ' ' then
				add_letter chunks (n+1) true
			else (
				let chunk = chunk_of_char str.(n) was_space in
				add_letter (chunk::chunks) (n+1) false
			) in
		add_letter [] 0 false
*)
	(* Given a poly and a line height, return a set of (start_pos, length) of lines *)
	let lines_of_poly poly height =
		ignore poly ;
		ignore height ;
		[]

	let typeset
		?(orientation=Horizontal) ?(direction=Left_to_right)
		?(vert_justification=Start) ?(horiz_justification=Start)
		frame chunks =
		let line_height = List.fold_left (fun h (_, b) -> max h (Bloc.size b 1)) K.zero chunks in
		let lines = lines_of_poly frame line_height in
		ignore lines ;
		ignore frame ;
		ignore chunks ;
		ignore orientation ;
		ignore direction ;
		ignore vert_justification ;
		ignore horiz_justification ;
		[]

end

module Typeset
	(Poly : Geom.POLYGON)
	(Bloc : BLOC with module K = Poly.Point.K)
	: Typeset_intf.TYPESET with module Poly = Poly and module Bloc = Bloc =
	Typeset_raw (Poly) (Bloc)
