module ConvexHullSet : Geom.CONVEX_HULL_SET =
	functor (Poly : Geom.POLYGON) ->
	functor (PSet : Geom.POINT_SET with module Point = Poly.Point) ->
struct
	let convex_hull points =
		let edges = Hashtbl.create (PSet.size points) in
		let last_point = ref None in
		let foreach_pairs f =
			PSet.iter points (fun p ->
				PSet.iter points (fun q -> if p != q then f p q)
			) in
		let are_some_points_at_right p q = (* anything that's either at right or (on the line but not on the segment) *)
			PSet.exists points (fun r ->
				if r == p || r == q then true else
				let cmp = PSet.Point.compare_left p q r in
				if cmp > 0 then true
				else if cmp = 0 then not (PSet.Point.compare_segment p q p)
				else false
			) in
		let are_all_points_at_left p q = not (are_some_points_at_right p q) in

		foreach_pairs (fun p q ->
			if are_all_points_at_left p q then
				Hashtbl.add edges p q ;
				last_point := Some p
		) ;
		match !last_point with
		| None -> Poly.empty
		| Some lastp ->
			let rec add_next_edge poly p =
				if Poly.get poly == p then poly
				else add_next_edge (Poly.insert_after poly p) (Hashtbl.find edges p) in
			add_next_edge Poly.empty lastp
end

module Algorithms
	(P_: Geom.POLYGON)
	(Path_: Geom.PATH with module Point = P_.Point)
	: Geom.ALGORITHMS with
		module Poly = P_ and module Path = Path_ =
struct
	module Poly = P_
	module Path = Path_

	let scale_point point center ratio =
		Poly.Point.add center (Poly.Point.mul (Poly.Point.sub point center) ratio)

	let next_pt p = Poly.get (Poly.next p)
	let prev_pt p = Poly.get (Poly.prev p)

	let area polys =
		List.fold_left (fun sum poly ->
			Poly.Point.K.add sum (Poly.fold poly (fun p s ->
				Poly.Point.K.add s (Poly.Point.area (Poly.get p) (Poly.get (Poly.next p))))
				Poly.Point.K.zero))
		Poly.Point.K.zero polys

	let is_convex_at poly =
		(Poly.Point.compare_left (prev_pt poly) (Poly.get poly) (next_pt poly)) >= 0

	let iter_concave poly f =
		Poly.iter poly (fun pol ->
			if not (is_convex_at pol) then f pol)

	let is_convex poly =
		try (
			iter_concave poly (fun _ -> raise Exit) ;
			true
		) with Exit -> false

	(* Check weither the target point in toward the interior of the poly at the focused point *)
	let in_cone poly target =
		let prev = prev_pt poly
		and next = next_pt poly
		and focus = Poly.get poly in
		let cmp1 = (Poly.Point.compare_left focus target prev) > 0
		and cmp2 = (Poly.Point.compare_left focus target next) > 0 in
		if cmp1 then
			not cmp2 || (Poly.Point.compare_left focus prev next) > 0
		else
			not cmp2 && (Poly.Point.compare_left focus prev next) > 0

	let is_diagonal pol0 pol1 =
		let p0 = Poly.get pol0 in
		let p1 = Poly.get pol1 in
		try (
			Poly.iter pol0 (fun pol ->
				let q0 = Poly.get pol in
				let q1 = next_pt pol in
				if q0 != p0 && q0 != p1 &&
				   q1 != p0 && q1 != p1 &&
				   Poly.Point.intersect p0 p1 q0 q1 then raise Exit) ;
			true
		) with Exit -> false
	
	let are_neighbour pol0 pol1 =
		let p = Poly.get pol0 in
		p == prev_pt pol1 || p == next_pt pol1

	let can_split pol0 pol1 =
		let p0 = Poly.get pol0 in
		let p1 = Poly.get pol1 in
		p0 != p1 &&
		not (are_neighbour pol0 pol1) &&
		in_cone pol0 p1 &&
		is_diagonal pol0 pol1

	let split_by pol0 pol1 =
		let first_point = Poly.get pol0
		and final_point = Poly.get pol1 in
		assert (first_point != final_point) ;
		let rec move_point old_pol new_pol =
			let p = Poly.get old_pol in
			if p == final_point then old_pol, new_pol else
				move_point (Poly.remove old_pol) (Poly.insert_after new_pol p) in
		let new_pol = (Poly.insert_after (Poly.insert_after (Poly.empty) final_point) first_point) in
		move_point (Poly.next pol0) new_pol

	let focus_on poly p =
		(* Return the same poly, focused on p *)
		let first_point = Poly.get poly in
		let rec refocus pol = match Poly.get pol with
			| p' when p' == first_point -> raise Not_found
			| p' when p' == p -> pol
			| _ -> refocus (Poly.next pol) in
		if first_point == p then poly else refocus (Poly.next poly)

	let split_by_points poly p0 p1 =
		(* Look for positions of these points, then use split_by *)
		split_by (focus_on poly p0) (focus_on poly p1)

	let make_chooser () =
		let best_segment = ref None in
		let best_dist = ref None in	(* Cache best_segment's length *)
		let choose_best pol0 pol1 thing =
			let dist = Poly.Point.norm2 (Poly.Point.sub
				(Poly.get pol0)
				(Poly.get pol1)) in
			match !best_dist with
			| Some d when d <= dist -> ()
			| _ -> best_dist := Some dist ; best_segment := Some (pol0, pol1, thing) in
		let pick_best () = !best_segment in
		(choose_best, pick_best)

	let split_concavity poly =
		(* Build the list of concave points *)
		let concave_pts = ref [] in
		iter_concave poly (fun pol -> concave_pts := pol::!concave_pts) ;
		(* Try splits using only concave points. *)
		let choose_best, pick_best = make_chooser () in
		let foreach_pairs l f =
			List.iter (fun e1 ->
				List.iter (fun e2 ->
					if e1 != e2 then f e1 e2
				) l
			) l
		in
		foreach_pairs !concave_pts (fun pol0 pol1 ->
			if can_split pol0 pol1 then choose_best pol0 pol1 ()
		) ;
		if pick_best () = None then (
			(* Now try each concave point against every points. *)
			List.iter (fun pol0 ->
				Poly.iter poly (fun pol1 ->	(* FIXME: we will once again test concave points. *)
					if can_split pol0 pol1 then choose_best pol0 pol1 ()
				)
			) !concave_pts
		) ;
		match pick_best () with
		| None -> (poly, Poly.empty)
		| Some (pol0, pol1, _) -> split_by pol0 pol1

	let simplify all_polys =
		let rec aux poly0 polys =
			let connect poly0 poly1 =
				let res = ref poly0 in
				Poly.iter poly1 (fun poly ->
					res := Poly.insert_after !res (Poly.get poly)) ;
				(* We must copy the points as we disallow a poly to store twice the same physical point *)
				Poly.insert_after
					(Poly.insert_after !res
						(Poly.Point.copy (Poly.get poly1)))
					(Poly.Point.copy (Poly.get poly0)) in
			let best_connection poly1 =
				(* Look for the best merging line *)
				let choose_best, pick_best = make_chooser () in
				Poly.iter poly0 (fun pol0 ->
					Poly.iter poly1 (fun pol1 ->
						choose_best pol0 pol1 ())) ;
				pick_best () in
			if polys = [] then poly0 else (
				let choose_best, pick_best = make_chooser () in
				(* Like List.iter, but supply a list without the selected element : *)
				let list_iter_with_cmpl f l =
					let rec aux prevs = function
						| [] -> ()
						| elmt::[] -> f prevs elmt []
						| elmt::nexts ->
							f prevs elmt nexts ;
							aux (elmt::prevs) nexts in
					aux [] l in
				list_iter_with_cmpl (fun prev_polys poly next_polys ->
					match best_connection poly with
					| None -> ()
					| Some (p0, p1, ()) -> choose_best p0 p1 (List.rev_append prev_polys next_polys))
					polys ;
				match pick_best () with
				| None -> poly0
				| Some (p0, p1, other_polys) ->
					let new_poly0 = connect p0 p1 in
					aux new_poly0 other_polys) in
		aux (List.hd all_polys) (List.tl all_polys)

	let convex_partition polys =
		let res = ref [] in
		let rec split_aux pol = match split_concavity pol with
			| pol0, pol1 when pol1 = Poly.empty -> res := pol0::!res
			| pol0, pol1 -> split_aux pol0 ; split_aux pol1 in
		split_aux (simplify polys) ;
		!res

	let rec iter_diagonals poly f =
		Poly.iter_pairs poly (fun p0 p1 -> if not (are_neighbour p0 p1) then f p0 p1)

	let iter_splitable_diagonals poly f =
		iter_diagonals poly (fun p0 p1 ->
			if can_split p0 p1 then f p0 p1)

	let triangulate_slow polys =
		let convex_polys = convex_partition polys in
		let res = ref [] in
		let rec triangulate_convex poly =
			let (choose_best, pick_best) = make_chooser () in
			iter_splitable_diagonals poly (fun p0 p1 -> choose_best p0 p1 ()) ;
			match pick_best () with
			| None -> res := poly::!res
			| Some (p0, p1, _) ->
				let pol0, pol1 = split_by p0 p1 in
				triangulate_convex pol0 ; triangulate_convex pol1
			in
		List.iter triangulate_convex convex_polys ;
		!res
	
	(* Tells weither ]p0;p1[ intersect any of the given polys *)
	let intersect_polys p0 p1 polys =
		let intersect_poly poly =
			try (
				Poly.iter poly (fun p ->
					let q0 = Poly.get p in
					let q1 = next_pt p in
					if Poly.Point.intersect ~closed:false p0 p1 q0 q1 then raise Exit) ;
				false
			) with Exit -> true in
		try (
			List.iter (fun poly ->
				if intersect_poly poly then raise Exit) polys ;
			false
		) with Exit -> true

	let inverse_single poly =
		let ret = ref Poly.empty in
		Poly.iter poly (fun p -> ret := Poly.insert_before !ret (Poly.get p)) ;
		!ret
	
	let inverse polys = List.map inverse_single polys

	let transform poly f =
		let new_poly = ref Poly.empty in
		Poly.iter poly (fun p ->
			new_poly := Poly.insert_after !new_poly (f (Poly.get p))) ;
		!new_poly

	let translate_single_poly poly vec =
		transform poly (fun point -> Poly.Point.add point vec)
	
	let translate_poly polys vec = List.map (fun p -> translate_single_poly p vec) polys

	let scale_single_poly poly center ratio =
		transform poly (fun point -> scale_point point center ratio)
	
	let scale_poly polys center ratio = List.map (fun p -> scale_single_poly p center ratio) polys
	
	let poly_of_path path res =
		let poly = ref Poly.empty in
		Path.iter path res (fun pt ->
			if not (Poly.is_empty !poly) then (
				if Poly.Point.eq (Poly.get !poly) pt then Format.printf "Adding twice point %a@\n" Poly.Point.print pt
			) ;
			poly := Poly.insert_after !poly pt) ;
		!poly
	
	module Monotonizer =
	struct
		(* Data structures needed during the sweep :
		 * - a procedural polygon, initialized with the user's poly list, to which
		 *   we will add diagonals. This is an array of (point, next idx, prev idx).
		 *   This is the result that we will converted back to functional poly list
		 *   before returning ;
		 * - an array of vertices -for fast access- of (point, kind, helper) ;
		 *   this can be merged with the procedural poly which is also an array,
		 *   adding a tag or using an array of (point, kind, next idx, prev idx, helper),
		 *   provided we use a distinct scheme to find the next/prev points when needed
		 *   during the slice (prev idx and next idx describe the result, not the
		 *   original polygon we sweep over). This is most easily done by adding two more
		 *   fields orig_prev and orig_next, that describes the original polygon (we cannot
		 *   merely count on the fact that prev index is i-1 and next i+1 because the
		 *   poly is made of saveral simple polys !)
		 * - an array of vertices index sorted by Y for the sweep ;
		 * - a tree of edges searched during the sweep.
		 *)
		type vertex_kind = Start | End | Regular_down | Regular_up | Split | Merge
		let string_of_kind = function
			| Start -> "Start"
			| End -> "End"
			| Regular_down -> "Regular down"
			| Regular_up -> "Regular up"
			| Split -> "Split"
			| Merge -> "Merge"

		(* The algorithm below is taken from Comp. Geom. by de Berg, van Kreveld etc
		 * where Y is taken to grow downward. *)
		let compare_point_y p1 p2 =
			let cmpy = Poly.Point.compare_y p1 p2 in
			if cmpy <> 0 then -cmpy else Poly.Point.compare_x p1 p2

		let kind_of_point poly =
			let prev = Poly.prev poly in
			let next = Poly.next poly in
			let cmp1 = compare_point_y (Poly.get prev) (Poly.get poly) in
			let cmp2 = compare_point_y (Poly.get next) (Poly.get poly) in
			assert (cmp1 <> 0 && cmp2 <> 0) ;
			if cmp1 = -1 && cmp2 = 1 then Regular_down else
			if cmp1 = 1 && cmp2 = -1 then Regular_up else
				let convex = is_convex_at poly in
				if cmp1 = -1 then (* prev and next points are before current point *)
					if convex then End else Merge
				else (* prev and next points are after current point *)
					if convex then Start else Split

		type vertex = {
			point : Poly.Point.t ;
			orig_prev : int ;
			orig_next : int ;
			orig_next_point : Poly.Point.t ;	(* so that eadges are easier to compare *)
			mutable res_prev : int ;
			mutable res_next : int ;
			mutable helper : int ;	(* -1 when unset *)
			kind : vertex_kind	(* kind of the starting vertex *)
		}

		type procpoly = {
			vertices : vertex option array ;
			mutable size : int	(* because the array is made larger so that we can add diagonals *)
		}

		(* return the procpoly equivalent to a list of simple polys *)
		let make_procpoly polys =
			let orig_size = List.fold_left (fun sz poly -> sz + (Poly.length poly)) 0 polys in
			let ppoly_size = orig_size * 2 in
			let loop_start = ref 0 in
			let left_polys = ref polys in
			{
				size = orig_size ;
				vertices = Array.init ppoly_size (fun i ->
					let vertex_of_poly poly = 
						let sz = Poly.length poly in
						let orig_prev = (if i > !loop_start then i else !loop_start + sz) -1 in
						let orig_next = if i - !loop_start < sz-1 then i+1 else !loop_start in
						{
							point = Poly.get poly ;
							orig_prev = orig_prev ;
							orig_next = orig_next ;
							orig_next_point = Poly.get (Poly.next poly) ;
							res_prev = orig_prev ;
							res_next = orig_next ;
							helper = -1 ;
							kind = kind_of_point poly
						} in
					let rec init_single () =
						if i >= orig_size then None
						else
							let poly = List.hd !left_polys in
							if i - !loop_start < Poly.length poly then (
								left_polys := (Poly.next poly)::(List.tl !left_polys) ;
								Some (vertex_of_poly poly)
							) else (
								loop_start := i ;
								left_polys := List.tl !left_polys ;
								init_single ()
							) in
						init_single ()
				)
			}

		(* The inverse of the previous one *)
		let make_funpoly ppoly =
			let res = ref [] in
			Array.iteri (fun i vertex_opt ->
				let rec poly_of_loop i poly = match ppoly.vertices.(i) with
					| None -> poly
					| Some vertex ->
						(*Format.printf "  Englobing vertex %d (%a), res_next %d@." i Poly.Point.print vertex.point vertex.res_next ;*)
						ppoly.vertices.(i) <- None ;
						poly_of_loop
							vertex.res_next
							(Poly.insert_after poly vertex.point) in
				match vertex_opt with
				| None -> ()
				| Some _vertex -> (* add the loop starting at i *)
					(*Format.printf "Adding loop starting at %d@." i ;*)
					res := (poly_of_loop i Poly.empty)::!res
			) ppoly.vertices ;
			!res

		(* Split a procpoly by two vertices *)
		let add_diag ppoly i1 i2 =
			(*Format.printf "Add diagonal from %d to %d@." i1 i2 ;*)
			assert (i1 <> i2) ;
			assert (i1 < ppoly.size && i2 < ppoly.size) ;
			assert (ppoly.size + 2 <= Array.length ppoly.vertices) ;
			let v1 = Cnt.unopt ppoly.vertices.(i1) in
			let v2 = Cnt.unopt ppoly.vertices.(i2) in
			ppoly.vertices.(ppoly.size) <- Some { v1 with res_next = ppoly.size+1 } ;
			ppoly.vertices.(ppoly.size+1) <- Some { v2 with res_prev = ppoly.size } ;
			(Cnt.unopt ppoly.vertices.(v1.res_prev)).res_next <- ppoly.size ;
			v1.res_prev <- i2 ;
			(Cnt.unopt ppoly.vertices.(v2.res_next)).res_prev <- ppoly.size+1 ;
			v2.res_next <- i1 ;
			ppoly.size <- ppoly.size + 2

		(* For the Tree *)
		let compare_edge_x v1 v2 =
			(* e1 is at left from e2 if the polygon (e1h, e1l, e2l, e2h) is direct *)
			let min_max p0 p1 = if compare_point_y p0 p1 < 0 then p0, p1 else p1, p0 in
			let e1l, e1h = min_max v1.point v1.orig_next_point in
			let e2l, e2h = min_max v2.point v2.orig_next_point in
			if e1l == e2l && e1h == e2h then 0
			else
				let poly = (Poly.insert_after (Poly.insert_after (Poly.insert_after (Poly.insert_after Poly.empty e1h) e1l) e2l) e2h) in
				let cmp = Poly.Point.K.compare (area [poly]) Poly.Point.K.zero in
				if cmp <> 0 then cmp
				else
					(* Can happen if e1 and e2 share a vertex, or are colinear.
					 * In this case we don't care of the result we just want it to be consistant. *)
					compare_point_y e1l e2l

		module Tree = Cnt_impl.SimpleTree (struct
			type t = vertex
			let compare v1 v2 = compare_edge_x v1 v2
		end)

		let monotonize ppoly =
			(* Build a list of all the vertices, sorted *)
			let queue = Array.init ppoly.size (fun i -> i) in
			Array.sort (fun v0 v1 ->
				compare_point_y
					(Cnt.unopt ppoly.vertices.(v0)).point
					(Cnt.unopt ppoly.vertices.(v1)).point)
				queue ;
			
			(*Format.printf "@[queue : " ;
			Array.iter (fun i -> Format.printf "(@[%d : %a of kind %s@]),@ " i Poly.Point.print (Cnt.unopt ppoly.vertices.(i)).point (string_of_kind (Cnt.unopt ppoly.vertices.(i)).kind)) queue ;
			Format.printf "@]@." ;*)

			(* Binary search tree of edges *)
			let tree = ref Tree.empty in
			
			(*let print_edge fmt v =
				Format.fprintf fmt "@[[%a %a %s %d]@]"
					Poly.Point.print v.point
					Poly.Point.print v.orig_next_point
					(string_of_kind v.kind)
					v.helper in
			let print_tree () =
				Format.printf "tree = @[" ;
				Tree.iter !tree (fun edge ->
					Format.printf "%a@ " print_edge edge) ;
				Format.printf "@]@." in*)

			let process_vertex i vertex =
				(*Format.printf "Process point %d at %a of kind %s :@." i Poly.Point.print vertex.point (string_of_kind vertex.kind) ;*)
				match vertex.kind with
				| Start ->
					vertex.helper <- i ;
					(*Format.printf "Inserting edge %a into tree@." print_edge vertex ;*)
					tree := Tree.insert !tree vertex (*;
					print_tree ()*)
				| End ->
					let prev = Cnt.unopt ppoly.vertices.((Cnt.unopt ppoly.vertices.(i)).orig_prev) in
					let h = prev.helper in
					if (Cnt.unopt ppoly.vertices.(h)).kind = Merge then add_diag ppoly i h ;
					(*Format.printf "Removing prev %a from tree@." print_edge prev ;*)
					tree := Tree.remove !tree prev (*;
					print_tree ()*)
				| Split ->
					let at_left = Tree.find_before !tree vertex in
					add_diag ppoly i at_left.helper ;
					at_left.helper <- i ;
					vertex.helper <- i ;
					(*Format.printf "Inserting edge %a into tree@." print_edge vertex ;*)
					tree := Tree.insert !tree vertex (*;
					print_tree ()*)
				| Merge ->
					let prev = Cnt.unopt ppoly.vertices.((Cnt.unopt ppoly.vertices.(i)).orig_prev) in
					let h = prev.helper in
					if (Cnt.unopt ppoly.vertices.(h)).kind = Merge then add_diag ppoly i h ;
					(*Format.printf "Removing prev %a from tree@." print_edge prev ;*)
					tree := Tree.remove !tree prev ;
					let at_left = Tree.find_before !tree vertex in
					let h' = at_left.helper in
					if (Cnt.unopt ppoly.vertices.(h')).kind = Merge then add_diag ppoly i h' ;
					at_left.helper <- i (*;
					print_tree ()*)
				| Regular_down ->
					let prev = Cnt.unopt ppoly.vertices.((Cnt.unopt ppoly.vertices.(i)).orig_prev) in
					let h = prev.helper in
					if (Cnt.unopt ppoly.vertices.(h)).kind = Merge then add_diag ppoly i h ;
					(*Format.printf "Removing prev %a from tree@." print_edge prev ;*)
					tree := Tree.remove !tree prev ;
					vertex.helper <- i ;
					(*Format.printf "Inserting edge %a into tree@." print_edge vertex ;*)
					tree := Tree.insert !tree vertex (*;
					print_tree ()*)
				| Regular_up ->
					let at_left = Tree.find_before !tree vertex in
					let h = at_left.helper in
					if (Cnt.unopt ppoly.vertices.(h)).kind = Merge then add_diag ppoly i h ;
					at_left.helper <- i (*;
					print_tree () *) in
			Array.iter (fun i -> process_vertex i (Cnt.unopt ppoly.vertices.(i))) queue
		
		let triangulate ppoly to_triangle =
			monotonize ppoly
			(* TODO: split monotone polys to convex polys or up to triangles *)

	end (* module Monotonizer *)
	
	let monotonize polys =
		let ppoly = Monotonizer.make_procpoly polys in
		Monotonizer.monotonize ppoly ;
		Monotonizer.make_funpoly ppoly
	
	let triangulate polys =
		let ppoly = Monotonizer.make_procpoly polys in
		Monotonizer.triangulate ppoly true ;
		Monotonizer.make_funpoly ppoly
			

end (* module Algorithms *)

