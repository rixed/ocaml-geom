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

	let debug = true

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

	let is_convex_at prev point next =
		Poly.Point.compare_left prev point next >= 0

	let iter_concave poly f =
		Poly.iter poly (fun pol ->
			if not (is_convex_at (prev_pt pol) (Poly.get pol) (next_pt pol)) then f pol)

	let is_convex poly =
		try (
			iter_concave poly (fun _ -> raise Exit) ;
			true
		) with Exit -> false

	let in_between prev focus next target =
		let cmp1 = (Poly.Point.compare_left focus target prev) > 0
		and cmp2 = (Poly.Point.compare_left focus target next) > 0 in
		if cmp1 then
			not cmp2 || (Poly.Point.compare_left focus prev next) > 0
		else
			not cmp2 && (Poly.Point.compare_left focus prev next) > 0


	(* Check weither the target point is toward the interior of the poly at the focused point *)
	let in_cone poly target =
		in_between (prev_pt poly) (Poly.get poly) (next_pt poly) target

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
		choose_best, pick_best

	let split_concavity poly =
		(* Build the list of concave points *)
		let concave_pts = ref [] in
		iter_concave poly (fun pol -> concave_pts := pol :: !concave_pts) ;
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
		| None -> poly, Poly.empty
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
						| elmt :: [] -> f prevs elmt []
						| elmt :: nexts ->
							f prevs elmt nexts ;
							aux (elmt :: prevs) nexts in
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

	let convex_partition_slow polys =
		let res = ref [] in
		let rec split_aux pol = match split_concavity pol with
			| pol0, pol1 when pol1 = Poly.empty -> res := pol0 :: !res
			| pol0, pol1 -> split_aux pol0 ; split_aux pol1 in
		split_aux (simplify polys) ;
		!res
	
	let rec iter_diagonals poly f =
		Poly.iter_pairs poly (fun p0 p1 -> if not (are_neighbour p0 p1) then f p0 p1)

	let iter_splitable_diagonals poly f =
		iter_diagonals poly (fun p0 p1 ->
			if can_split p0 p1 then f p0 p1)

	let triangulate_slow polys =
		let convex_polys = convex_partition_slow polys in
		let res = ref [] in
		let rec triangulate_convex poly =
			let choose_best, pick_best = make_chooser () in
			iter_splitable_diagonals poly (fun p0 p1 -> choose_best p0 p1 ()) ;
			match pick_best () with
			| None -> res := poly :: !res
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
			let prev, point, next = prev_pt poly, Poly.get poly, next_pt poly in
			let cmp1 = compare_point_y prev point in
			let cmp2 = compare_point_y next point in
			assert (cmp1 <> 0 && cmp2 <> 0) ;
			if cmp1 = -1 && cmp2 = 1 then Regular_down else
			if cmp1 = 1 && cmp2 = -1 then Regular_up else
				let convex = is_convex_at prev point next in
				if cmp1 = -1 then (* prev and next points are before current point *)
					if convex then End else Merge
				else (* prev and next points are after current point *)
					if convex then Start else Split

		type vertex = {
			point : Poly.Point.t ;
			next_point : Poly.Point.t ;	(* so that edges are easier to compare *)
			prev : int ;
			next : int ;
			kind : vertex_kind ;	(* true only on the original ppoly, not resulting one *)
			mutable helper : int ;	(* -1 when unset *)
			mutable diags : int list
		}

		let print_vertex fmt v =
			Format.fprintf fmt "@[{fr:%a to:%a k:%s h:%d <-:%d ->:%d diags:["
				Poly.Point.print v.point
				Poly.Point.print v.next_point
				(string_of_kind v.kind)
				v.helper v.prev v.next ;
			List.iter (fun i -> Format.fprintf fmt "%d;" i) v.diags ;
			Format.fprintf fmt "]}@]"

		type procpoly = {
			vertices : vertex array ;
		}

		(* return the procpoly equivalent to a list of simple polys *)
		let make_procpoly polys =
			let size = List.fold_left (fun sz poly -> sz + (Poly.length poly)) 0 polys in
			let loop_start = ref 0 in
			let left_polys = ref polys in
			{
				vertices = Array.init size (fun i ->
					let vertex_of_poly poly = 
						let sz = Poly.length poly in
						let prev = (if i > !loop_start then i else !loop_start + sz) -1 in
						let next = if i - !loop_start < sz-1 then i+1 else !loop_start in
						{
							point = Poly.get poly ;
							next_point = Poly.get (Poly.next poly) ;
							prev = prev ;
							next = next ;
							kind = kind_of_point poly ;
							helper = -1 ;
							diags = [ next ]
						} in
					let rec init_single () =
						let poly = List.hd !left_polys in
						if i - !loop_start < Poly.length poly then (
							left_polys := Poly.next poly :: List.tl !left_polys ;
							vertex_of_poly poly
						) else (
							loop_start := i ;
							left_polys := List.tl !left_polys ;
							init_single ()
						) in
					init_single ()
				)
			}

		(* The inverse of the previous one, taking into account internal diagonals.
		 * We proceed by emptying vertices diags. *)
		let make_funpoly ppoly =
			let res = ref [] in	(* our result, a list of polys *)
			let add_all_loops_from i start_vertex =
				let rec poly_of_loop j from_opt poly =
					let vertex = ppoly.vertices.(j) in
					let best_diag, rem_diags = match from_opt with
						| None ->
							(* If we are just starting, any diag will do *)
							List.hd vertex.diags, List.tl vertex.diags
						| Some from ->
							let rec test_diag bads best others =
								match others with
								| [] -> best, bads
								| other :: rest ->
									if best = from || in_between
										ppoly.vertices.(from).point
										vertex.point
										ppoly.vertices.(best).point
										ppoly.vertices.(other).point
									then test_diag (best :: bads) other rest
									else test_diag (other :: bads) best rest in
							test_diag [] (List.hd vertex.diags) (List.tl vertex.diags) in
					vertex.diags <- rem_diags ;
					let new_poly = Poly.insert_after poly vertex.point in
					if best_diag = i then new_poly
					else (
						if debug then Format.printf "  Adding vertex #%d, then to #%d@." j best_diag ;
						poly_of_loop best_diag (Some j) new_poly
					) in
				while start_vertex.diags <> [] do
					if debug then Format.printf "Adding a loop starting at %d@." i ;
					res := poly_of_loop i None Poly.empty :: !res
				done in
			Array.iteri add_all_loops_from ppoly.vertices ;
			!res

		let add_diag ppoly i1 i2 =
			if debug then Format.printf "Add diagonal from %d to %d@." i1 i2 ;
			assert (i1 <> i2) ;
			ppoly.vertices.(i1).diags <- i2 :: ppoly.vertices.(i1).diags ;
			ppoly.vertices.(i2).diags <- i1 :: ppoly.vertices.(i2).diags

		(* For the Tree *)
		let compare_edge_x v1 v2 =
			(* e1 is at left from e2 if the polygon (e1h, e1l, e2l, e2h) is direct *)
			let min_max p0 p1 = if compare_point_y p0 p1 < 0 then p0, p1 else p1, p0 in
			let e1l, e1h = min_max v1.point v1.next_point in
			let e2l, e2h = min_max v2.point v2.next_point in
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

		(* Build a list of all the vertices, sorted *)
		let make_queue ppoly =
			let queue = Array.init (Array.length ppoly.vertices) (fun i -> i) in
			Array.sort (fun v0 v1 ->
				compare_point_y ppoly.vertices.(v0).point ppoly.vertices.(v1).point)
				queue ;
			queue

		let monotonize polys =
			let ppoly = make_procpoly polys in
			let queue = make_queue ppoly in
			
			if debug then (
				Format.printf "@[queue : " ;
				Array.iter (fun i ->
					Format.printf "(@[%d : %a@]),@ "
						i print_vertex ppoly.vertices.(i))
					queue ;
				Format.printf "@]@." ;
			) ;

			(* Binary search tree of edges *)
			let tree = ref Tree.empty in
			
			let print_tree () =
				Format.printf "tree = @[" ;
				Tree.iter !tree (fun edge ->
					Format.printf "%a@ " print_vertex edge) ;
				Format.printf "@]@." in

			let process_vertex i vertex =
				if debug then Format.printf "Process point %d at %a of kind %s :@." i Poly.Point.print vertex.point (string_of_kind vertex.kind) ;
				match vertex.kind with
				| Start ->
					vertex.helper <- i ;
					if debug then Format.printf "Inserting edge %a into tree@." print_vertex vertex ;
					tree := Tree.insert !tree vertex ;
					if debug then print_tree ()
				| End ->
					let prev = ppoly.vertices.(ppoly.vertices.(i).prev) in
					let h = prev.helper in
					if ppoly.vertices.(h).kind = Merge then add_diag ppoly i h ;
					if debug then Format.printf "Removing prev %a from tree@." print_vertex prev ;
					tree := Tree.remove !tree prev ;
					if debug then print_tree ()
				| Split ->
					let at_left = Tree.find_before !tree vertex in
					add_diag ppoly i at_left.helper ;
					at_left.helper <- i ;
					vertex.helper <- i ;
					if debug then Format.printf "Inserting edge %a into tree@." print_vertex vertex ;
					tree := Tree.insert !tree vertex ;
					if debug then print_tree ()
				| Merge ->
					let prev = ppoly.vertices.(ppoly.vertices.(i).prev) in
					let h = prev.helper in
					if ppoly.vertices.(h).kind = Merge then add_diag ppoly i h ;
					if debug then Format.printf "Removing prev %a from tree@." print_vertex prev ;
					tree := Tree.remove !tree prev ;
					let at_left = Tree.find_before !tree vertex in
					let h' = at_left.helper in
					if ppoly.vertices.(h').kind = Merge then add_diag ppoly i h' ;
					at_left.helper <- i ;
					if debug then print_tree ()
				| Regular_down ->
					let prev = ppoly.vertices.(ppoly.vertices.(i).prev) in
					let h = prev.helper in
					if ppoly.vertices.(h).kind = Merge then add_diag ppoly i h ;
					if debug then Format.printf "Removing prev %a from tree@." print_vertex prev ;
					tree := Tree.remove !tree prev ;
					vertex.helper <- i ;
					if debug then Format.printf "Inserting edge %a into tree@." print_vertex vertex ;
					tree := Tree.insert !tree vertex ;
					if debug then print_tree ()
				| Regular_up ->
					let at_left = Tree.find_before !tree vertex in
					let h = at_left.helper in
					if ppoly.vertices.(h).kind = Merge then add_diag ppoly i h ;
					at_left.helper <- i ;
					if debug then print_tree ()
				in
			Array.iter (fun i -> process_vertex i ppoly.vertices.(i)) queue ;
			make_funpoly ppoly
		
		let triangulate polys =
			let mono_polys = monotonize polys in
			let triangulate_mono poly =
				if debug then Format.printf "Triangulate poly %a@." Poly.print poly;
				let ppoly = make_procpoly [poly] in
				let queue = make_queue ppoly in
				(* The stack of not already triangulated vertices *)
				let stack = Stack.create () in
				let print_stack () =
					Format.printf "stack = @[" ;
					Stack.iter (fun i -> Format.printf "%d@ " i) stack ;
					Format.printf "@]@." in
				let rec add_diag_to_all_but_last v =
					let vs = Stack.pop stack in
					if not (Stack.is_empty stack) then (
						add_diag ppoly v vs ;
						add_diag_to_all_but_last v
					) in
				let process_opposed_vertex v =
					let last = Stack.top stack in
					if debug then Format.printf "Process vertex %d (%a),@ oposed to last (%d, %a)@."
						v print_vertex ppoly.vertices.(v) last print_vertex ppoly.vertices.(v) ;
					add_diag_to_all_but_last v ;
					Stack.push last stack ;
					Stack.push v stack in
				let process_sameside_vertex v =
					if debug then Format.printf "Process vertex %d (%a),@ same side than last (%d, %a)@."
						v print_vertex ppoly.vertices.(v)
						(Stack.top stack) print_vertex ppoly.vertices.(Stack.top stack) ;
					let diag_is_inside prev last =
						let pt_of v = ppoly.vertices.(v).point in
						let v_kind = ppoly.vertices.(v).kind in
						let cmp = Poly.Point.compare_left (pt_of v) (pt_of last) (pt_of prev) in
						v_kind = Regular_up && cmp > 0 || v_kind = Regular_down && cmp < 0 in
					let last_pop = ref (Stack.pop stack) in
					let rec aux last =
						if not (Stack.is_empty stack) then
							let top = Stack.top stack in
							if diag_is_inside top last then (
								add_diag ppoly v top ;
								last_pop := Stack.pop stack ;
								aux top
							) in
					aux !last_pop ;
					Stack.push !last_pop stack ;
					Stack.push v stack in
				let process_vertex i v =
					if debug then Format.printf "Processing queue.(%d) = %d (%a)@."
						i v print_vertex ppoly.vertices.(v) ;
					if i < 2 then Stack.push v stack
					else if i = (Array.length queue) - 1 then (
						(* Add diags to remaining points except last and first *)
						if debug then Format.printf "Empty the stack@." ;
						if not (Stack.is_empty stack) then
							try (
								ignore (Stack.pop stack) ;
								add_diag_to_all_but_last v
							) with Stack.Empty -> ()
					) else (
						(* Since ppoly is monotone, it only have one Start (at top),
						 * one End (at end) and Regular_up/down vertices. So all vertices
						 * on the stack are regular except for one Start. *)
						let top = ppoly.vertices.(Stack.top stack) in
						Format.printf "Stack top = %a, v = %a@."
							print_vertex top print_vertex ppoly.vertices.(v) ;
						if top.kind = Start || top.kind = ppoly.vertices.(v).kind then
							process_sameside_vertex v
						else
							process_opposed_vertex v
					) ;
					if debug then print_stack () in
				assert (ppoly.vertices.(queue.(0)).kind = Start) ;
				assert (ppoly.vertices.(queue.(Array.length queue -1)).kind = End) ;
				(* TODO: and that all others are regular *)
				Array.iteri process_vertex queue ;
				make_funpoly ppoly in
			List.flatten (List.map triangulate_mono (mono_polys))

	end (* module Monotonizer *)
	
	let monotonize = Monotonizer.monotonize
	
	let triangulate = Monotonizer.triangulate
			
	let convex_partition polys = convex_partition_slow polys


end (* module Algorithms *)

