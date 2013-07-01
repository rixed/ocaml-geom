module Make (Point : Geom.POINT)
	: Geom.PATH with module Point = Point =
struct
	module Point = Point
	type point = Point.t
	type interpolator = point -> point -> point list -> Point.K.t -> point list

	type t = {
		start : point ;
		edges : (point * point list * interpolator) list
	}

	let print ff path =
		Format.pp_open_box ff 0 ;
		Format.pp_print_string ff "<" ; Format.pp_print_space ff () ;
		let rec aux start edges =
			Point.print ff start ;
			(match edges with
			| [] -> ()
			| (e, _, _)::edges' -> aux e edges') in
		aux path.start path.edges ;
		Format.pp_print_string ff ">" ;
		Format.pp_close_box ff ()

	(*
	 * Path manipulation
	 *)

	let empty start = { start ; edges = [] }

	let is_empty path = path.edges = []

	let is_closed path =
		let rec last_point = function
			| [] -> path.start
			| [ p, _, _ ] -> p
			| _ :: e' -> last_point e' in
		let last = last_point path.edges and first = path.start in
		Point.eq first last

	let extend path next ctrls interp = (match path.edges with
		| (pt, [], _) :: _ -> if 0 = Point.compare pt next then
			Format.printf "Path edge of no length at point %a@\n" Point.print pt
		| _ -> ()) ;
		{ path with edges = path.edges @ [next, ctrls, interp] }

	let concat path1 path2 = { start = path1.start ; edges = path1.edges @ path2.edges }

	let length path = List.length path.edges

	let translate path disp =
		let edge_translate (p, ctrls, i) =
			Point.add p disp, List.map (fun p -> Point.add p disp) ctrls, i in
		{ start = Point.add path.start disp ; edges = List.map edge_translate path.edges }

	let rec inverse path = match path.edges with
		| [] -> path
		| [target, ctrls, interp] ->
			{ start = target ; edges = [ path.start, List.rev ctrls, interp ] }
		| (target, ctrls, interp) :: e' ->
			extend (inverse { start = target ; edges = e' }) path.start (List.rev ctrls) interp
		
	let center path =
		(* FIXME: we should add the center of each edge instead of adding the starting point and every edge's last *)
		let add_pos p (n, _, _) = Point.add p n in
		Point.mul (Point.K.inv (Point.K.of_int (length path))) (List.fold_left add_pos path.start path.edges)

	let scale path center scale =
		let scale_me p = Point.add center (Point.mul scale (Point.sub p center)) in
		let edge_scale (p, ctrls, i) = scale_me p, List.map scale_me ctrls, i in
		{ start = scale_me path.start ; edges = List.map edge_scale path.edges }

	let scale_along center axis ratio path =
		let scale_me p =
			(* decompose vector from center to p as one vector along axis and one perpendicular component. *)
			let t = Point.sub p center in
			let t_axis = Point.mul (Point.scalar_product axis t) axis in
			let t_perp = Point.sub t t_axis in
			(* now rescale axis component *)
			let new_t_axis = Point.mul ratio t_axis in
			(* and rebuild new p from these *)
			Point.add center (Point.add new_t_axis t_perp) in
		let edge_scale (p, ctrls, i) = scale_me p, List.map scale_me ctrls, i in
		{ start = scale_me path.start ; edges = List.map edge_scale path.edges }
		
    let clip p0 p1 path =
        let is_left p = Point.compare_left p0 p1 p >= 0 in
        let rec aux res start start_is_left = function
            | [] -> res
            | (stop, ctrls, interp) :: edges ->
                let stop_is_left = is_left stop in
                let res =
                    if start_is_left then
                        (* add this point to previous path *)
                        match res with
                        | [] -> [ extend (empty start) stop ctrls interp ]
                        | p::res' -> extend p stop ctrls interp :: res'
                    else if stop_is_left || List.exists is_left ctrls then
                        (* start a new path *)
                        extend (empty start) stop ctrls interp :: res
                    else res in
                aux res stop stop_is_left edges in
        let start_is_left = is_left path.start in
        let res = if start_is_left then [ empty path.start ] else [] in
        aux res path.start start_is_left path.edges
        
	(*
	 * Interpolators
	 *)

	(* Whatever the resolution, a straight line need no intermediary points *)
	let make_straight_line _start _stop _control _res = []

	let rec bezier ctrls res =
		let half_vector = Array.map Point.K.half in
		let len = Array.length ctrls in
		let mid_point = half_vector (Point.add ctrls.(0) ctrls.(len-1)) in
		let ctrls_l = Array.make len Point.zero in
		let ctrls_r = Array.make len Point.zero in
		let bino_coef = Array.init len (fun i -> if i = 0 then Point.K.one else Point.K.zero) in
		let divisor = ref Point.K.one in
		for l = 0 to (len-1) do
			let ll = len-1-l in
			(* compute line l (resp. len+1-l) of ctrls_l (resp. ctrls_r) *)
			for c = 0 to (len-1) do
				let cc = len-1-c in
				ctrls_l.(l)  <- Point.add ctrls_l.(l)  (Point.mul bino_coef.(c)  ctrls.(c)) ;
				ctrls_r.(ll) <- Point.add ctrls_r.(ll) (Point.mul bino_coef.(cc) ctrls.(c))
			done ;
			ctrls_l.(l)  <- Point.mul (Point.K.inv !divisor) ctrls_l.(l)  ;
			ctrls_r.(ll) <- Point.mul (Point.K.inv !divisor) ctrls_r.(ll) ;
			(* update binomial coefs *)
			for c = (len - 1) downto 1 do
				bino_coef.(c) <- Point.K.add bino_coef.(c) bino_coef.(c-1)
			done ;
			divisor := Point.K.double !divisor
		done ;
		if (Point.norm2 (Point.sub mid_point ctrls_r.(0))) < Point.K.square res then
			[ctrls_r.(0)]
		else
			(bezier ctrls_l res) @ [ctrls_r.(0)] @ (bezier ctrls_r res)

	let make_bezier_curve start stop ctrls res =
		let len = List.length ctrls in
		let ctrls_arr = Array.init (len+2) (fun i ->
			if i = 0 then start
			else if i = len+1 then stop
			else List.nth ctrls (i-1)) in
		bezier ctrls_arr res

	let iter res path f =
		let rec aux start edges =
			f start ;
			match edges with
			| [] -> ()
			| (stop, ctrls, interp) :: rest ->
				List.iter f (interp start stop ctrls res) ;
				aux stop rest
		in
		aux path.start path.edges

	let iter_edges path f =
		let rec aux prec = function
			| [] -> f prec path.start
			| (stop, _, _) :: e' -> f prec stop ; aux stop e'
		in
		if not (is_empty path) then aux path.start path.edges

    let map_pts f path =
        let edges =
            List.map (fun (stop, ctrls, interp) ->
                let stop', ctrls' = f stop ctrls in
                stop', ctrls', interp) path.edges in
        { path with edges }
	
	let area_min path =
		let s = ref Point.K.zero in
		let add_edge a b = s := Point.K.add !s (Point.area a b) in
		iter_edges path add_edge ;
		Point.K.half !s

    module IsInside = Geom.MakeIsInside (Point.K)
    let is_inside res path point =
        let rec aux start edges f =
            match edges with
            | [] ->
                if start != path.start then f start path.start
            | (stop, [], _interp) :: edges' ->
                f start stop ;
                aux stop edges' f
            | (stop, ctrls, interp) :: edges' ->
                let bbox = Point.Bbox.make start in
                let bbox = Point.Bbox.add bbox stop in
                let bbox = List.fold_left Point.Bbox.add bbox ctrls in
                (match bbox with
                | Point.Bbox.Box ([| _xmi;ymi |], [| xma;yma |]) ->
                    if Point.K.compare point.(0) xma <= 0 &&
                       Point.K.compare point.(1) yma <= 0 &&
                       Point.K.compare point.(1) ymi >= 0 then (
                        (* iter on this edge *)
				        let last = List.fold_left (fun prev next ->
                            f prev next ;
                            next)
                            start
                            (interp start stop ctrls res) in
                        f last stop
                    )
                | _ -> assert false) ;
                aux stop edges' f in
        IsInside.is_inside (aux path.start path.edges) point

	let bbox path =
		let union_ctls bbox ctl = Point.Bbox.add bbox ctl in
		let union_edges bbox (dest, ctls, _) =
			List.fold_left union_ctls (Point.Bbox.add bbox dest) ctls in
		List.fold_left union_edges (Point.Bbox.make path.start) path.edges

	let rounded ?(radius=Point.K.one) paths =
		let first_stop path = match List.hd path.edges with stop, _, _ -> stop in
		let shorten a b d =
			let v = Point.sub b a in
			let r = Point.K.div d (Point.norm v) in
			let ratio = Point.K.sub Point.K.one r in
			Point.add a (Point.mul ratio v) in
		let rec aux path path' start = function
			| [] -> path'
			| [stop, ctrls, interp] ->
				if Point.eq stop path.start then (
					let d1 = shorten start stop radius
					and d2 = shorten (first_stop path) path.start radius in
					let path' = extend path' d1 ctrls interp in
					let path' = extend path' d2 [ stop ] make_bezier_curve in
					{ path' with start = d2 }
				) else path'
			| (stop1, ctrls1, interp1)::(stop2, ctrls2, interp2)::edges ->
				let d1 = shorten start stop1 radius
				and d2 = shorten stop2 stop1 radius in
				let path' = extend path' d1 ctrls1 interp1 in
				let path' = extend path' d2 [ stop1 ] make_bezier_curve in
				aux path path' d2 ((stop2, ctrls2, interp2)::edges) in
		let rounded_single path = aux path (empty path.start) path.start path.edges in
		List.map rounded_single paths
end

module Draw (Path : Geom.PATH) =
struct
	open Path
	module K = Point.K

	let extend_straight t next = extend t next [] make_straight_line
	let ( -- ) = extend_straight

	let rectangle corner00 corner10 =
		let corner01 = [| corner10.(0) ; corner00.(1) |]
		and corner11 = [| corner00.(0) ; corner10.(1) |] in
		(empty corner00) -- corner01 -- corner10 -- corner11 -- corner00

	let rectangle_of_size center width height =
		let diag = [| K.half width ; K.half height |] in
		let corner00 = Point.sub center diag
		and corner10 = Point.add center diag in
		rectangle corner00 corner10
	
	let box corner00 corner10 width =
		let thickness = [| width ; width |] in
		let corner00' = Point.add corner00 thickness
		and corner10' = Point.sub corner10 thickness in
		[ rectangle corner00 corner10 ; inverse (rectangle corner00' corner10') ]
	
end
