open Algen_intf

module Make (Point : Geom.POINT)
	: Geom.PATH with module Point = Point =
struct
	module Point = Point
	type point = Point.t
(*
let vector_add (ax, ay) (bx, by) = (ax+.bx, ay+.by)
let vector_mul s (ax, ay) = (ax*.s, ay*.s)
let vector_div (ax, ay) d = (ax/.d, ay/.d)
let vector_rotate (cx, cy) angle (px, py) =
	let c = cos angle in
	let s = sin angle in
	let x = px -. cx in
	let y = py -. cy in
	(x*.c -. y*.s +. cx, x*.s +. y*.c +. cy)
let vector_sub (x1, y1) (x2, y2) = (x2-.x1, y2-.y1)
let vector_zero = (0., 0.)


(* Displace point p by vector v *)
let point_disp v p = vector_add p v
(* Distance from a to b *)
let point_dist a b = vector_sub b a
let point_scale center scale p = vector_add center (vector_mul scale (vector_sub p center))
*)
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

	let empty start = { start = start ; edges = [] }

	let is_empty path = path.edges = []

	let is_closed path =
		let rec last_point = function
			| [] -> path.start
			| [ p, _, _ ] -> p
			| _ :: e' -> last_point e' in
		let last = last_point path.edges and first = path.start in
		Point.eq first last

	let extend path next ctrls interp = (match path.edges with
		| (pt, _, _) :: _ -> if 0 = Point.compare pt next then
			Format.printf "Path edge of no length at point %a@\n" Point.print pt
		| _ -> ()) ;
		{ path with edges = path.edges @ [next, ctrls, interp] }

	let concat path1 path2 = { start = path1.start ; edges = path1.edges @ path2.edges }

	let size path = List.length path.edges

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
		Point.mul (Point.K.inv (Point.K.of_int (size path))) (List.fold_left add_pos path.start path.edges)

	(* FIXME: move this under ALGO, and use scale_point. Wait, then algo would know internal structure ? *)
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

	let iter path res f =
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
	
	let area_min path =
		let s = ref Point.K.zero in
		let add_edge a b = s := Point.K.add !s (Point.area a b) in
		iter_edges path add_edge ;
		!s

	let bbox path =
		let union_ctls bbox ctl = Point.Bbox.add bbox ctl in
		let union_edges bbox (dest, ctls, _) =
			List.fold_left union_ctls (Point.Bbox.add bbox dest) ctls in
		List.fold_left union_edges (Point.Bbox.make path.start) path.edges

end
