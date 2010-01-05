module Make (Point_: Geom.POINT)
	: Geom.PATH with module Point = Point_ =
struct
	module Point = Point_
	type point = Point.t
	type scalar = Point.K.t
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
	type interpolator = point -> point -> point list -> Point.scalar -> point list

	type t = {
		start : point ;
		edges : (point * point list * interpolator) list
	}

	(*
	 * Path manipulation
	 *)

	let empty start = { start = start ; edges = [] }

	let extend path next ctrls interp = (match path.edges with
		| (pt, _, _)::_ -> if Point.eq pt next then
			Format.printf "Path edge of no length at point %a@\n" Point.print pt
		| _ -> () ) ;
		{ path with edges = path.edges @ [next, ctrls, interp] }

	let concat path1 path2 = { start = path1.start ; edges = path1.edges @ path2.edges }

	let size path = List.length path.edges

	let translate path disp =
		let edge_translate (p, ctrls, i) =
			(Point.add p disp, List.map (fun p -> Point.add p disp) ctrls, i) in
		{ start = Point.add path.start disp ; edges = List.map edge_translate path.edges }

	let center path =
		(* FIXME: we should add the center of each edge instead of adding the starting point and every edge's last *)
		let add_pos p (n, _, _) = Point.add p n in
		Point.mul (List.fold_left add_pos path.start path.edges) (Point.K.inv (Point.K.of_int (size path)))

	(* FIXME: move this under ALGO, and use scale_point *)
	let scale path center scale =
		let scale_me p = Point.add center (Point.mul (Point.sub p center) scale) in
		let edge_scale (p, ctrls, i) = (scale_me p, List.map scale_me ctrls, i) in
		{ start = scale_me path.start ; edges = List.map edge_scale path.edges }

	(*
	 * Interpolators
	 *)

	(* Whatever the resolution, a straight line need no intermediary points *)
	let make_straight_line _start _stop _control _res = []

	let rec bezier ctrls res =
		let len = Array.length ctrls in
		let mid_point = Point.half (Point.add ctrls.(0) ctrls.(len-1)) in
		let ctrls_l = Array.make len Point.zero in
		let ctrls_r = Array.make len Point.zero in
		let bino_coef = Array.init len (fun i -> if i = 0 then Point.K.one else Point.K.zero) in
		let divisor = ref Point.K.one in
		for l = 0 to (len-1) do
			let ll = len-1-l in
			(* compute line l (resp. len+1-l) of ctrls_l (resp. ctrls_r) *)
			for c = 0 to (len-1) do
				let cc = len-1-c in
				ctrls_l.(l)  <- Point.add ctrls_l.(l)  (Point.mul ctrls.(c) bino_coef.(c)) ;
				ctrls_r.(ll) <- Point.add ctrls_r.(ll) (Point.mul ctrls.(c) bino_coef.(cc))
			done ;
			ctrls_l.(l)  <- Point.mul ctrls_l.(l)  (Point.K.inv !divisor) ;
			ctrls_r.(ll) <- Point.mul ctrls_r.(ll) (Point.K.inv !divisor) ;
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
			| (stop, ctrls, interp)::rest ->
				List.iter f (interp start stop ctrls res) ;
				aux stop rest
		in
		aux path.start path.edges

end
