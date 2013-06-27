open Algen_intf

module Point (Vector : VECTOR)
	: Geom.POINT with module K = Vector.K =
struct
	include Vector

	let area a b = K.sub (K.mul a.(0) b.(1)) (K.mul a.(1) b.(0))

	let compare_left p0 p1 p =
		let v0 = sub p1 p0 in
		let v1 = sub p p0 in
		K.compare (area v0 v1) K.zero
	
	let compare_segment ?(closed=true) p0 p1 p =
		let vl = sub p0 p1 in
		let vp = sub p0 p in
		let scal = scalar_product vl vp in
		let cmp1 = K.compare scal K.zero in
		if cmp1 < 0 then false else
		if cmp1 = 0 then closed else
		let cmp2 = K.compare (K.square scal) (norm2 vl) in
		if cmp2 > 0 then false else
		if cmp2 = 0 then closed else
		true
	
	let compare_coord c p0 p1 =
		K.compare p0.(c) p1.(c)

	let compare_x = compare_coord 0
	
	let compare_y = compare_coord 1

	let intersect ?(closed=true) p0 p1 q0 q1 =
		let (^^) a b = (* Exclusive logical or *)
			if a then not b else b in
		let at_left a b c = (compare_left a b c) > 0 in
		if p0 == q0 || p1 == q1 then closed else
		(* Only one of {q0,q1} is at left of (p0,p1) *)
		((at_left p0 p1 q0) ^^ (at_left p0 p1 q1)) &&
		(* Only one of {p0,p1} is at left of (q0,q1) *)
		((at_left q0 q1 p0) ^^ (at_left q0 q1 p1))

	let copy p = add p zero

	let center p1 p2 =
		let p = add p1 p2 in
		mul (K.half K.one) p
end

module Polygon
	(Point : Geom.POINT)
	: Geom.POLYGON with module Point = Point =
struct
	module Point = Point
	type e = Point.t

	module Ring = Ring_impl.Ring
    type 'a ring = 'a Ring.t
    include (Ring : Pfds_intf.RING_GEN with type 'a t := 'a ring)

	type t = Point.t ring
	
	let iter_pairs f t =
		let rec aux t1 f n =
			if n > 0 then (
				f t1 ;
				aux (next t1) f (n-1)
			) in
		let len = ref (length t) in
		iterr (fun t0 ->
			decr len ;
			aux (next t0) (f t0) !len) t

    let iter_edges t f =
        iterr (fun t ->
            let p0 = get t and p1 = get (next t) in
            f p0 p1) t

	let print ff poly =
		let focus = get poly in
		Format.pp_open_box ff 0 ;
		Format.pp_print_string ff "{" ; Format.pp_print_space ff () ;
		iter
			(fun point ->
				if point == focus then Format.pp_print_string ff "*" ;
				Point.print ff point ; Format.pp_print_space ff ())
			poly ;
		Format.pp_print_string ff "}" ;
		Format.pp_close_box ff ();

    module IsInside = Geom.MakeIsInside (Point.K)
    let is_inside poly point =
        IsInside.is_inside (iter_edges poly) point
end

