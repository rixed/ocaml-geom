module Point (Vector : Geom.VECTOR)
	: Geom.POINT with type t = Vector.t and module K = Vector.K and type scalar = Vector.scalar =
struct
	include Vector

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
	
	let compare_x p0 p1 =
		let (x0, _, _) = to_3scalars p0 in
		let (x1, _, _) = to_3scalars p1 in
		K.compare x0 x1
	
	let compare_y p0 p1 =
		let (_, y0, _) = to_3scalars p0 in
		let (_, y1, _) = to_3scalars p1 in
		K.compare y0 y1

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
end

module Polygon
	(P_: Geom.POINT)
	(Ring : Cnt.GENRING with type elmt = P_.t)
	: Geom.POLYGON with module Point = P_ and type elmt = Ring.elmt =
struct
	module Point = P_
	include Ring
	type t = Point.t ring
	
	let print ff poly =
		let focus = get poly in
		Format.pp_open_box ff 0 ;
		Format.pp_print_string ff "{" ; Format.pp_print_space ff () ;
		iter poly (fun p ->
			let point = get p in
			if point == focus then Format.pp_print_string ff "*" ;
			Point.print ff point ; Format.pp_print_space ff ()) ;
		Format.pp_print_string ff "}" ;
		Format.pp_close_box ff ();
end

