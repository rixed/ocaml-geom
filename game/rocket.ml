open Mlrocket

type t =
	{ name  : string ;
	  poly  : Poly.t ;
	  mutable pos    : Point.t ;
	  mutable orient : float * float ;
	  mutable speed  : Vec.t }

let make_shape () =
	let (++) p1 p2 = Path.extend p1 p2 [] Path.make_straight_line in
	let width = K.half K.one in
	let triangle_path = 
		(Path.empty (Point.make_unit 0)) ++
		(Point.of_2scalars (K.neg K.one, width)) ++
		(Point.of_2scalars (K.neg K.one, K.neg width)) in
	Algo.poly_of_path triangle_path K.one

let make () =
	{ name   = "TheRocket!" ;
	  poly   = make_shape () ;
	  pos    = Point.zero ;
	  orient = 1., 0. ;
	  speed  = Vec.zero }

let poly_of_rocket rocket = rocket.poly
let pos_of_rocket rocket () = Point.to_point3 rocket.pos
let orient_of_rocket rocket () = rocket.orient
let set_orient rocket orient = rocket.orient <- orient
