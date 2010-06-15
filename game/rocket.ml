open Mlrocket

type t =
	{ name : string ;
	  shape : Poly.t ;
	  position : Point.t ;
	  speed : Vec.t }

let make_shape () =
	let (++) p1 p2 = Path.extend p1 p2 [] Path.make_straight_line in
	let width = K.half K.one in
	let triangle_path = 
		(Path.empty (Point.make_unit 1)) ++
		(Point.of_2scalars (K.neg width, K.neg K.one)) ++
		(Point.of_2scalars (width, K.neg K.one)) in
	Algo.poly_of_path triangle_path K.one

let make () =
	{ name = "TheRocket!" ;
	  shape = make_shape () ;
	  position = Point.zero ;
	  speed = Vec.zero }
