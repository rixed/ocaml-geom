open Mlrocket

type t =
	{ name  : string ;
	  poly  : Poly.t ;
	  mutable pos    : Point.t ;
	  mutable orient : float * float ;
	  mutable speed  : float }

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
	  speed  = 0. }

let poly rocket = rocket.poly
let pos rocket () = Point.to_point3 rocket.pos
let orient rocket () = rocket.orient
let set_orient rocket orient = rocket.orient <- orient
let set_speed rocket speed = rocket.speed <- speed

let run rocket =
	let dir_x = K.of_float (fst rocket.orient)
	and dir_y = K.of_float (snd rocket.orient) in
	let speed = Vec.mul (Vec.of_2scalars (dir_x, dir_y)) (K.of_float rocket.speed) in
	rocket.pos <- Vec.add rocket.pos speed
