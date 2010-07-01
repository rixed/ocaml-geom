open Mlrocket

type t =
	{ name  : string ;
	  poly  : Poly.t ;
	  mutable pos    : Point.t ;
	  mutable orient : float * float ;
	  mutable thrust : float ;
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
	mlog "\tBuilding rocket..." ;
	{ name   = "TheRocket!" ;
	  poly   = make_shape () ;
	  pos    = Point.zero ;
	  orient = 1., 0. ;
	  thrust = 0. ;
	  speed  = Vec.zero }

let poly rocket = rocket.poly
let pos rocket () = Point.to_point3 rocket.pos
let orient rocket () = rocket.orient
let set_orient rocket orient = rocket.orient <- orient
let set_thrust rocket thrust = rocket.thrust <- thrust

let run gravity dt rocket =
	let dir_x = K.of_float (fst rocket.orient)
	and dir_y = K.of_float (snd rocket.orient) in
	let s = rocket.thrust *. dt in
	let thrust = Vec.mul (Vec.of_2scalars (dir_x, dir_y)) (K.of_float s) in
	let gravity' = K.mul gravity (K.of_float dt) in
	let g = Vec.mul (Vec.normalize rocket.pos) gravity' in
	rocket.speed <- Vec.add rocket.speed (Vec.add thrust g) ;
	rocket.pos   <- Vec.add rocket.pos rocket.speed ;
	(* loose 9/10th of your thrust every second *)
	rocket.thrust <- rocket.thrust *. (0.1**dt)
