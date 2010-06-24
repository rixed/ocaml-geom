open Mlrocket

(* Note : the space is incurved the other way around, so that the ground surrounds us. *)
let radius = K.of_int 10

type t =
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	}

let make_ground () =
	(* Start from a mere "sphere" *)
	let upper_right  = Point.add (Point.make_unit 0) (Point.make_unit 1) in
	let bottom_right = Point.sub (Point.make_unit 0) (Point.make_unit 1) in
	let upper_left   = Point.sub (Point.make_unit 1) (Point.make_unit 0) in
	let bottom_left  = Point.mul upper_right (K.neg K.one) in
	let left  = Point.mul (Point.make_unit 0) (K.neg K.one) in
	let right = Point.make_unit 1 in
	let upper_circle = Path.extend
		(Path.empty left) right
		[ upper_left ; upper_right ] Path.make_bezier_curve in
	let circle = Path.extend
		upper_circle left [ bottom_right ; bottom_left ] Path.make_bezier_curve in
	Path.scale circle Point.zero radius

let make () =
	{ ground = make_ground () ;
	  rockets = [ Rocket.make () ] ;
	  gravity = Random.int fixed_prec }

let run dt world = List.iter (Rocket.run dt) world.rockets

