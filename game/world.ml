open Mlrocket

(* Note : the space is incurved the other way around, so that the ground surrounds us. *)
let radius = K.of_int 30

type t =
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	}

let make_ground () =
	(* Start from a mere "sphere" *)
	let up    = Point.make_unit 1 in
	let down  = Point.sub Point.zero up in
	let right = Point.make_unit 0 in
	let left  = Point.sub Point.zero right in
	let upper_right  = Point.add right up in
	let bottom_right = Point.add right down in
	let upper_left   = Point.add left  up in
	let bottom_left  = Point.add left  down in
	let circ_0 = Path.extend (Path.empty left) up    [ upper_left ]   Path.make_bezier_curve in
	let circ_1 = Path.extend circ_0            right [ upper_right ]  Path.make_bezier_curve in
	let circ_2 = Path.extend circ_1            down  [ bottom_right ] Path.make_bezier_curve in
	let circ_3 = Path.extend circ_2            left  [ bottom_left ]  Path.make_bezier_curve in
	Path.scale circ_3 Point.zero radius

let make () =
	{ ground = make_ground () ;
	  rockets = [ Rocket.make () ] ;
	  gravity = Random.int fixed_prec }

let run dt world = List.iter (Rocket.run dt) world.rockets

