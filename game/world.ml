open Mlrocket

(* Note : the space is incurved the other way around, so that the ground surrounds us. *)
let radius = K.of_int 100

type t =
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	}

let randomize_path res path =
	mlog "\t\tRandomizing ground..." ;
	let path' = ref None in
	let last_ctrl = ref Point.zero in
	let last_p = ref Point.zero in
	let add_segment p =
		let r = (K.to_float res) *. 1. in
		let half_r = r /. 2. in
		let rand () = K.of_float ((Random.float r) -. half_r) in
		let rand_dec = Point.of_2scalars (rand (), rand ()) in
		match !path' with
		| None ->
			path' := Some (Path.empty p) ;
			last_ctrl := rand_dec ;
			last_p := p ;
		| Some path'' ->
			let rand_pt = Vec.right_turn (Vec.add rand_dec (Vec.sub p !last_p)) in
			path' := Some (Path.extend path'' p [Point.sub !last_p !last_ctrl ; Point.add p rand_pt] Path.make_bezier_curve) ;
			last_ctrl := rand_pt ;
			last_p := p in
	Path.iter path res add_segment ;
	Cnt.unopt !path'

let make_ground () =
	mlog "\tBuilding ground..." ;
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
	let circle = Path.extend circ_2            left  [ bottom_left ]  Path.make_bezier_curve in
	let grnd_0 = Path.scale circle Point.zero radius in
	let grnd_1 = randomize_path (K.of_float 9.) grnd_0 in
	let ground = randomize_path (K.of_float 3.) grnd_1 in
	ground
	

let make () =
	mlog "Building world of radius %a..." K.print radius ;
	let gravity = K.of_float 0.1 in
	mlog "\tGravity = %a" K.print gravity ;
	{ ground = make_ground () ;
	  rockets = [ Rocket.make () ] ;
	  gravity = gravity }

let run dt world = List.iter (Rocket.run world.gravity dt) world.rockets

