open Mlrocket

type t =
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	;
      radius : K.t ;
      mutable max_speed : K.t }

let randomize_path res path =
	mlog "\t\tRandomizing ground..." ;
	let path' = ref None in
	let last_ctrl = ref Point.zero in
	let last_p = ref Point.zero in
	let add_segment p =
		let r = (K.to_float res) *. 1. in
		let half_r = r /. 2. in
		let rand () = K.of_float ((Random.float r) -. half_r) in
		let rand_dec = [| rand () ; rand () |] in
		match !path' with
		| None ->
			path' := Some (Path.empty p) ;
			last_ctrl := rand_dec ;
			last_p := p ;
		| Some path'' ->
            let right_turn v = [| v.(1) ; K.neg v.(0) |] in
			let rand_pt = right_turn (G.V.add rand_dec (G.V.sub p !last_p)) in
			path' := Some (Path.extend path'' p [Point.sub !last_p !last_ctrl ; Point.add p rand_pt] Path.make_bezier_curve) ;
			last_ctrl := rand_pt ;
			last_p := p in
	Path.iter res path add_segment ;
	Bricabrac.unopt !path'

(* Note : the space is incurved the other way around, so that the ground surrounds us. *)
let make_ground ~radius =
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
	
let make ~radius =
    let radius = K.of_int radius in
	mlog "Building world of radius %a..." K.print radius ;
	let gravity = K.of_float 0.1 in
	mlog "\tGravity = %a" K.print gravity ;
	{ ground = make_ground ~radius ;
	  rockets = [ Rocket.make (Point.mul (K.half radius) (Point.make_unit 0)) ] ;
	  gravity ;
      radius ;
      max_speed = K.zero }

let prec = K.of_float 0.5

let run dt world =
    List.iter (fun rocket ->
        (* move rocket *)
        Rocket.run world.gravity dt rocket ;
        (* check collision with ground *)
        if Poly.exists (fun p ->
            let m = View.get_transform ~src:(Rocket.viewable rocket) () in
            let p' = G.M.mul_vec m [| p.(0) ; p.(1) ; K.one ; K.one |] in
            not (Algo.is_inside_path prec world.ground p')) (Rocket.poly rocket
        ) then (
            mlog "boum!" ;
            mlog "Your max speed was %a" K.print (K.sqrt world.max_speed) ;
            exit 0
        ) else (
            (* record max speed *)
            let speed = G.V.norm2 (Rocket.speed rocket) in
            if K.compare speed world.max_speed > 0 then world.max_speed <- speed
        ))
        world.rockets

