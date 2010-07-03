open Mlrocket

let fps = 5

let matrix_mult m (x, y, z) =
	x *. m.(0).(0) +. y *. m.(1).(0) +. z *. m.(2).(0) +. m.(3).(0),
	x *. m.(0).(1) +. y *. m.(1).(1) +. z *. m.(2).(1) +. m.(3).(1),
	x *. m.(0).(2) +. y *. m.(1).(2) +. z *. m.(2).(2) +. m.(3).(2)

let game_clic world camera (x, y) =
	if abs_float x > 0.1 || abs_float y > 0.1 then
	let rocket = List.hd world.World.rockets in
	(* get the matrix transforming coords from camera to root *)
	let m = View.get_transform ~src:camera () in
	let x', y', _ = matrix_mult m (x, y, 0.) in
	let rx, ry, _ = Vec.to_point3 (Rocket.pos rocket) in
	let x'', y'' = x' -. rx, y' -. ry in
	let n = sqrt (x''*.x'' +. y''*.y'') in
	Rocket.set_orient rocket (x''/.n, y''/.n) ;
	Rocket.set_thrust rocket (n *. 0.03)

let game_painter camera () =
	View.draw_viewable camera

let uni_gc color =
	let faded (r, g, b) = (r *. 0.8, g *. 0.8, b *. 0.8) in
	{ Pic_intf.fill_color = Some (faded color) ; Pic_intf.outline_color = Some color }

let pos_of_camera world =
	let rocket = List.hd world.World.rockets in
	let camera_z = Vec.of_3scalars (K.zero, K.zero, K.one) in
	Vec.add (Rocket.pos rocket) camera_z 

let zoom_of_camera =
	let speed_to_zoom = K.of_float 5. in
	let min_zoom = K.of_float 7. in
	let prev_pos = ref None in
	let dt = K.inv (K.of_int fps) in
	(fun world -> 
		let npos = pos_of_camera world in
		let zoom = match !prev_pos with
			| None -> 20.
			| Some ppos ->
				let dist = Vec.norm (Vec.sub npos ppos) in
				let speed = K.div dist dt in
				K.to_float (K.add (K.mul speed speed_to_zoom) min_zoom) in
		prev_pos := Some npos ;
		zoom)

let orient_of_camera world () =
	let rocket = List.hd world.World.rockets in
	let x, y, _ = Vec.to_point3 (Rocket.pos rocket) in
	let n = sqrt (x*.x +. y*.y) in
	(* we are not supposed to reach that far, but that's the starting position for now :-) *)
	if n > 0.1 then -. y /. n, x /. n else 1., 0.

let camera_of_world world =
	mlog "Generating world..." ;
	let root =
		let bg = Pic.Clear, uni_gc (0.08, 0.08, 0.18) in
		let stars = 
			let rec add_star l n =
				let rand () = (Random.int (K.double World.radius)) - World.radius in
				let point =
					let rad2 = K.square World.radius in
					let rec aux () =
						let p = Point.of_2scalars (rand (), rand ()) in
						if K.compare (Point.norm2 p) rad2 < 0 then p
						else aux () in
					aux () in
				let rand_col () = (Random.float 0.5) +. 0.5 in
				let col = rand_col (), rand_col (), rand_col () in
				let star = Pic.Dot point, uni_gc col in
				if n = 0 then l else add_star (star::l) (n-1) in
			let stars_density = 0.01 in
			let pi = K.of_float (4. *. atan 1.) in
			let world_surface = K.to_float (K.mul pi (K.square World.radius)) in
			let nb_stars = int_of_float (stars_density *. world_surface) in
			mlog "\tAdding %d stars for surface = %f..." nb_stars world_surface ;
			add_star [] nb_stars in
		let ground =
			Pic.Path world.World.ground, uni_gc (1., 1., 1.) in
		View.make_viewable "root" (fun () -> Pic.draw ~prec:(K.of_float 0.5) (bg :: ground :: stars)) View.identity in
	List.iter
		(fun rocket ->
			ignore (View.make_viewable
				~parent:root "a rocket"
				(fun () -> Pic.draw [ Pic.Poly (Rocket.poly rocket), uni_gc (1., 1., 1.) ])
				(View.trans_orientor (fun () -> Vec.to_point3 (Rocket.pos rocket)) (Rocket.orient rocket))))
		world.World.rockets ;
	let rocket_follower = View.make_viewable
		~parent:root "rocket follower"
		(fun () -> ())
		(View.trans_orientor
			(fun () ->
				let x, y, z = Vec.to_point3 (pos_of_camera world) in
				x, y, z +.1.)
			(orient_of_camera world)) in
	(* As we use an ortho projection we can't zoom merely by changing camera altitude. *)
	View.make_viewable
		~parent:rocket_follower "camera"
		(fun () -> ())
		(View.scaler
			(fun () -> let zoom = zoom_of_camera world in zoom, zoom, 1.))

let play world =
	let camera = camera_of_world world in
	mlog "Fps = %d" fps ;
	View.display
		~onclic:(game_clic world camera)
		~timer:(fun () -> World.run (1./.(float_of_int fps)) world) ~fps:fps
		[ game_painter camera ]

