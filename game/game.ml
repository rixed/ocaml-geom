open Mlrocket

let fps = 15.

let matrix_mult m (x, y, z) =
    let ( * ) = K.mul and ( + ) = K.add in
	[| x * m.(0).(0) + y * m.(1).(0) + z * m.(2).(0) + m.(3).(0) ;
	   x * m.(0).(1) + y * m.(1).(1) + z * m.(2).(1) + m.(3).(1) ;
	   x * m.(0).(2) + y * m.(1).(2) + z * m.(2).(2) + m.(3).(2) |]

let game_clic world camera = function
    | G.Clic (x, y, w, h) ->
        (* we cannot easily use unproject here because we want the coordinate
         * of the click on the base attached to the rocket but that does not
         * rotate with it (same orientation than root). This viewable does not
         * exist. *)
        let x = float_of_int (x - w/2) /. float_of_int (w/2)
        and y = float_of_int (y - h/2) /. float_of_int (h/2) in
        let y = -. y in
        if abs_float x > 0.1 || abs_float y > 0.1 then
        let rocket = List.hd world.World.rockets in
        (* get the matrix transforming coords from camera to root *)
        let m = View.get_transform ~src:camera () in
        let clickpos = matrix_mult m (K.of_float x, K.of_float y, K.zero) in
        let pos = Rocket.pos rocket in
        let pos' = Point.sub clickpos pos in
        let n = Point.norm pos' in
        Rocket.set_orient rocket (Point.mul (K.inv n) pos') ;
        Rocket.set_thrust rocket (K.mul n (K.of_float 0.03))
    | _ -> ()

let game_painter camera () =
	View.draw_viewable camera

let uni_gc color =
	let faded c = G.C.mul (K.of_float 0.8) c in
	{ Pic.fill_color = Some (G.Uniq (faded color)) ; Pic.outline_color = Some (G.Uniq color) }

let pos_of_camera world =
	let rocket = List.hd world.World.rockets in
    Rocket.pos rocket

let ppos_of_camera world =
    let rocket = List.hd world.World.rockets in
    Rocket.prev_pos rocket

let zoom_of_camera =
	let speed_to_zoom = K.of_float 5. in
	let min_zoom = K.of_float 10. in
	let dt = K.inv (K.of_float fps) in
	fun world -> 
		let npos = pos_of_camera world in
	    let ppos = ppos_of_camera world in
        let dist = Point.norm (Point.sub npos ppos) in
        let speed = K.div dist dt in
		let zoom = K.add (K.mul speed speed_to_zoom) min_zoom in
		zoom

let orient_of_camera world =
	let rocket = List.hd world.World.rockets in
	let pos = Rocket.pos rocket in
	let n = Point.norm pos in
	(* we are not supposed to reach that far, but that's the starting position for now :-) *)
	if K.to_float n > 0.1 then
        Point.mul (K.inv n) [| K.neg pos.(1) ; pos.(0) |]
    else
        [| K.one ; K.zero |]

let clock_dt =
    let last = ref None in
    fun () -> match !last with
        | None -> 0.01
        | Some t ->
            let n = Unix.gettimeofday () in
            let dt = n -. t in
            last := Some n ;
            dt

let camera_of_world world =
	let root =
		let bg = Pic.Clear, uni_gc [| K.of_float 0.08 ; K.of_float 0.08 ; K.of_float 0.18 |] in
		let stars = 
			let rec add_star l n =
                if n = 0 then l else
				let rand () = K.sub (K.of_float (Random.float (K.to_float (K.double world.World.radius)))) world.World.radius in
				let point =
					let rad2 = K.square world.World.radius in
					let rec aux () =
						let p = [| rand () ; rand () |] in
						if K.compare (Point.norm2 p) rad2 < 0 then p
						else aux () in
					aux () in
				let rand_col () = K.of_float ((Random.float 0.5) +. 0.5) in
				let col = [| rand_col () ; rand_col () ; rand_col () |] in
				let star = Pic.Dot point, uni_gc col in
				add_star (star::l) (n-1) in
			let stars_density = 0.01 in
			let pi = K.of_float (4. *. atan 1.) in
			let world_surface = K.to_float (K.mul pi (K.square world.World.radius)) in
			let nb_stars = int_of_float (stars_density *. world_surface) in
			mlog "\tAdding %d stars..." nb_stars ;
			add_star [] nb_stars in
		let ground =
			Pic.Path world.World.ground, uni_gc [| K.one ; K.one ; K.one |] in
		View.make_viewable "root" (fun () ->
            (* Profit from the start of painting to update world *)
            let dt = clock_dt () in
            World.run (K.of_float dt) world ;
            Pic.draw ~prec:World.prec (bg :: ground :: stars))
            View.identity in
	List.iter
		(fun rocket ->
			Rocket.set_viewable rocket (View.make_viewable
				~parent:root "a rocket"
				(fun () ->
                    Pic.draw [ Pic.Poly (Rocket.poly rocket), uni_gc [| K.one ; K.one ; K.one |] ])
				(View.trans_orientor
                    (fun () ->
                        let pos = Rocket.pos rocket in
                        pos.(0), pos.(1), K.zero)
                    (fun () ->
                        let o = Rocket.orient rocket in
                        o.(0), o.(1)))))
		world.World.rockets ;
	let rocket_follower = View.make_viewable
		~parent:root "rocket follower"
		(fun () -> ())
		(View.trans_orientor
			(fun () ->
				let pos = pos_of_camera world in
				pos.(0), pos.(1), K.one)
			(fun () ->
                let o = orient_of_camera world in
                o.(0), o.(1))) in
	View.make_viewable
		~parent:rocket_follower "camera"
		(fun () -> ())
		(View.scaler
			(fun () -> let zoom = zoom_of_camera world in zoom, zoom, K.one))

let play world =
	let camera = camera_of_world world in
	View.display
		~on_event:(game_clic world camera)
		[ game_painter camera ]

