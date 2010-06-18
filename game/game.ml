open Mlrocket

let game_clic world (x, y) =
	if abs_float x > 0.1 || abs_float y > 0.1 then (
		let n = sqrt (x*.x +. y*.y) in
		let rocket = List.hd world.World.rockets in
		Rocket.set_orient rocket (x/.n, y/.n)
	)

let game_painter camera () =
	View.draw_viewable camera

let uni_gc color =
	let faded (r, g, b) = (r *. 0.8, g *. 0.8, b *. 0.8) in
	{ Pic_intf.fill_color = Some (faded color) ; Pic_intf.outline_color = Some color }

let pos_of_camera world () =
	let x, y, z = Rocket.pos_of_rocket (List.hd world.World.rockets) () in
	x, y, z+.1.

let camera_of_world world =
	let root =
		let bg = Pic.Clear, uni_gc (0.08, 0.08, 0.18) in
		let stars = 
			let rec add_star l n =
				let rand () = (Random.int (K.double World.radius)) - World.radius in
				let point = Point.of_2scalars (rand (), rand ()) in
				let rand_col () = (Random.float 0.5) +. 0.5 in
				let col = rand_col (), rand_col (), rand_col () in
				let star = Pic.Dot point, uni_gc col in
				if n = 0 then l else add_star (star::l) (n-1) in
			add_star [] 70 in
		View.make_viewable (fun () -> Pic.draw (bg :: stars)) View.identity in
	List.iter (fun rocket ->
		ignore (View.make_viewable
			~parent:root
			(fun () ->
				Pic.draw [ Pic.Poly (Rocket.poly_of_rocket rocket), uni_gc (1., 1., 1.) ])
			(View.trans_orientor
				(Rocket.pos_of_rocket rocket)
				(Rocket.orient_of_rocket rocket))))
		world.World.rockets ;
	(* As we use an ortho projection we can't zoom merely by changing camera altitude. *)
	let lens = View.make_viewable
		~parent:root
		(fun () -> ())
		(View.scaler (fun () -> 0.1, 0.1, 1.)) in
	View.make_viewable
		~parent:lens
		(fun () -> ())
		(View.translator (pos_of_camera world))

let play world =
	let camera = camera_of_world world in
	View.display ~onclic:(game_clic world) [ game_painter camera ]

