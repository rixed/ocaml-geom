open Mlrocket

let game_clic _world (x, y) =
	Printf.printf "Clic at %f, %f\n%!" x y

let game_painter camera () =
	View.draw_viewable camera

let uni_gc color =
	let faded (r, g, b) = (r *. 0.8, g *. 0.8, b *. 0.8) in
	{ Pic_intf.fill_color = Some (faded color) ; Pic_intf.outline_color = Some color }

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
	let rockets = List.map
		(fun rocket ->
			View.make_viewable
				~parent:root
				(fun () -> Pic.draw [ Pic.Poly rocket.Rocket.poly, uni_gc (1., 1., 1.) ])
				(View.translator (fun () -> Point.to_point3 rocket.Rocket.pos)))
		world.World.rockets in
	(* As we use an ortho projection we can't zoom merely by changing camera altitude. *)
	let lens = View.make_viewable
		~parent:(List.hd rockets)
		(fun () -> ())
		(View.scaler (fun () -> 0.1, 0.1, 1.)) in
	View.make_viewable
		~parent:lens
		(fun () -> ())
		(View.translator (fun () -> 0., 0., 1.))

let play world =
	let camera = camera_of_world world in
	View.display ~onclic:(game_clic world) [ game_painter camera ]

