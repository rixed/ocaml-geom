open Mlrocket

let game_clic _world (x, y) =
	Printf.printf "Clic at %f, %f\n%!" x y

let game_painter camera () =
	View.draw_viewable camera

let uni_gc color =
	let faded (r, g, b) = (r *. 0.8, g *. 0.8, b *. 0.8) in
	{ Pic_intf.fill_color = Some (faded color) ; Pic_intf.outline_color = Some color }

let camera_of_world _world =
	let root =
		let bg = Pic.Clear, uni_gc (0.08, 0.08, 0.18) in
		let stars = 
			let rec add_star l n =
				let rand () = (Random.int World.radius) - (K.half World.radius) in
				let point = Point.of_2scalars (rand (), rand ()) in
				let star = Pic.Dot point, uni_gc (1., 1., 1.) in
				if n = 0 then l else add_star (star::l) (n-1) in
			add_star [] 70 in
		View.make_viewable (fun () -> Pic.draw (bg :: stars)) View.identity in
	View.make_viewable
		~parent:root
		(fun () -> ())
		(View.translator (fun () -> 0., 0., 0.5))

let play world =
	let camera = camera_of_world world in
	View.display ~onclic:(game_clic world) [ game_painter camera ]

