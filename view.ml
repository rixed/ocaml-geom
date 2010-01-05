module type PAINTER =
sig
	module Poly : Geom.POLYGON

	val draw_vector : Poly.Point.t -> Poly.Point.t -> unit
	val draw_point : Poly.Point.t -> unit
	val draw_single_poly : Poly.t -> unit
	val draw_poly : Poly.t list -> unit
	val draw_background : unit -> unit
	val draw_turtle : Poly.Point.t -> Poly.Point.t -> unit
end

type painter = unit -> unit

(* FIXME: protect with a mutex ? *)
let last_clics = ref []

let win_size = ref (640, 480)
let znear_size = ref (1., 1.)
let altitude = 7.5

let unproject x y =
	(* GluUnproject is full of bugs from arch to arch *)
	let xf = (fst !znear_size)*.(2.*.(float_of_int x) /. (float_of_int (fst !win_size)) -. 1.) in
	let yf = (snd !znear_size)*.(1. -. 2.*.(float_of_int y) /. (float_of_int (snd !win_size))) in
	(altitude *. xf, altitude *. yf, -.altitude)

let display painters =
	ignore (Glut.init ~argv:Sys.argv) ;
	Glut.initDisplayMode ~alpha:false ~double_buffer:true ~depth:false () ;
	Glut.initWindowSize ~w:(fst !win_size) ~h:(snd !win_size) ;
	ignore (Glut.createWindow ~title:"Geometry Viewer") ;
    Glut.reshapeFunc ~cb:(fun ~w ~h ->
		win_size := (w, h) ;
		GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h ;
		GlMat.mode `projection ;
		GlMat.load_identity () ;
		let r = float w /. float h in
		let r' = 1. /. r in
		if (w>h) then (
			GlMat.frustum ~x:(-. r, r) ~y:(-1., 1.) ~z:(1., altitude +. 1.) ;
			znear_size := (r, 1.)
		) else (
			GlMat.frustum ~x:(-1., 1.) ~y:(-.r', r') ~z:(1., 10.) ;
			znear_size := (1., r')
		) ;
		GlMat.mode `modelview ;
		GlMat.load_identity() ;
		GlMat.translate ~z:(-.altitude) ()) ;
	Glut.mouseFunc ~cb:(fun ~button ~state ~x ~y ->
		let ox, oy, _oz = unproject x y in (*GluMat.unproject (float_of_int x, float_of_int ((snd !win_size) - y), 0.) in*)
		(*Printf.printf "%d, %d unproject to %f %f %f\n%!" x y ox oy oz ;*)
		if button = Glut.LEFT_BUTTON && state = Glut.DOWN then (
			last_clics := (ox, oy)::!last_clics ;
			Glut.postRedisplay ()
		)) ;
    Glut.displayFunc ~cb:(fun () ->
		List.iter
			(fun painter ->
				GlMat.push () ;
				painter () ;
				GlMat.pop ())
			painters ;
		Glut.swapBuffers()) ;
	Glut.mainLoop ()

