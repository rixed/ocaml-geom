module K = Geom.CheckedField (Geom_algebr.FloatField)
module V = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (V)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)
module Path = Geom_path.Make (Point)
module Pic = Pic_impl.Make (Poly) (Path)
module Plot = Plot_impl.Make (Pic)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Painter = View_simple.Make_painter (Poly)

let plot_view =
	let plot = Plot.make_dataset ~label:"temp" (fun f ->
		let sq x = x *. x in
		for x = -5 to 5 do f (float x) (sq (float x)) done) in
	View.make_viewable (fun () -> Pic.draw (Plot.pic_of_plot ~scale:(2., 1.) [plot])) (fun () -> View.identity)

let camera_pos = ref (1., 0.2, 0.5)
let camera = View.make_viewable ~parent:plot_view (fun () -> ()) (fun () -> View.translator camera_pos)

let () = View.display
	[
		Painter.draw_background ;
		(fun () -> View.draw_viewable camera)
	]


