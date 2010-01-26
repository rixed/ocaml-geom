module K = Geom.CheckedField (Geom_algebr.FloatField)
module V = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (V)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)
module Path = Geom_path.Make (Point)
module Pic = Pic_impl.Make (Poly) (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let point_of_floats (x, y) = Point.of_2scalars (K.of_float x, K.of_float y)
let poly_of_floats floats = Algo.poly_of_points (List.map point_of_floats floats)
let path_of_floats floats = Algo.path_of_points (List.map point_of_floats floats)

let uni_gc color =
	let faded (r, g, b) = (r *. 0.8, g *. 0.8, b *. 0.8) in
	{ Pic_intf.fill_color = Some (faded color) ; Pic_intf.outline_color = Some color }

let root =
	let ground = Pic.Poly (poly_of_floats [ -2., -1. ; 2., -1. ; 2., 0. ; -2., 0. ]), uni_gc (0.2, 0.6, 0.1) in
	let sky = Pic.Poly (poly_of_floats [ -2., 0. ; 2., 0. ; 2., 1. ; -2., 1. ]), uni_gc (0.4, 0.4, 1.) in
	View.make_viewable
		(fun () -> Pic.draw [ ground ; sky ])
		(fun () -> View.identity)

let small_sq_pos = ref (-1.5, 0.2, 0.)
let half_unit_square = Algo.scale_single_poly Algo.unit_square Point.zero (K.half K.one)
let small_sq = View.make_viewable ~parent:root
	(fun () -> Pic.draw [ Pic.Poly half_unit_square, uni_gc (1., 1., 0.) ])
	(fun () -> View.translator small_sq_pos)

let smaller_sq = View.make_viewable ~parent:small_sq
	(fun () -> Pic.draw [ Pic.Poly half_unit_square, uni_gc (1., 0., 1.)])
	(fun () -> View.translator (ref (0.5,0.,0.)))

let camera_pos = ref (0., 0., 0.5)
let camera = View.make_viewable ~parent:root (fun () -> ()) (fun () -> View.translator camera_pos)

let () = View.display
	~onclic:(fun (x, y) -> small_sq_pos := (x, y, 0.))
	[
(*		(fun () -> GlClear.clear [`color]) ;*)
		(fun () -> View.draw_viewable camera)
	]

