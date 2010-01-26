module K = Geom.CheckedField (Geom_algebr.FloatField)
module V = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (V)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)
module Path = Geom_path.Make (Point)
module Pic = Pic_impl.Make (Poly) (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let unit_sq =
	let gc = { Pic_intf.fill_color = Some (0.8,0.8,0.8) ; Pic_intf.outline_color = Some (1.,1.,1.) } in
	View.make_viewable
		(fun () -> Pic.draw [ Pic.Poly Algo.unit_square, gc ])
		(fun () -> View.identity)

let small_sq_pos = ref (-1.5, 0.2, 0.)
let half_unit_square = Algo.scale_single_poly Algo.unit_square Point.zero (K.half K.one)
let small_sq =
	let gc = { Pic_intf.fill_color = Some (0.8,0.8,0.) ; Pic_intf.outline_color = Some (1.,1.,0.) } in
	View.make_viewable ~parent:unit_sq
		(fun () -> Pic.draw [ Pic.Poly half_unit_square, gc ])
		(fun () -> View.translator small_sq_pos)

let smaller_sq =
	let gc = { Pic_intf.fill_color = Some (0.8,0.,0.8) ; Pic_intf.outline_color = Some (1.,0.,1.) } in
	View.make_viewable ~parent:small_sq
		(fun () -> Pic.draw [ Pic.Poly half_unit_square, gc ])
		(fun () -> View.translator (ref (0.5,0.,0.)))

let camera_pos = ref (0., 0., 0.5)
let camera = View.make_viewable ~parent:unit_sq (fun () -> ()) (fun () -> View.translator camera_pos)

let () = View.display
	~onclic:(fun (x, y) ->
		Format.printf "Clic on %f %f@." x y ;
		small_sq_pos := (x, y, 0.))
	[
		(fun () -> GlClear.clear [`color]) ;
		(fun () -> View.draw_viewable camera)
	]

