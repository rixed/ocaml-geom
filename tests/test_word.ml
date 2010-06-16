module K = Geom.CheckedField (Geom_algebr.FloatField)
module V = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (V)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)
module Path = Geom_path.Make (Point)
module Painter = View_simple.Make_painter (Poly)
module Glyph = Text_impl.Glyph (Poly) (Path)
module Word = Text_impl.Word (Glyph)

let background = View.make_viewable Painter.draw_background View.identity

let word_polys =
	let word = Word.make "T.AVO" in
	Word.to_poly word 1.

let word_view =
	let painter = (fun () -> Painter.draw_poly word_polys) in
	View.make_viewable ~parent:background painter (View.scaler (fun () -> 0.02, 0.02, 0.02))

let camera_pos = ref (1., 0.2, 0.5)
let camera = View.make_viewable ~parent:background (fun () -> ()) (View.translator (fun () -> !camera_pos))

let () = View.display [ (fun () -> View.draw_viewable camera) ]

