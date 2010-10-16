module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)

module Point = Geom_shapes.Point (G.V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Glyph = Text_impl.Glyph (Poly) (Path)
module Word = Text_impl.Word (Glyph)

let draw_poly polys =
	let draw_single poly =
		let varray = G.make_vertex_array (Poly.length poly) in
		let idx = ref 0 in
		Poly.iter (fun point ->
			G.vertex_array_set varray !idx point ; incr idx) poly ;
		G.render G.Line_loop varray (G.Uniq G.white) in
	List.iter draw_single polys

let background = View.make_viewable "bg" (fun () -> G.clear ~color:G.black ()) View.identity

let word_polys =
	let word = Word.make "T.AVO" in
	Word.to_polys word G.K.one

let word_view =
	let painter = (fun () ->
		List.iter (fun (pos, polys) ->
			G.push_modelview () ;
			G.mult_modelview (G.M.translate pos.(0) pos.(1) G.K.zero) ;
			draw_poly polys ;
			G.pop_modelview ()) word_polys) in
	View.make_viewable ~parent:background "word"
		painter (View.scaler (fun () -> (G.K.of_float 0.02, G.K.of_float 0.02, G.K.of_float 0.02)))

let camera_pos = ref (G.K.one, G.K.of_float 0.2, G.K.of_float 0.5)
let camera = View.make_viewable ~parent:background "camera"
	(fun () -> ()) (View.translator (fun () -> !camera_pos))

let () = View.display [ (fun () -> View.draw_viewable camera) ]

