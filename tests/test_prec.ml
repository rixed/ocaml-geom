module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)

module Point = Geom_shapes.Point (G.V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Glyph = Text_impl.Glyph (Poly) (Path)

let draw_poly polys =
	let draw_single poly =
		let varray = G.make_vertex_array (Poly.length poly) in
		let idx = ref 0 in
		Poly.iter (fun point ->
			G.vertex_array_set varray !idx point ; incr idx) poly ;
		G.render G.Line_loop varray (G.Uniq G.white) in
	List.iter draw_single polys

let background = View.make_viewable "bg" (fun () -> G.clear ~color:G.black ()) View.identity

let o = Glyph.make 'O'
let bbox_diag = Point.Bbox.diagonal (Glyph.bbox o)

let letters_view =
	let rec build_ppolys ppolys step prec pos =
		if step > 0 then
			let polys   = Glyph.to_polys o prec in
			let ppolys  = (pos, polys) :: ppolys in
			let prec    = G.K.half (G.K.half prec) in
			let pos     = Point.add [| bbox_diag.(0) ; G.K.zero |] pos in
			build_ppolys ppolys (step - 1) prec pos
		else ppolys in
	let ppolys = build_ppolys [] 7 (G.K.of_float 8.) Point.zero in
	let painter = (fun () ->
		List.iter (fun (pos, polys) ->
			G.push_modelview () ;
			G.mult_modelview (G.M.translate pos.(0) pos.(1) G.K.zero) ;
			draw_poly polys ;
			G.pop_modelview ()) ppolys) in
	View.make_viewable ~parent:background "letters"
		painter (View.scaler (fun () -> (G.K.of_float 0.02, G.K.of_float 0.02, G.K.one)))

let camera_pos = ref (G.K.of_float 1.5, G.K.of_float 0.2, G.K.of_float 0.5)
let camera = View.make_viewable ~parent:background "camera"
	(fun () -> ()) (View.translator (fun () -> !camera_pos))

let () = View.display [ (fun () -> View.draw_viewable camera) ]

