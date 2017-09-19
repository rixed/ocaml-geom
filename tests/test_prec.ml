module K = Algen_impl.FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Glyph = Text_impl.Glyph (Poly) (Path)
module Color = Oaah_color.Make (K)
module Img = Oaah_image.Make (Color)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let draw_polys polys =
  let image = Img.make ~default:Color.white 800 600 in
  (* So far we have polys in between -10 and 10. Move them in the image: *)
  let polys = Algo.scale_poly 4. polys in
  let polys = Algo.translate_poly [| 400.; 300. |] polys in
  Algo.rasterize (Img.poke_scanline image Color.black) polys ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

let o = Glyph.make 'O'
let bbox_diag = Point.Bbox.diagonal (Glyph.bbox o)

let () =
	let rec build_ppolys ppolys step res pos =
		if step > 0 then
			let polys   = Glyph.to_polys ~res o in
			let ppolys  = (pos, polys) :: ppolys in
			let res     = K.half (K.half res) in
			let pos     = Point.add [| bbox_diag.(0) ; K.zero |] pos in
			build_ppolys ppolys (step - 1) res pos
		else ppolys in
	build_ppolys [] 7 (K.of_float 8.) [| ~-.70.; 0. |] |>
  List.map (fun (pos, polys) -> Algo.translate_poly pos polys) |>
  List.concat |>
  draw_polys
