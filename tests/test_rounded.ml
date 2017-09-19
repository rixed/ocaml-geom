module K = Algen_impl.FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Draw = Geom_path.Draw (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Color = Oaah_color.Make (K)
module Img = Oaah_image.Make (Color)

let draw_poly polys =
  let image = Img.make ~default:Color.white 800 600 in
  (* Zoom: *)
  let polys = Algo.scale_poly 10. polys in
  let polys = Algo.translate_poly [| 400.; 300. |] polys in
  Algo.rasterize (Img.poke_scanline image Color.black) polys ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

let letters_view =
	let box = Draw.box
		[| K.of_float (-10.) ; K.of_float (-5.) |]
		[| K.of_float 10. ; K.of_float 5. |]
		(K.of_float 0.3) in
	List.map (fun (radius, y) ->
		let pos = [| K.zero ; K.of_float y |]
		and radius = K.of_float radius in
		let paths = Path.rounded ~radius box in
		let res = K.of_float 0.01 in
		pos, Algo.polys_of_paths ~res paths)
    [ 0.1, -15. ; 1., 0. ; 2.5, 15. ] |>
  List.map (fun (pos, polys) -> Algo.translate_poly pos polys) |>
  List.concat |>
  draw_poly
