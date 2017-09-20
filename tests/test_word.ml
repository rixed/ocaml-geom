module K = Algen_impl.FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
module P = Geom_shapes.Point (V)
module Color = Oaah_color.Make (K)
module Img = Oaah_image.Make (Color)
module Poly = Geom_shapes.Polygon (P)
module Path = Geom_path.Make (P)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Glyph = Text_impl.Glyph (Poly) (Path)
module Word = Text_impl.Word (Glyph)

let draw_polys polys =
  let image = Img.make ~default:Color.white 800 600 in
  (* So far we have polys in between -10 and 10. Move them in the image: *)
  let polys = Algo.scale_poly 4. polys in
  let polys = Algo.translate_poly [| 400.; 300. |] polys in
  Algo.rasterize (Img.poke_scanline image Color.black 1.) polys ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

let word_polys =
  let word = Word.make "T.AVO" in
  Word.to_polys ~res:K.one word |>
  List.map (fun (pos, polys) -> Algo.translate_poly pos polys) |>
  List.concat

let () =
  draw_polys word_polys
