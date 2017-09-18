module K = Algen_impl.FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Draw = Geom_path.Draw (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Color = Oaah_color.Make (K)
module Img = Oaah_image.Make (Color)

let box = Algo.poly_of_ascii_repr [
    "   a     g   " ;
    "             " ;
    "           f " ;
    "b            " ;
    "      e      " ;
    "             " ;
    "   c     d   " ]

let _ = Format.printf "box = %a@." Poly.print box

let draw_polys =
  let polys = [ box ] in
  let image = Img.make ~default:Color.white 800 600 in
  (* So far we have polys in between -10 and 10. Move them in the image: *)
  let polys = Algo.scale_poly polys [|0.;0.|] 20. in
  let polys = Algo.translate_poly polys [| 300.; 250. |] in
  Algo.rasterize polys (Img.poke_scanline image Color.black) ;
  Img.open_graph image ;
  Img.draw image ;
  let open Graphics in
  let rec event_loop () =
    let e = wait_next_event [Button_down; Key_pressed] in
    if e.button then (
      let p = [| K.of_int e.mouse_x; K.of_int e.mouse_y |] in
      let ins = Poly.is_inside (List.hd polys) p in
      Format.printf "%a %a -> %s@." K.print p.(0) K.print p.(1) (if ins then "inside" else "outside") ;
      event_loop ()
    ) in
  event_loop () ;
  close_graph ()
