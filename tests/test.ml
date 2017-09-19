(* Test the shipped implementations. *)
module K = Algen_impl.FloatField
module V = Algen_vector.Make (K) (struct let v = 2 end)
module P = Geom_shapes.Point (V)
module Color = Oaah_color.Make (K)
module Img = Oaah_image.Make (Color)

let make_point x y = [| K.of_float x ; K.of_float y |]

module Poly = Geom_shapes.Polygon (P)

module Path = Geom_path.Make (P)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Glyph = Text_impl.Glyph (Poly) (Path)

let make_poly parr =
  let rec add_point pol n =
    if n >= Array.length parr then pol
    else add_point (Poly.insert_after pol parr.(n)) (n+1) in
  add_point Poly.empty 0

let square = make_poly [|
  make_point (-1.) (-1.) ;
  make_point 1. (-1.) ;
  make_point 1. 1. ;
  make_point (-1.) 1.
|] 

let test_path = 
  Path.empty (make_point (-1.) 1.) |>
  Path.bezier_to (make_point 1. 1.) [ make_point (-1.) (-2.) ; make_point 1. (-2.) ]

let letter_a =
  Algo.scale_poly
    (K.of_float 0.1)
    (Glyph.to_polys ~res:(K.of_float 1.2) (Glyph.make 'a'))

let polys =
  let polys_list = [
    [ square ] ;
    [ make_poly [|
      make_point (-1.) (-1.) ;
      make_point 1. (-1.) ;
      make_point 1. 1. ;
      make_point 0.5 1. ;
      make_point 0.5 (-0.5) ;
      make_point (-0.5) (-0.5) ;
      make_point (-0.5) 1. ;
      make_point (-1.) 1.
    |] ] ;
    [ make_poly [|
      make_point (-1.) (-1.) ;
      make_point 1. (-1.) ;
      make_point 0.8 1. ;
      make_point 0.6 (-0.5) ;
      make_point 0.4 1. ;
      make_point 0.2 (-0.5) ;
      make_point 0. 1. ;
      make_point (-0.2) (-0.5) ;
      make_point (-0.4) 1. ;
      make_point (-0.6) (-0.5) ;
      make_point (-0.8) 1.
    |] ] ;
    [ make_poly [|
      make_point (-1.) 0.5 ;
      make_point (-0.7) 0.5 ;
      make_point (-0.7) 0.7 ;
      make_point 0.7 0.7 ;
      make_point 0.7 0.15 ;
      make_point (-0.7) 0.15 ;  (* 5 *)
      make_point (-1.) (-0.15) ;
      make_point (-1.) (-0.7) ;
      make_point (-0.7) (-1.) ;
      make_point 0.7 (-1.) ;
      make_point 1. (-0.7) ;  (* 10 *)
      make_point 1. (-0.5) ;
      make_point 0.7 (-0.5) ;
      make_point 0.7 (-0.7) ;
      make_point (-0.7) (-0.7) ;
      make_point (-0.7) (-0.15) ; (* 15 *)
      make_point 0.7 (-0.15) ;
      make_point 1. 0.15 ;
      make_point 1. 0.7 ;
      make_point 0.7 1. ;
      make_point (-0.7) 1. ;  (* 20 *)
      make_point (-1.) 0.7
    |] ] ;
    [ square ; Algo.reverse_single (Poly.scale (K.of_float 0.5) square) |> Poly.rotate (K.of_float 0.2) ] ;
    [ Algo.poly_of_path ~res:(P.K.of_float 0.2) test_path ] ;
    [ Algo.line_of_path ~width:(P.K.of_float 0.2) ~res:(P.K.of_float 0.1) test_path ] ;
    [ Path.circle P.K.one |>
      Algo.poly_of_path ~res:(P.K.of_float 0.2) ] ;
    letter_a ;
    List.map (Algo.inflate (P.K.of_float 0.1)) letter_a
  ] in
  let poly_pos n =
    let nb_polys = List.length polys_list in
    let poly_size = 1.8 in
    let x = n mod nb_polys in
    let y = n / nb_polys in
    [| K.of_float (float_of_int (x - nb_polys/2) *. poly_size) ;
       K.of_float ((1.5 -. float_of_int y) *. poly_size) |] in
  let list_mapi f l =
    let n = ref 0 in
    List.map (fun a -> let b = f !n a in incr n ; b) l in
  list_mapi (fun n poly -> Algo.translate_poly (poly_pos n) poly) polys_list

let draw_polys polys =
  let image = Img.make ~default:Color.white 800 600 in
  (* So far we have polys in between -10 and 10. Move them in the image: *)
  let polys = Algo.scale_poly 40. polys in
  let polys = Algo.translate_poly [| 400.; 300. |] polys in
  Algo.rasterize (Img.poke_scanline image Color.black) polys ;
  Img.open_graph image ;
  Img.draw image ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

(* Display something *)

let () =
  let to_draw =
    (* Raw version *)
    polys @
    (* Monotonization *)
    (List.map (fun poly -> Algo.monotonize (Algo.translate_poly [| K.zero ; K.of_int (-3) ; K.zero |] poly)) polys) @
    (* Triangulation *)
    (List.map (fun poly -> Algo.triangulate (Algo.translate_poly [| K.zero ; K.of_int (-6) ; K.zero |] poly)) polys)
    (* Convex partition *)
    (* (List.map (fun poly -> Algo.convex_partition (Algo.translate_poly [| K.zero ; K.of_int (-3) ; K.zero |] poly)) polys) *)
  in
  draw_polys (List.concat to_draw)
