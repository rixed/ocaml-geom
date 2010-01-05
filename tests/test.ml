(* Test the shipped implementations. *)

module K = Geom.CheckedField (Geom_algebr.FloatField)

module V = Geom_algebr.Vector2D (K)
module TestV = Geom.Test_Vector (V)

module P = Geom_shapes.Point (V)

let make_point x y = V.of_3scalars (x, y, 0.)

module R = Cnt_impl.GenRing (struct type t = P.t end)
module TestR = Cnt.Test_GenRing (R) (struct let v = make_point 1. 1. end)

module Poly = Geom_shapes.Polygon (P) (R)

module Path = Geom_path.Make (P)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Text = Geom_text.Make (Algo)

module Painter = View_simple.Make_painter (Poly)

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
	Path.extend (Path.empty (make_point (-1.) 1.)) (make_point 1. 1.) [ make_point (-1.) (-2.) ; make_point 1. (-2.) ] Path.make_bezier_curve

let letter_a = Algo.scale_poly (Text.poly_of_glyph 97 1.2) P.zero 0.1

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
			make_point (-0.7) 0.15 ;	(* 5 *)
			make_point (-1.) (-0.15) ;
			make_point (-1.) (-0.7) ;
			make_point (-0.7) (-1.) ;
			make_point 0.7 (-1.) ;
			make_point 1. (-0.7) ;	(* 10 *)
			make_point 1. (-0.5) ;
			make_point 0.7 (-0.5) ;
			make_point 0.7 (-0.7) ;
			make_point (-0.7) (-0.7) ;
			make_point (-0.7) (-0.15) ;	(* 15 *)
			make_point 0.7 (-0.15) ;
			make_point 1. 0.15 ;
			make_point 1. 0.7 ;
			make_point 0.7 1. ;
			make_point (-0.7) 1. ;	(* 20 *)
			make_point (-1.) 0.7
		|] ] ;
		[ square ; (Algo.inverse_single (Algo.scale_single_poly square P.zero 0.5)) ] ;
		[ Algo.poly_of_path test_path (P.K.of_float 0.2) ] ;
		letter_a
	] in
	let poly_pos n =
		let nb_polys = List.length polys_list in
		let poly_size = 2.5 in
		let x = n mod nb_polys in
		let y = n / nb_polys in
		V.of_3scalars (
			float_of_int (x - nb_polys/2) *. poly_size,
			(2.5 -. float_of_int y) *. poly_size,
			0.) in
	let rec list_mapi f l =
		let n = ref 0 in
		List.map (fun a -> let b = f !n a in incr n ; b) l in
	list_mapi (fun n poly -> Algo.translate_poly poly (poly_pos n)) polys_list

(* Display something *)

let () =
	let painters =
		let to_draw =
			(* Raw version *)
			polys @
			(* Convex partition *)
			(List.map (fun poly -> Algo.convex_partition (Algo.translate_poly poly (V.of_3scalars (0., -3., 0.)))) polys) @
			(* Triangulation *)
			(List.map (fun poly -> Algo.triangulate (Algo.translate_poly poly (V.of_3scalars (0., -6., 0.)))) polys) @
			(* Monotonization *)
			(List.map (fun poly -> Algo.monotonize (Algo.translate_poly poly (V.of_3scalars (0., -9., 0.)))) polys)
		in
		Painter.draw_background :: (List.map (fun poly -> fun () -> Painter.draw_poly poly) to_draw) in

	View.display painters

