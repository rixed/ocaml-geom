(* Test the shipped implementations. *)
open Algen_intf
module G = View.Glop

module P = Geom_shapes.Point (G.V)

let make_point x y = [| G.K.of_float x ; G.K.of_float y |]

module R = Cnt_impl.GenRing (struct type t = P.t end)
module TestR = Cnt.Test_GenRing (R) (struct let v = make_point 1. 1. end)

module Poly = Geom_shapes.Polygon (P) (R)

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
	Path.extend (Path.empty (make_point (-1.) 1.)) (make_point 1. 1.) [ make_point (-1.) (-2.) ; make_point 1. (-2.) ] Path.make_bezier_curve

let letter_a =
	Algo.scale_poly
		(Glyph.to_poly (Glyph.make 'a') (G.K.of_float 1.2))
		P.zero
		(G.K.of_float 0.1)

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
		[ square ; Algo.inverse_single (Algo.scale_single_poly square P.zero (G.K.of_float 0.5)) ] ;
		[ Algo.poly_of_path test_path (P.K.of_float 0.2) ] ;
		letter_a
	] in
	let poly_pos n =
		let nb_polys = List.length polys_list in
		let poly_size = 2.5 in
		let x = n mod nb_polys in
		let y = n / nb_polys in
		[| G.K.of_float (float_of_int (x - nb_polys/2) *. poly_size) ;
		   G.K.of_float ((1.5 -. float_of_int y) *. poly_size) ;
		   G.K.zero |] in
	let rec list_mapi f l =
		let n = ref 0 in
		List.map (fun a -> let b = f !n a in incr n ; b) l in
	list_mapi (fun n poly -> Algo.translate_poly poly (poly_pos n)) polys_list

let draw_polys polys () =
	let draw_single poly =
		let varray = G.make_vertex_array (Poly.length poly) in
		let idx = ref 0 in
		Poly.iter poly (fun p -> G.vertex_array_set varray !idx (Poly.get p) ; incr idx) ;
		G.render G.Line_loop varray (G.Uniq G.white) in
	List.iter draw_single polys

(* Display something *)

let () =
	let painters =
		let to_draw =
			(* Raw version *)
			polys @
			(* Monotonization *)
			(List.map (fun poly -> Algo.monotonize (Algo.translate_poly poly [| G.K.zero ; G.K.of_int (-3) ; G.K.zero |])) polys) @
			(* Triangulation *)
			(List.map (fun poly -> Algo.triangulate (Algo.translate_poly poly [| G.K.zero ; G.K.of_int (-6) ; G.K.zero |])) polys)
			(* Convex partition *)
			(* (List.map (fun poly -> Algo.convex_partition (Algo.translate_poly poly [| G.K.zero ; G.K.of_int (-3) ; G.K.zero |])) polys) *)
		in
		(fun () ->
			G.set_modelview G.M.id ;
			G.mult_modelview (G.M.translate G.K.zero G.K.zero (G.K.neg G.K.one)) ;
			G.mult_modelview (G.M.scale (G.K.of_float 0.15) (G.K.of_float 0.15) (G.K.one))) ::
		(fun () ->
			let bg_color = [| G.K.of_float 0.1 ; G.K.of_float 0.1 ; G.K.of_float 0.5 |] in
			G.clear ~color:bg_color ()) ::
		(List.map draw_polys to_draw) in

	View.display painters

