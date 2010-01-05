(* Run the glut viewer in a separate thread,
 * with a single painter that render a toplevel accessible list of polygons. *)

module K = Geom_algebr.IntField (struct let v = 4 end)
module V = Geom_algebr.Vector2D (K)
module P = Geom_shapes.Point (V)
module R = Cnt_impl.GenRing (struct type t = P.t end)
module Poly = Geom_shapes.Polygon (P) (R)
module Path = Geom_path.Make (P)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Painter = View_simple.Make_painter (Poly)

let make_point (x, y) = V.of_3scalars (K.of_float x, K.of_float y, K.zero)

let make_poly cx cy plist =
	let center_vec = V.of_3scalars (cx, cy, K.zero) in
	let rec add_point pol = function
		| [] -> pol
		| p::rest -> add_point (Poly.insert_after pol p) rest in
	Algo.translate_single_poly (add_point Poly.empty plist) center_vec

let big_U = [
	make_point (-1., -1.) ;
	make_point (1., -1.) ;
	make_point (1., 1.) ;
	make_point (0.5, 1.) ;
	make_point (0.5, -0.5) ;
	make_point (-0.5, -0.5) ;
	make_point (-0.5, 1.) ;
	make_point (-1., 1.)
]

let comb = [
	make_point (-1., -1.) ;
	make_point (1., -1.) ;
	make_point (0.8, 1. );
	make_point (0.6, -0.5) ;
	make_point (0.4, 1. );
	make_point (0.2, -0.5) ;
	make_point (0., 1. );
	make_point (-0.2, -0.5) ;
	make_point (-0.4, 1. );
	make_point (-0.6, -0.5) ;
	make_point (-0.8, 1.)
]

let big_2 = [
	make_point (-1., 0.5 );
	make_point (-0.7, 0.5 );
	make_point (-0.7, 0.7 );
	make_point (0.7, 0.7 );
	make_point (0.7, 0.15 );
	make_point (-0.7, 0.15 );
	make_point (-1., -0.15) ;
	make_point (-1., -0.7) ;
	make_point (-0.7, -1.) ;
	make_point (0.7, -1.) ;
	make_point (1., -0.7) ;
	make_point (1., -0.5) ;
	make_point (0.7, -0.5) ;
	make_point (0.7, -0.7) ;
	make_point (-0.7, -0.7) ;
	make_point (-0.7, -0.15) ;
	make_point (0.7, -0.15) ;
	make_point (1., 0.15 );
	make_point (1., 0.7 );
	make_point (0.7, 1. );
	make_point (-0.7, 1. );
	make_point (-1., 0.7)
]

let square = [
	make_point (-1., -1.) ;
	make_point (1., -1.) ;
	make_point (1., 1. );
	make_point (-1., 1.)
]

let polys : (string, Poly.t list) Hashtbl.t = Hashtbl.create 10

let show_poly name poly =
	Hashtbl.replace polys name poly ;
	Glut.postRedisplay ()

let show_polys name polys =
	let count = ref 0 in
	List.iter (fun p ->
		let n = name ^ (string_of_int !count) in
		show_poly n p ;
		incr count) polys

let del_poly name =
	Hashtbl.remove polys name ;
	Glut.postRedisplay ()

(* A mere turtle like interface *)

let pi = acos (-1.)

type turtle = { mutable pos : P.t ; mutable dir : float }
let glob_turtle = {
	pos = P.zero;
	dir = pi/.2.
}

let turtle_dir_vec () =
	V.of_3scalars (K.of_float (cos glob_turtle.dir), K.of_float (sin glob_turtle.dir), K.zero)

let turtle_painter () =
	Painter.draw_turtle glob_turtle.pos (turtle_dir_vec ())

let turn a =
	glob_turtle.dir <- glob_turtle.dir +. a ;
	Glut.postRedisplay ()

let run d =
	glob_turtle.pos <- P.add glob_turtle.pos (V.mul (turtle_dir_vec ()) d) ;
	Glut.postRedisplay ()

let last_clic () = make_point (List.hd !(View.last_clics))

let clics () = List.rev_map make_point !View.last_clics

let get_clics () =
	let ret = clics () in
	View.last_clics := [] ;
	Glut.postRedisplay () ;
	ret

let poly_of_clics cx cy =
	[ make_poly cx cy (get_clics ()) ]

let clics_painter () =
	List.iter Painter.draw_point (clics ())

(* Run GLUT *)

let start () =
	let polys_painter () = Hashtbl.iter (fun _ p -> Painter.draw_poly p) polys in
	ignore (Thread.create (fun () ->
		View.display [
			Painter.draw_background ;
			polys_painter ;
			turtle_painter ;
			clics_painter
		]) ())
