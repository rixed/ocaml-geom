(* This program is a test program to play with OCaml and the geom library in
 * a "realistic" setting. Don't expect too much. *)

module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)
module Point = Geom_shapes.Point (G.V)
module K = Point.K
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)

open Format

let mlog fmt =
	kfprintf
		(fun ff ->
			pp_print_newline ff () ;
			pp_print_flush ff ())
		std_formatter fmt

