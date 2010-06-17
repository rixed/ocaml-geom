(* This program is a test program to play with OCaml and the geom library in
 * a "realistic" setting. Don't expect too much. *)

(* Here we merely build the polygon implementation and friends *)
let fixed_prec = 16
module K = Geom_algebr.IntField (struct let v = fixed_prec end)
module Vec = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (Vec)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)
module Path = Geom_path.Make (Point)
module Pic = Pic_impl.Make (Poly) (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)
