open Algen_intf

module type POINT =
sig
	include VECTOR

	(** >0 if the 3rd point is "at left" of the line formed by first two. *)
	val compare_left : t -> t -> t -> int

	(** =0 if the 3rd point is within the segment formed by first two, <0 if before first, etc. *)
	val compare_segment : ?closed:bool -> t -> t -> t -> bool

	(* Compare two points according to their coordinate *)
	val compare_x : t -> t -> int
	val compare_y : t -> t -> int

	val intersect : ?closed:bool -> t -> t -> t -> t -> bool

	val copy : t -> t
	
	val area : t -> t -> K.t
	(** [area a b] returns the area of the triangle O a b *)

	val center : t -> t -> t
end

module type POINT_SET =
sig
	module Point : POINT
	type t
	val iter : t -> (Point.t -> unit) -> unit
	val exists : t -> (Point.t -> bool) -> bool
	val size : t -> int
end

module type POINT_XY_LIST = (* Ordered points in order X, Y *)
sig
	module Point : POINT
	type t
	(* TODO *)
end

exception Bad_geometry

(** A POLYGON is essentialy a Point Ring, but may have some other properties as well. *)
module type POLYGON =
sig
	module Point : POINT
	include Pfds_intf.ITERABLE with type e = Point.t

	val get           : t -> Point.t
	val next          : t -> t
	val prev          : t -> t
	val insert_before : t -> Point.t -> t
	val insert_after  : t -> Point.t -> t
	val remove        : t -> t
	val iterr         : (t -> unit) -> t -> unit
	val iterir        : (int -> t -> unit) -> t -> unit
	val fold_leftr    : ('b -> t -> 'b) -> 'b -> t -> 'b
	val fold_rightr   : (t -> 'b -> 'b) -> t -> 'b -> 'b

	val iter_pairs    : (t -> t -> unit) -> t -> unit
	val print         : Format.formatter -> t -> unit
end

(** A Path is basically a point list. We consider it "closed" when the last point is at the same
 * location (Point.eq) that the first one (which is trivially true when there is one one point,
 * and false when the path is empty). *)
module type PATH =
sig
	type t

	module Point : POINT

	type point = Point.t
	
	(** Function that, given a starting point, a stopping point, a control points list
	    and a resolution, will return a list of intermediary points going from the
	    starting point to the destination at the given resolution. *)
	
	type interpolator = point -> point -> point list -> Point.K.t -> point list
	val make_straight_line : interpolator
	val make_bezier_curve : interpolator

	(** Return the empty path starting at given position. *)
	val empty : point -> t

	(** Tells if a path is empty *)
	val is_empty : t -> bool

	(** Tells if a path is closed (ie. last point eq first point) *)
	val is_closed : t -> bool

	(** Extend a path *)
	val extend : t -> point (* next one *) -> point list (* control pts *) -> interpolator -> t

	(** Build a path composed of the first one followed by the second one. *)
	val concat : t -> t -> t

	(** Returns the number of points in a path. *)
	val length : t -> int
    (* FIXME: include Pfds_intf.ITERABLE *)

	(* These belongs to ALGO (rename to path_translate, etc) *)
	(** Translates a path. *)
	(* FIXME: Point here should be Vector *)
	val translate : t -> point -> t

	val inverse : t -> t

	(** Return the center position of a path. *)
	val center : t -> point

	(** Scale a path relatively to a point. *)
	val scale : t -> point -> Point.K.t -> t

	(** Scale a path along a given axis. *)
	val scale_along : point (*center*) -> point (*axis*) -> Point.K.t -> t -> t
	
	val iter : Point.K.t -> t -> (point -> unit) -> unit
	val iter_edges : t -> (point -> point -> unit) -> unit

	val bbox : t -> Point.Bbox.t

	val area_min : t -> Point.K.t

	val print : Format.formatter -> t -> unit

	val rounded : ?radius:Point.K.t -> t list -> t list
end

module type ALGORITHMS =
sig
	module Poly : POLYGON
	module Path : PATH with module Point = Poly.Point

	val area_polys : Poly.t list -> Poly.Point.K.t
	val area_paths_min : Path.t list -> Path.Point.K.t
	(** [area_paths_min paths] returns the area covered by the path as if they were composed of straight lines only *)
	val is_convex_at : Poly.Point.t -> Poly.Point.t -> Poly.Point.t -> bool
	val is_convex : Poly.t -> bool
	val in_cone : Poly.t -> Poly.Point.t -> bool
	val is_diagonal : Poly.t -> Poly.t -> bool
	val can_split : Poly.t -> Poly.t -> bool
	val iter_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit
	val iter_splitable_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit
	val iter_edges : Poly.t -> (Poly.Point.t -> Poly.Point.t -> unit) -> unit

	val convex_partition : Poly.t list -> Poly.t list
	val triangulate : Poly.t list -> Poly.t list
	val monotonize : Poly.t list -> Poly.t list
	val inverse_polys : Poly.t list -> Poly.t list
	val inverse_single : Poly.t -> Poly.t
	val inverse_paths : Path.t list -> Path.t list
	val simplify : Poly.t list -> Poly.t
	val translate_poly : Poly.t list -> Poly.Point.t -> Poly.t list
	val translate_single_poly : Poly.t -> Poly.Point.t -> Poly.t
	val scale_poly : Poly.t list -> Poly.Point.t -> Poly.Point.K.t -> Poly.t list
	val scale_single_poly : Poly.t -> Poly.Point.t -> Poly.Point.K.t -> Poly.t
	(** Close the path and convert it to a Polygon. *)
	val poly_of_path : Path.t -> Path.Point.K.t -> Poly.t
	val polys_of_paths : Path.t list -> Path.Point.K.t -> Poly.t list
	val scale_point : Poly.Point.t -> Poly.Point.t -> Poly.Point.K.t -> Poly.Point.t
	val bbox_single_poly : Poly.t -> Poly.Point.Bbox.t
	val bbox : Poly.t list -> Poly.Point.Bbox.t

	(* Rasterization *)
	val rasterize : Poly.t list ->
		(int (* x_start *) -> int (* x_stop *) -> int (* y *) -> Poly.Point.K.t (* alpha *) -> unit) ->
		unit
	(* [iter_rasters polys fun] will call [fun x y alpha] for every pixel of the polygon *)

	(* Some utilities *)
	val path_of_points : Poly.Point.t list -> Path.t
	val poly_of_points : Poly.Point.t list -> Poly.t
	val poly_of_ascii_repr : string list -> Poly.t
	val unit_square : Poly.t
end

module type CONVEX_HULL_SET =
	functor (Poly : POLYGON) ->
	functor (PSet : POINT_SET with module Point = Poly.Point) ->
sig
	val convex_hull : PSet.t -> Poly.t
end

module type CONVEX_HULL_SORTED_SET =
	functor (Poly : POLYGON) ->
	functor (PXYL : POINT_XY_LIST with module Point = Poly.Point) ->
sig
	val convex_hull : PXYL.t -> Poly.t
end

