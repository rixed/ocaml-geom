open Algen_intf

module type POINT =
sig
  include VECTOR

  val origin : t

  (** >0 if the 3rd point is "at left" of the line formed by first two. *)
  val compare_left : t -> t -> t -> int

  (** =0 if the 3rd point is within the segment formed by first two, <0 if before first, etc. *)
  val compare_segment : ?closed:bool -> t -> t -> t -> bool

  (* Compare two points according to their coordinate *)
  val compare_x : t -> t -> int
  val compare_y : t -> t -> int

  (* Compute the distance between the two given points *)
  val distance2 : t -> t -> K.t
  val distance : t -> t -> K.t

  (* Quickly tells if the two lines/segments intersects *)
  val intersect : ?closed:bool -> t -> t -> t -> t -> bool

  (** [intersection p1 p2 q1 q2] returns the location of the intersection
   * of lines (p0, p1) and (q1, q2), or None if they do not intersect
   * (epsilon controls how small the determinant is allowed to be before
   * considering the lines are parallel. *)
  val intersection : ?epsilon:K.t -> t -> t -> t -> t -> t option

  val copy : t -> t

  val area : t -> t -> K.t
  (** [area a b] returns the area of the parallelogram build from O a and b *)

  val center : t -> t -> t

  val determinant : K.t -> K.t -> K.t -> K.t -> K.t

  val turn_left : t -> t

  val turn_right : t -> t

  val scale : ?center:t -> K.t -> t -> t

  val rotate : ?center:t -> K.t -> t -> t
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

(** A POLYGON is essentially a Point Ring, but may have some other properties
 * as well. *)
module type POLYGON =
sig
  module Point : POINT
  type 'a ring = 'a Ring_impl.Ring.t
  include Pfds_intf.RING_GEN with type 'a t := 'a ring
  type t = Point.t ring

  val iter_pairs    : (t -> t -> unit) -> t -> unit
  val iter_edges    : t -> (Point.t -> Point.t -> unit) -> unit

  (** Map all the edges into a new edge and return the resulting polygon.
   * If two successive edges are not connected any more, [map_edges] will
   * connect them by moving those points to the intersection of those edges.
   *)
  val map_edges     : ?min_dist2:Point.K.t ->
                      (Point.t -> Point.t -> Point.t * Point.t) -> t -> t

  val is_inside     : t -> Point.t -> bool
  val translate     : Point.t (* Should be vector *) -> t -> t
  val scale         : ?center:Point.t -> Point.K.t -> t -> t
  val rotate        : ?center:Point.t -> Point.K.t -> t -> t
  val print         : Format.formatter -> t -> unit
end

(** A Path is basically a point list with interpolators in between
 * (an interpolator being a function to build intermediary points).
 * We consider it "closed" when the last point is at the same
 * location (Point.eq) that the first one (which is trivially true when
 * there is only one point, and false when the path is empty). *)
module type PATH =
sig
  type t

  module Point : POINT

  type point = Point.t

  (** Function that, given a starting point, a stopping point, a control
   * points list and a resolution, will return a list of intermediary
   * points going from the starting point to the destination at the given
   * resolution. *)
  type interpolator =
    point -> point -> point list -> Point.K.t -> point list

  val make_straight_line : interpolator
  val make_bezier_curve : interpolator

  (** Return the empty path starting at given position. *)
  val empty : point -> t

  (** Same as empty *)
  val start : point -> t

  (** Tells if a path is empty *)
  val is_empty : t -> bool

  (** Tells if a path is closed (ie. last point eq first point) *)
  val is_closed : t -> bool

  (** Extend a path. On all transformation functions the transformed
   * object is the last parameter to ease piping transformations. *)
  val extend : point (* next one *) -> point list (* control pts *) -> interpolator -> t -> t

  val straight_to : point -> t -> t
  val bezier_to : point -> (point list) -> t -> t

  (* Replace all points and controllers: *)
  val map : (Point.t -> Point.t) -> t -> t

  (** Build a path composed of the first one followed by the second one.
   * They are joint in such a way that the last point of the first path
   * becomes (and replaces) the starting point of the second one (without
   * translating the second path from the difference between the new and
   * old starting points). *)
  val concat : t -> t -> t

  (** Returns the number of points in a path (beside the starting point). *)
  val length : t -> int
  (* FIXME: include Pfds_intf.ITERABLE *)

  (** Translates a path. *)
  val translate : Point.t -> t -> t

  (** Reverse a path so that its last point become its new starting point
   * and the other way around. *)
  val reverse : t -> t

  (** Return the center position of a path. *)
  val center : t -> point

  (** Scale a path relatively to a point. *)
  val scale : ?center:point -> Point.K.t -> t -> t

  (** Scale a path along a given axis. *)
  val scale_along : ?center:point -> axis:point -> Point.K.t -> t -> t

  val rotate : ?center:Point.t -> Point.K.t -> t -> t

  (** Returns only that part of the path that is on the left of the given
   * line. This can of course return 0, 1 or more paths. *)
  val clip : point -> point -> t -> t list

  (** Iter over points with no points farther apart than given distance
   * (except for straight lines) *)
  val iter : Point.K.t -> t -> (point -> unit) -> unit

  (* TODO: fold! *)

  val iter_edges : t -> (point -> point -> unit) -> unit

  (* Build another path which edges are given by the passed function,
   * computed form the edges of the given path. Notice both path will
   * have the same starting point. *)
  val map_pts : (point -> point list -> point * point list) -> t -> t

  (* Tells if the point is inside the given path. The additional K.t that's
   * passed control the interpolators step. If the path is not closed then
   * the result is undefined. *)
  val is_inside : Point.K.t -> t -> Point.t -> bool

  (* Returns the bbox enclosing all control points (therefore an
   * overestimation that's independent of the interpolators *)
  val bbox : t -> Point.Bbox.t

  (* Returns the (rectangular) path around the given bounding-box.
   * @raise [Failure] if the bounding box is empty. *)
  val of_bbox : Point.Bbox.t -> t

  val area_min : t -> Point.K.t

  val print : Format.formatter -> t -> unit

  val rounded : ?radius:Point.K.t -> t list -> t list

  (** Some common shapes: *)

  (** Approximate a circle using 4 bezier quadratic curves *)
  val circle : ?center:Point.t -> Point.K.t -> t

  val rect : Point.t -> Point.t -> t
end

module type ALGORITHMS =
sig
  module Poly : POLYGON
  module Path : PATH with module Point = Poly.Point

  val area_polys : Poly.t list -> Poly.Point.K.t

  (** [area_paths_min paths] returns the area covered by the path as if they were composed of straight lines only *)
  val area_paths_min : Path.t list -> Path.Point.K.t

  val is_convex_at : Poly.Point.t -> Poly.Point.t -> Poly.Point.t -> bool
  val is_convex : Poly.t -> bool
  val in_cone : Poly.t -> Poly.Point.t -> bool
  val is_diagonal : Poly.t -> Poly.t -> bool
  val can_split : Poly.t -> Poly.t -> bool
  val iter_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit
  val iter_splitable_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit

  val convex_partition : Poly.t list -> Poly.t list
  val triangulate : Poly.t list -> Poly.t list
  val triangulate_slow : Poly.t list -> Poly.t list
  val intersect_polys : Poly.Point.t -> Poly.Point.t -> Poly.t list -> bool
  val monotonize : Poly.t list -> Poly.t list
  val reverse_polys : Poly.t list -> Poly.t list
  val reverse_single : Poly.t -> Poly.t
  val reverse_paths : Path.t list -> Path.t list
  val simplify : Poly.t list -> Poly.t
  val translate_poly : Poly.Point.t -> Poly.t list -> Poly.t list
  val scale_poly : ?center:Poly.Point.t -> Poly.Point.K.t -> Poly.t list -> Poly.t list

  (** Close the path if not already, and convert it to a Polygon: *)
  val poly_of_path : res:Path.Point.K.t -> Path.t -> Poly.t

  (** Same as [poly_of_path] but doing several paths in one go: *)
  val polys_of_paths : res:Path.Point.K.t -> Path.t list -> Poly.t list

  (** Take an open path and turn it into a flat poly. [res] is the
   * resolution for the interpolators while [width] is the desired
   * width of the resulting polygon (please keep it >= 0). *)
  val line_of_path : width:Path.Point.K.t -> res:Path.Point.K.t ->
                     Path.t -> Poly.t

  (** Move each edge of a poly away (on the right) by a given distance: *)
  val inflate : Path.Point.K.t -> Poly.t -> Poly.t

  (** [fall_on ~dir p1 p2] returns a polygon that's a translated version
   * of [p2] in the direction given by [dir] (or [-dir]) for its focused
   * point to reach [p1]. If [p1] is not in that direction, then returns
   * [p2]. *)
  (* TODO: a flag to forbid moving toward [-dir] *)
  val fall_on : dir:Poly.Point.t -> Poly.t -> Poly.t -> Poly.t

  val bbox_single_poly : Poly.t -> Poly.Point.Bbox.t
  val bbox : Poly.t list -> Poly.Point.Bbox.t

  (* [rasterize polys fun] will call [fun x1 x2 y alpha] for every pixel of
   * the polygon *)
  (* FIXME: clipping in Y and X! *)
  val rasterize :
    (int (* x_start *) -> int (* x_stop *) -> int (* y *) -> Poly.Point.K.t (* alpha *) -> unit) ->
    Poly.t list -> unit

  (* Some utilities *)
  val path_of_points : Poly.Point.t list -> Path.t
  val poly_of_points : Poly.Point.t list -> Poly.t
  val poly_of_ascii_repr : string list -> Poly.t
  (** Build a polygon from a list of strings where letters are connected in alphabetic order *)

  val unit_square : Poly.t
end

module type CONVEX_HULL_SET =
sig
  module Poly : POLYGON
  module PSet : POINT_SET with module Point = Poly.Point
  val convex_hull : PSet.t -> Poly.t
end

module type CONVEX_HULL_SORTED_SET =
sig
  module Poly : POLYGON
  module PXYL : POINT_XY_LIST with module Point = Poly.Point
  val convex_hull : PXYL.t -> Poly.t
end

(* Various useful stuff *)

exception Bad_geometry

module MakeIsInside (K : FIELD) =
struct
  let is_inside iter point =
    let nb_intersect = ref 0 in
    iter (fun p0 p1 ->
      if (K.compare p1.(1) point.(1) > 0 && K.compare p0.(1) point.(1) <= 0) ||
         (K.compare p0.(1) point.(1) > 0 && K.compare p1.(1) point.(1) <= 0) then (
          (* compute location of intersection *)
          let ( * ) = K.mul and ( - ) = K.sub and ( + ) = K.add and ( / ) = K.div in
          let x = p0.(0) + ((p0.(1) - point.(1)) * (p1.(0) - p0.(0))) / (p0.(1) - p1.(1)) in
          if K.compare x point.(0) > 0 then incr nb_intersect
      )) ;
    !nb_intersect land 1 = 1
end
