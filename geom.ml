module type FIELD =
sig
	type t

	val zero : t
	val one : t
	val compare : t -> t -> int
	val of_float : float -> t
	val to_float : t -> float
	val of_int : int -> t
	val to_string : t -> string

	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val neg : t -> t
	val inv : t -> t
	val half : t -> t
	val double : t -> t
	val square : t -> t
	val sqrt : t -> t

	val print : Format.formatter -> t -> unit
end

module CheckedField (K : FIELD)
	: FIELD with type t = K.t =
struct
	include K

	let add a b =
		let c = add a b in
		let c_= add b a in
		assert (c = c_) ;
		if a = zero then assert (c = b) else if b = zero then assert (c = a) ;
		c
	
	let sub a b =
		let c = sub a b in
		let c_= sub b a in
		assert (add c c_ = zero) ;
		if a = b then assert (c = zero) ;
		if b = zero then assert (c = a) ;
		c
	
	let mul a b =
		let c = mul a b in
		let c_= mul b a in
		assert (c = c_) ;
		if a = zero || b = zero then assert (c = zero) ;
		if a = one then assert (c = b) ;
		if b = one then assert (c = a) ;
		c
	
	let div a b =
		let c = div a b in
		if b = one then assert (c = a) ;
		if a = zero then assert (c = zero) ;
		c

	let _ =
		assert (add one zero = one) ;
		assert (sub one zero = one) ;
		assert (sub one one = zero) ;
		assert (mul one one = one) ;
		assert (mul one zero = zero) ;
		assert (div one one = one) ;
		assert (div zero one = zero) ;
		assert (square zero = zero) ;
		assert (square one = one) ;
		assert (half zero = zero)
end

module type VECTOR =
sig
	module K : FIELD
	type t
	type scalar = K.t

	val zero : t
	val make_unit : int -> t
	val of_3scalars : scalar * scalar * scalar -> t
	val of_2scalars : scalar * scalar -> t
	val to_3scalars : t -> scalar * scalar * scalar
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> scalar -> t
	val half : t -> t
	val scalar_product : t -> t -> scalar
	val norm2 : t -> scalar
	val to_point3 : t -> Gl.point3
	val area : t -> t -> scalar
	val right_turn : t -> t	(** Return the same but turned in anticlockwise direction of PI/2 *)
	val eq : t -> t -> bool
	
	val print : Format.formatter -> t -> unit
end

module Test_Vector (V : VECTOR) =
struct
	let _ =
		assert (V.zero = V.of_3scalars (V.K.zero, V.K.zero, V.K.zero)) ;
		assert (V.add V.zero V.zero = V.zero) ;
		assert (V.sub V.zero V.zero = V.zero) ;
		assert (V.mul V.zero V.K.one = V.zero) ;
		assert (V.norm2 V.zero = V.K.zero)
end

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
	include Cnt.GENRING with type elmt = Point.t
	type t = Point.t ring

	val print : Format.formatter -> t -> unit
end

(** A Path is basically a point list with added functionnalities. *)
module type PATH =
sig
	type t
	
	(** Types used for path geometry. *)
	module Point : POINT
	type point = Point.t
	type scalar = Point.scalar

	(** Function that, given a starting point, a stopping point, a control points list
	    and a resolution, will return a list of intermediary points going from the
	    starting point to the destination at the given resolution. *)
	
	type interpolator = point -> point -> point list -> scalar -> point list
	val make_straight_line : interpolator
	val make_bezier_curve : interpolator

	(** Return the empty path starting at given position. *)
	val empty : point -> t

	(** Extend a path *)
	val extend : t -> point (* next one *) -> point list (* control pts *) -> interpolator -> t

	(** Build a path composed of the first one followed by the second one. *)
	val concat : t -> t -> t

	(** Returns the number of points in a path. *)
	val size : t -> int

	(* These belongs to ALGO (rename to path_translate, etc) *)
	(** Translates a path. *)
	(* FIXME: Point here should be Vector *)
	val translate : t -> Point.t -> t

	(** Return the center position of a path. *)
	val center : t -> point

	(** Scale a path relatively to a point. *)
	val scale : t -> point -> scalar -> t

	(** Scale a path along a given axis. *)
	val scale_along : point (*center*) -> point (*axis*) -> scalar -> t -> t
	
	val iter : t -> scalar -> (point -> unit) -> unit
end

module type ALGORITHMS =
sig
	module Poly : POLYGON
	module Path : PATH with module Point = Poly.Point
	val area : Poly.t list -> Poly.Point.scalar
	val is_convex_at : Poly.Point.t -> Poly.Point.t -> Poly.Point.t -> bool
	val is_convex : Poly.t -> bool
	val in_cone : Poly.t -> Poly.Point.t -> bool
	val is_diagonal : Poly.t -> Poly.t -> bool
	val can_split : Poly.t -> Poly.t -> bool
	val iter_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit
	val iter_splitable_diagonals : Poly.t -> (Poly.t -> Poly.t -> unit) -> unit

	val convex_partition : Poly.t list -> Poly.t list
	val triangulate : Poly.t list -> Poly.t list
	val monotonize : Poly.t list -> Poly.t list
	val inverse : Poly.t list -> Poly.t list
	val inverse_single : Poly.t -> Poly.t
	val simplify : Poly.t list -> Poly.t
	val translate_poly : Poly.t list -> Poly.Point.t -> Poly.t list
	val translate_single_poly : Poly.t -> Poly.Point.t -> Poly.t
	val scale_poly : Poly.t list -> Poly.Point.t -> Poly.Point.scalar -> Poly.t list
	val scale_single_poly : Poly.t -> Poly.Point.t -> Poly.Point.scalar -> Poly.t
	(** Close the path and convert it to a Polygon. *)
	val poly_of_path : Path.t -> Path.Point.scalar -> Poly.t

	val scale_point : Poly.Point.t -> Poly.Point.t -> Poly.Point.scalar -> Poly.Point.t

	(* Some utilities *)
	val path_of_points : Poly.Point.t list -> Path.t
	val poly_of_points : Poly.Point.t list -> Poly.t
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

module type TEXT =
sig
	module Poly : POLYGON
	module Path : PATH with module Point = Poly.Point

	val poly_of_glyph : int -> Path.scalar -> Poly.t list
end
