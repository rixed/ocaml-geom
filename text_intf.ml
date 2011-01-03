type orientation = Vertical | Horizontal

module type GLYPH =
sig
	module Poly : Geom.POLYGON
	module Path : Geom.PATH with module Point = Poly.Point
	type t

	val make    : char -> t
	val to_poly : t -> Path.Point.K.t -> Poly.t list
	val bbox    : t -> Poly.Point.Bbox.t
	val advance : ?orientation:orientation -> t -> t -> Poly.Point.t
(*	val line_gap : t -> Path.scalar *)
end

module type WORD =
sig
	module Glyph : GLYPH

	type t

	val make     : ?orientation:orientation -> string -> t
	val bbox     : t -> Glyph.Poly.Point.Bbox.t
	val to_polys : t -> Glyph.Path.Point.K.t -> (Glyph.Poly.Point.t * Glyph.Poly.t list) list
	(* [to_polys word prec] returns a list of offset*polys *)
end
