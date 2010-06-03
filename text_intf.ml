type orientation = Vertical | Horizontal

module type GLYPH =
sig
	module Poly : Geom.POLYGON
	module Path : Geom.PATH with module Point = Poly.Point
	type t

	val make : int -> t
	val to_poly : t -> Path.scalar -> Poly.t list
	val bbox : t -> Poly.Point.bbox
	val advance : t -> t -> Path.scalar
(*	val line_gap : t -> Path.scalar
	val kerning : t -> t -> Poly.Point.t *)
end

module type WORD =
sig
	module Glyph : GLYPH

	type t

	val make : ?orientation:orientation -> string -> t
	val bbox : t -> Glyph.Poly.Point.bbox
	val to_poly : t -> Glyph.Path.scalar -> Glyph.Poly.t list
end
