type orientation = Vertical | Horizontal

module type GLYPH =
sig
  module Poly : Geom.POLYGON
  module Path : Geom.PATH with module Point = Poly.Point
  type t

  val make     : char -> t
  val to_paths : t -> Path.t list
  val to_polys : res:Path.Point.K.t -> t -> Poly.t list
  val bbox     : t -> Poly.Point.Bbox.t
  val advance  : ?orientation:orientation -> t -> t -> Poly.Point.t
(*  val line_gap : t -> Path.scalar *)
end

module type WORD =
sig
  module Glyph : GLYPH

  type t

  val make     : ?orientation:orientation -> string -> t
  val bbox     : t -> Glyph.Poly.Point.Bbox.t
  val to_paths : t -> (Glyph.Poly.Point.t * Glyph.Path.t list) list
  val to_polys : res:Glyph.Path.Point.K.t -> t -> (Glyph.Poly.Point.t * Glyph.Poly.t list) list
  (* [to_polys ~res word] returns a list of offset*polys *)
end
