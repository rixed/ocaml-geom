type color = Gl.rgb

type gc = {
	fill_color : color option ;
	outline_color : color option
}

module type PIC =
sig
	module Poly : Geom.POLYGON
	module Path : Geom.PATH with module Point = Poly.Point

	type elmt = Poly of Poly.t | Path of Path.t | Dot of Poly.Point.t
	type t = (elmt * gc) list

	val draw : ?prec:Poly.Point.K.t -> t -> unit
	val bbox : t -> (Poly.Point.t * Poly.Point.t)
end	
