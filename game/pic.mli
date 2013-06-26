open Mlrocket

type color = G.color_specs

type gc = {
	fill_color : color option ;
	outline_color : color option
}

type elmt = Poly of Poly.t | Path of Path.t | Dot of Poly.Point.t | Clear
type t = (elmt * gc) list

val draw : ?prec:Poly.Point.K.t -> t -> unit
val bbox : t -> Poly.Point.Bbox.t
