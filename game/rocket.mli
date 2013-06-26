open Mlrocket

type t

val make       : Point.t -> t
val poly       : t -> Poly.t
val pos        : t -> Point.t
val prev_pos   : t -> Point.t
val orient     : t -> G.V.t
val set_orient : t -> G.V.t -> unit
val set_thrust : t -> K.t -> unit
val run        : K.t -> K.t -> t -> unit

