open Mlrocket

type t

val make       : Point.t -> t
val poly       : t -> Poly.t
val pos        : t -> unit -> float * float * float
val orient     : t -> unit -> float * float
val set_orient : t -> float * float -> unit
val set_thrust : t -> float -> unit
val run        : K.t -> float -> t -> unit
