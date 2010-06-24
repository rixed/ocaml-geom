open Mlrocket

type t

val make       : unit -> t
val poly       : t -> Poly.t
val pos        : t -> unit -> float * float * float
val orient     : t -> unit -> float * float
val set_orient : t -> float * float -> unit
val set_speed  : t -> float -> unit
val run        : t -> unit
