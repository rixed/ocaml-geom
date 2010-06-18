open Mlrocket

type t

val make : unit -> t
val poly_of_rocket : t -> Poly.t
val pos_of_rocket : t -> unit -> float * float * float
val orient_of_rocket : t -> unit -> float * float
val set_orient : t -> float * float -> unit
