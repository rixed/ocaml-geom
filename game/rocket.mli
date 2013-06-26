open Mlrocket

type t

val make         : Point.t -> t
val poly         : t -> Poly.t
val pos          : t -> Point.t
val prev_pos     : t -> Point.t
val orient       : t -> G.V.t
val speed        : t -> G.V.t
val viewable     : t -> View.viewable
val set_orient   : t -> G.V.t -> unit
val set_thrust   : t -> K.t -> unit
val set_viewable : t -> View.viewable -> unit
val run          : K.t -> K.t -> t -> unit

