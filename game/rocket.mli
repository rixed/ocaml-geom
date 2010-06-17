open Mlrocket

type t =
	{ name  : string ;
	  poly  : Poly.t ;
	  pos   : Point.t ;
	  speed : Vec.t }

val make : unit -> t

