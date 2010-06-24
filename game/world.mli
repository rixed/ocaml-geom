open Mlrocket

val radius : K.t

type t = 
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	}

val make : unit -> t
(* Run the world for this fraction of a second *)
val run  : float -> t -> unit
