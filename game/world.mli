open Mlrocket

type t = 
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
	  gravity : K.t	;
      radius : K.t ;
      mutable max_speed : K.t }

(* FIXME: variable prec *)
val prec : K.t

val make : radius:int -> t
(* Run the world for this fraction of a second *)
val run  : K.t -> t -> unit
