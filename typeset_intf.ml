(*
	Given a Polygon, typeset a set of boxes into it.
*)

open Algen_intf

type orientation = Horizontal | Vertical
type direction = Left_to_right | Right_to_left
type justification = Start | Stop | Center | Justified

module type BLOC =
sig
	module K : FIELD
	type t
	(* Returns the size of a bloc in the given dimension *)
	val size : t -> int (* dimension *) -> K.t
end

module type TYPESET =
sig
	module Poly : Geom.POLYGON
	module Bloc : BLOC with module K = Poly.Point.K
	type k = Poly.Point.K.t

	type glue_kind =
		| Stuck		(* following chunk entry point must = this chunk exit point *)
		| Space of (k * k (*min, max*))	(* they must be distant from min to max, or can be on distinct line *)
		(*| Hiphen (* they should stuck but can be put on separate lines - hyphenation *)*)

	type chunk = glue_kind * Bloc.t

	(* Given the total space in some dimension and a list of chunks,
	 * return the list of their starting position (for those who fit) *)
	val typeset_line :
		?justification:justification ->
		int (* Dimension *) ->
		k (* total size *) ->
		chunk list -> Poly.Point.t list

	(* Given a poly and a list of chunks, return the list of their computed positions
	 * (ie. position of their entry points) *)
	val typeset :
		?orientation:orientation -> ?direction:direction ->
		?vert_justification:justification -> ?horiz_justification:justification ->
		Poly.t -> chunk list -> Poly.Point.t list

end
