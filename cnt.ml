module type ORDERED =
sig
	type t
	val compare : t -> t -> int
end

module type GENMAP =
sig
	type 'a t
	type key
	
	val empty : 'a t
	val is_empty : 'a t -> bool

	val add : 'a t -> key -> 'a -> 'a t
	val remove : 'a t -> key -> 'a t

	val find : 'a t -> key -> 'a	(** Or throw Not_found *)

	val iter : 'a t -> (key -> 'a -> unit) -> unit
	val map : 'a t -> (key -> 'a -> 'b) -> 'b t
end

module type GENSET =
sig
	type 'a t
	
	val empty : 'a t
	val is_empty : 'a t -> bool
	val size : 'a t -> int

	val add : 'a t -> 'a -> 'a t

	val iter : 'a t -> ('a -> unit) -> unit
	val map : 'a t -> ('a -> 'b) -> 'b t
	val exists : 'a t -> ('a -> bool) -> bool
end

module type GENRING =
sig
	type elmt	(* We do not want to force polygons (that are rings) to use a universal ring, but we try to allow it nonetheless. *)
	type 'a ring

	val empty : elmt ring
	val is_empty : elmt ring -> bool
	val length : elmt ring -> int
	val get : elmt ring -> elmt
	val next : elmt ring -> elmt ring
	val prev : elmt ring -> elmt ring
	val insert_before : elmt ring -> elmt -> elmt ring
	val insert_after : elmt ring -> elmt -> elmt ring
	val remove : elmt ring -> elmt ring
	(* iter from current focus to previous element *)
	val iter : elmt ring -> (elmt ring -> unit) -> unit
	val iter_pairs : elmt ring -> (elmt ring -> elmt ring -> unit) -> unit
	val fold : elmt ring -> (elmt ring -> 'a -> 'a) -> 'a -> 'a
end

module Test_GenRing (R : GENRING) (ELMT_VALUE : sig val v : R.elmt end) =
struct
	let value = ELMT_VALUE.v
	let _ = assert(R.is_empty R.empty = true)
	let _ = assert(R.length R.empty = 0)
	let _ = assert(R.next R.empty = R.empty)
	let _ = assert(R.prev R.empty = R.empty)
	let _ = try (ignore(R.get R.empty) ; assert false) with Not_found -> ()
	let _ = assert(R.get (R.insert_after R.empty value) = value)
	let _ = assert(R.get (R.insert_before R.empty value) = value)
	let _ = try (ignore(R.remove R.empty) ; assert false) with Not_found -> ()
end

module type GENSORTLIST =
sig
	type 'a t

	val empty : ('a -> 'a -> int) -> 'a t
	val is_empty : 'a t -> bool

	val insert : 'a t -> 'a -> 'a t
	val remove : 'a t -> 'a -> 'a t
end

module type GENBAG =
sig
	type 'a t

	val create : int -> 'a t
	val is_empty  : 'a t -> bool

	val put : 'a t -> 'a -> unit
	val take : 'a t -> 'a
	val find : 'a t -> ('a -> bool) -> 'a
end

module type GENTREE =
sig
	module Elmt : ORDERED
	type 'a tree

	val empty : Elmt.t tree
	val is_empty : Elmt.t tree -> bool
	val insert : Elmt.t tree -> Elmt.t -> Elmt.t tree
	val remove : Elmt.t tree -> Elmt.t -> Elmt.t tree
	val find_before : Elmt.t tree -> Elmt.t -> Elmt.t
	val iter : Elmt.t tree -> (Elmt.t -> unit) -> unit
end

(* Then some tools *)

let unopt = function
	| Some x -> x
	| None -> failwith "Cannot unopt None"

let may o f = match o with
	| Some x -> f x
	| None -> ()

