module type GENTREE =
sig
  module Elmt : Pfds_intf.ORDERED
  type 'a tree

  val empty : Elmt.t tree
  val is_empty : Elmt.t tree -> bool
  val insert : Elmt.t tree -> Elmt.t -> Elmt.t tree
  val remove : Elmt.t tree -> Elmt.t -> Elmt.t tree
  val min : Elmt.t tree -> Elmt.t
  val find_before : Elmt.t tree -> Elmt.t -> Elmt.t
  val iter : Elmt.t tree -> (Elmt.t -> unit) -> unit
end

