(* API inspired from http://www.solutoire.com/experiments/flotr/examples/basic-axis.html *)

module type PLOT =
sig
	module Pic : Pic_intf.PIC

	type dataset
	type data_iterator = (Pic.Poly.Point.K.t -> Pic.Poly.Point.K.t -> unit) -> unit
	type ticks_formater = Pic.Poly.Point.K.t -> string

	val make_dataset : ?label:string -> ?color:Pic_intf.color -> ?with_lines:bool -> ?with_points:bool -> data_iterator -> dataset

	val pic_of_plot :
		?show_axis:bool ->
		?ticks_formater:ticks_formater ->
		?scale:(Pic.Poly.Point.K.t * Pic.Poly.Point.K.t) ->
		dataset list ->
		Pic.t
end

