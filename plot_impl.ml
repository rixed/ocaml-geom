open Plot_intf

module Make
	(Pic_: Pic_intf.PIC)
	: PLOT with module Pic = Pic_ =
struct
	module Pic = Pic_
	module Path = Pic.Path
	module Poly = Pic.Poly
	module Point = Poly.Point
	module K = Point.K
	module Algo = Geom_algo.Algorithms (Poly) (Path)

	type data_iterator = (K.t -> K.t -> unit) -> unit
	type ticks_formater = K.t -> string

	type dataset = {
		label : string option ;
		color : Pic_intf.color ;
		with_lines : bool ;
		with_points : bool ;
		iter : data_iterator }

	let next_color =
		let def_colors = [|
			1.0, 0.0, 0.0 ; 0.0, 1.0, 0.0 ; 0.0, 0.0, 1.0 ;
			0.8, 0.8, 0.0 ; 0.8, 0.0, 0.8 ; 0.0, 0.8, 0.8
		|] in
		let next_idx = ref 0 in
		fun () ->
			let ret = def_colors.(!next_idx) in
			next_idx := if !next_idx >= Array.length def_colors then 0 else !next_idx + 1 ;
			ret

	let make_dataset ?label ?color ?(with_lines=true) ?(with_points=false) iter = {
		label = label ;
		color = (match color with	(* TODO: Try to set next_color () as default value *)
			| None -> next_color ()
			| Some col -> col) ;
		with_lines = with_lines ;
		with_points = with_points ;
		iter = iter }

	let def_ticks_formater : ticks_formater = K.to_string
	
	(* FIXME: range belongs to Algo (bbox_of_path) *)
	type range = { xmin : K.t ; xmax : K.t ; ymin : K.t ; ymax : K.t }

	let pic_of_plot ?(show_axis=true) ?(ticks_formater=def_ticks_formater) ?(scale=K.one, K.one) datasets =
		ignore show_axis ;
		ignore (ticks_formater Point.K.zero) ;
		(* First the frame *)
		let frame =
			Algo.poly_of_points [
				Point.zero ; Point.of_2scalars (fst scale, K.zero) ;
				Point.of_2scalars scale ; Point.of_2scalars (K.zero, snd scale) ] in
		let frame_fg = 0., 0., 0. in
		let frame_bg = 1., 1., 1. in
		let frame_gc =
			{ Pic_intf.fill_color = Some frame_bg ; Pic_intf.outline_color = Some frame_fg } in
		(* Then build the path for the plot *)
		let rescale_path path range =
			(* First translate the path so that bottom left point is at origin *)
			let justify_left path =
				Path.translate path (Point.of_2scalars (K.neg range.xmin, K.neg range.ymin)) in
			(* Then rescale it to proper dimentions *)
			let scale path =
				let scale_x = Path.scale_along Point.zero (Point.make_unit 0) in
				let scale_y = Path.scale_along Point.zero (Point.make_unit 1) in
				let x_ratio = K.div (fst scale) (K.sub range.xmax range.xmin) in
				let y_ratio = K.div (snd scale) (K.sub range.ymax range.ymin) in
				try scale_y y_ratio (scale_x x_ratio path)
				with Division_by_zero -> path in
			scale (justify_left path) in
		let path_of_dataset dtset =
			let res = ref None in
			let add_point x y =
				let point = Point.of_2scalars (x, y) in
				res := Some (match !res with
					| None ->
						Path.empty point, { xmin = x ; xmax = x ; ymin = y ; ymax = y }
					| Some (p, r) ->
						Path.extend p point [] Path.make_straight_line, {
							xmin = if x < r.xmin then x else r.xmin ;
							xmax = if x > r.xmax then x else r.xmax ;
							ymin = if y < r.ymin then y else r.ymin ;
							ymax = if y > r.ymax then y else r.ymax }) in
			dtset.iter add_point ;
			match !res with
				| None -> failwith "Empty dataset"
				| Some (p, r) -> rescale_path p r in
		let gc_of_dataset dtset =
			{ Pic_intf.fill_color = Some dtset.color ; Pic_intf.outline_color = None } in
		(Pic.Poly frame, frame_gc) ::
			List.map (fun dtset -> Pic.Path (path_of_dataset dtset), gc_of_dataset dtset) datasets
end
