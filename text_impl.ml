open Text_intf

open Freetype
let lib = init ()
let (face, face_info) =
	let font_files = [
		"/usr/share/fonts/truetype/Isabella.ttf" ;
		"/usr/share/fonts/corefonts/arial.ttf" ;
		"/usr/share/fonts/ttf-bitstream-vera/Vera.ttf" ;
		"/usr/share/fonts/alee-fonts/Bandal.ttf" ] in
	let rec init_first = function
		| [] -> failwith "No working font file !"
		| file :: others ->
			try
				let ret = new_face lib file 0 in
				Format.printf "Using true type font file %s@." file ;
				ret
			with Failure _ -> init_first others in
	init_first font_files

module Glyph
	(Poly_: Geom.POLYGON)
	(Path_: Geom.PATH with module Point = Poly_.Point)
	: GLYPH with module Poly = Poly_ and module Path = Path_ =
struct
	module Poly = Poly_
	module Path = Path_
	module Point = Poly.Point
	module Algo = Geom_algo.Algorithms (Poly) (Path)

	type t = {
		index : char_index ;
		paths : Path.t list ;
		advance_x : float ;
		advance_y : float }

	let make chr =
		let index = get_char_index face (int_of_char chr) in
		let (advance_x, advance_y) = load_glyph face index [ Load_no_scale ; Load_no_hinting ] in
		let outline = get_outline_contents face in
		let to_point (x, y) =
			let xs = Poly.Point.K.of_float x in
			let ys = Poly.Point.K.of_float y in
			[| xs ; ys ; Poly.Point.K.zero |] in
		let rec path_of_contour next last path =
			if next > last then path else (
				match outline.tags.(next) with
				| On_point -> path_of_contour (next+1) last
					(Path.extend path (to_point outline.points.(next)) [] Path.make_straight_line)
				| Off_point_conic ->
					if next + 1 > last then path (* Sometime the outline ends on a point conic *) else (
						let middle_point (x1,y1) (x2,y2) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.) in
						let (next_current, next_next) =
							if outline.tags.(next+1) = On_point then
								(outline.points.(next+1), next+2)
							else
								(middle_point outline.points.(next) outline.points.(next+1), next+1)
						in
						path_of_contour next_next last
							(Path.extend path (to_point next_current) [to_point outline.points.(next)] Path.make_bezier_curve)
					)
				| Off_point_cubic -> path_of_contour (next+3) last
					(Path.extend path (to_point outline.points.(next+2))
						[to_point outline.points.(next) ; to_point outline.points.(next+1)] Path.make_bezier_curve)
			) in
		let get_path c =
			let first = if c = 0 then 0 else outline.contours.(c-1)+1 in
			let last = outline.contours.(c) in
			path_of_contour (first+1) last (Path.empty (to_point outline.points.(first))) in
		let get_all_paths () =
			let paths = ref [] in
			for c = 0 to outline.n_contours-1 do paths := get_path c :: !paths done ;
			!paths in
		{
			index = index ;
			paths = get_all_paths () ;
			advance_x = advance_x ;
			advance_y = advance_y }

	let to_poly glyph prec =
		let rec to_polys polys = function
			| [] -> polys
			| path :: other ->
				to_polys (Algo.poly_of_path path prec :: polys) other in
		let is_clockwise polys =
			Point.K.compare (Algo.area polys) Point.K.zero < 0 in
			(*0 <> (outline.flags land 4) (* Too bad we can't trust this flag *) *)
		let polys = to_polys [] glyph.paths in
		if is_clockwise polys then Algo.inverse polys else polys
	
	let bbox glyph =
		let rec extend_bbox current = function
			| [] -> current
			| path :: other ->
				extend_bbox (Point.Bbox.union current (Path.bbox path)) other in
		extend_bbox Point.Bbox.empty glyph.paths
	
	let advance ?(orientation=Horizontal) prev_glyph next_glyph =
		match orientation with
		| Horizontal ->
			let kern_vec = get_kerning face prev_glyph.index next_glyph.index Kerning_unscaled in
			[| Point.K.of_float (prev_glyph.advance_x +. kern_vec.ft_x) ;
			   Point.K.of_float kern_vec.ft_y |]
		| Vertical ->
			[| Point.K.zero ;
			   Point.K.of_float prev_glyph.advance_y |]
end

module Word
	(Glyph_: GLYPH)
	: WORD with module Glyph = Glyph_ =
struct
	module Glyph = Glyph_
	module Poly = Glyph.Poly
	module Path = Glyph.Path
	module Point = Poly.Point
	module Algo = Geom_algo.Algorithms (Poly) (Path)

	type t = (Glyph.t * Point.t) list

	let make ?(orientation=Horizontal) str =
		let rec add_char i word pos =
			if i >= String.length str then
				word
			else
				let c = str.[i] in
				(* TODO: use also previous char to choose a better glyph for 2 successive chars *)
				let glyph = Glyph.make c in
				let adv = match word with
				| [] -> pos
				| (prev_g, _) :: _ ->
					let advance = Glyph.advance ~orientation prev_g glyph in
					Point.add pos advance in
				let next_word = (glyph, adv) :: word in
				add_char (i+1) next_word adv in
		add_char 0 [] Point.zero

	let bbox word = 
		(* A glyph has no position since it's only the "pure", abstract representation of a symbol.
		   But glyphs in words are positionned. So we must compute the bbox as the union of all
		   glyph's bboxes translated to match glyph potision in word. *)
		let rec aux bbox = function
			| [] -> bbox
			| (glyph, pos) :: others ->
				let translate_bbox pos = function
					| Point.Bbox.Empty -> Point.Bbox.Empty
					| Point.Bbox.Box (x, y) ->
						Point.Bbox.Box (Point.add x pos, Point.add y pos) in
				let g_bbox = translate_bbox pos (Glyph.bbox glyph) in
				let new_bbox = Point.Bbox.union bbox g_bbox in
				aux new_bbox others in
		aux Glyph.Poly.Point.Bbox.empty word

	let to_poly word prec =
		let rec add_glyph polys = function
			| [] -> polys
			| (glyph, pos) :: others ->
				let new_polys = Algo.translate_poly (Glyph.to_poly glyph prec) pos in
				add_glyph (new_polys @ polys) others in
		add_glyph [] word
end
