open Freetype
let lib = init ()
let font_file =
	(*"/usr/share/fonts/truetype/Isabella.ttf"*)
	"/usr/share/fonts/corefonts/arial.ttf"
let (face, face_info) = new_face lib font_file 0

module Make
	(Algo : Geom.ALGORITHMS)
	: Geom.TEXT with module Poly = Algo.Poly and module Path = Algo.Path =
struct
	module Poly = Algo.Poly
	module Path = Algo.Path

	let poly_of_glyph glyph resolution =
		let (_advance_x, _advance_y) = load_char face glyph [Load_no_scale;Load_no_hinting] in
		let outline = get_outline_contents face in
		let to_point (x, y) =
			let xs = Poly.Point.K.of_float x in
			let ys = Poly.Point.K.of_float y in
			Algo.Poly.Point.of_3scalars (xs, ys, Poly.Point.K.zero) in
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
		let polys = ref [] in
		for c = 0 to outline.n_contours-1 do
			let first = if c = 0 then 0 else outline.contours.(c-1)+1 in
			let last = outline.contours.(c) in
			let path = path_of_contour (first+1) last (Path.empty (to_point outline.points.(first))) in
			polys := (Algo.inverse_single (Algo.poly_of_path path resolution))::!polys
		done ;
(*		Format.printf "Poly list = " ;
		List.iter (fun p -> Format.printf "@[%a@]@," Poly.print p) !polys ;
		Format.printf " =>@,%a@\n" Poly.print (Algo.simplify !polys) ; *)
		!polys

end
