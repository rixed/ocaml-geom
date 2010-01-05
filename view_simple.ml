module Make_painter (P_ : Geom.POLYGON)
	: View.PAINTER with module Poly = P_ =
struct
	module Poly = P_
	module Point = Poly.Point
	module K = Point.K

	let width = 0.05

	let point3_of_vector v = Point.repr3d v

	let draw_vector_in_color rgb p v =
		GlDraw.color rgb;
		let v_start = p in
		let v_stop = Point.add v_start v in
		GlDraw.begins `lines ;
		GlDraw.vertex3 (point3_of_vector v_start) ;
		GlDraw.vertex3 (point3_of_vector v_stop) ;
		(* Arrow head *)
		let ratio = K.of_float 0.2 in
		let v1 = Point.mul v ratio in
		let v2 = Point.half (Point.mul (Point.right_turn v) ratio) in
		GlDraw.vertex3 (point3_of_vector v_stop) ;
		GlDraw.vertex3 (point3_of_vector (Point.add (Point.sub v_stop v1) v2)) ;
		GlDraw.vertex3 (point3_of_vector v_stop) ;
		GlDraw.vertex3 (point3_of_vector (Point.sub (Point.sub v_stop v1) v2)) ;
		GlDraw.ends ()
	
	let draw_vector = draw_vector_in_color (0.6, 0.9, 0.9)

	let draw_point p =
		let point_color = 0.8, 0.7, 0.3 in
		let (x, y, z) = point3_of_vector p in
		GlDraw.color point_color ;
		GlDraw.begins `lines ;
		GlDraw.vertex3 (x -. width, y -. width, z) ;
		GlDraw.vertex3 (x +. width, y +. width, z) ;
		GlDraw.vertex3 (x -. width, y +. width, z) ;
		GlDraw.vertex3 (x +. width, y -. width, z) ;
		GlDraw.ends ()
	
	let draw_single_poly poly =
		Poly.iter poly (fun p ->
			let point = Poly.get p in
			draw_point point ;
			draw_vector point (Point.sub (Poly.get (Poly.next p)) point))
	
	let draw_poly polys = List.iter draw_single_poly polys

	let draw_grid_xy ~disp ~rgb (xmin, ymin) (xmax, ymax) =
		GlDraw.color rgb ;
		GlDraw.begins `lines ;
		let rec auxx x =
			GlDraw.vertex2 (x, ymin) ; GlDraw.vertex2 (x, ymax) ;
			if x <= xmax then auxx (x +. disp) in
		let rec auxy y =
			GlDraw.vertex2 (xmin, y) ; GlDraw.vertex2 (xmax, y) ;
			if y <= ymax then auxy (y +. disp) in
		auxx xmin ;
		auxy ymin ;
		GlDraw.ends ()

	let draw_background () =
		GlClear.color (0.3, 0.3, 0.4) ;
		GlClear.clear [`color] ;
		let total_grid = draw_grid_xy (-10.0, -9.0) (10.0, 9.0) in (* FIXME: compute from prj+view *)
		total_grid ~disp:0.25 ~rgb:(0.32, 0.32, 0.42) ;
		total_grid ~disp:1.0 ~rgb:(0.35, 0.35, 0.45) ;
		draw_vector_in_color (1., 0., 0.) Point.zero (Point.of_3scalars (K.one, K.zero, K.zero)) ;
		draw_vector_in_color (0., 1., 0.) Point.zero (Point.of_3scalars (K.zero, K.one, K.zero)) ;
		draw_vector_in_color (0., 0., 1.) Point.zero (Point.of_3scalars (K.zero, K.zero, K.one))

	let draw_turtle pos dir =
		let center_vec = pos in
		let head_vec = Point.add center_vec dir in
		let ortho_vec = Point.half (Point.right_turn dir) in
		GlDraw.color (0.9, 0.86, 1.) ;
		GlDraw.begins `line_loop ;
		GlDraw.vertex3 (point3_of_vector head_vec) ;
		GlDraw.vertex3 (point3_of_vector (Point.add center_vec ortho_vec)) ;
		GlDraw.vertex3 (point3_of_vector (Point.sub center_vec ortho_vec)) ;
		GlDraw.ends ()
end
