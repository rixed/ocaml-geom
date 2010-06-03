open Pic_intf

module Make
	(Poly_: Geom.POLYGON)
	(Path_: Geom.PATH with module Point = Poly_.Point)
	: PIC with module Poly = Poly_ and module Path = Path_ =
struct
	module Poly = Poly_
	module Path = Path_
	module Point = Poly.Point

	type elmt = Poly of Poly.t | Path of Path.t | Dot of Point.t

	type t = (elmt * gc) list

	let draw ?(prec=Point.K.one) pic =
		let draw_elmt (elmt, gc) =
			let draw_iter_prim iter prim color =
				GlDraw.color color ;
				GlDraw.begins prim ;
				iter (fun pt -> GlDraw.vertex3 (Point.to_point3 pt)) ;
				GlDraw.ends () in
			let draw_poly poly =
				let poly2point_iter f =
					Poly.iter poly (fun pol -> f (Poly.get pol)) in
				Cnt.may gc.fill_color (draw_iter_prim poly2point_iter `polygon) ;
				Cnt.may gc.outline_color (draw_iter_prim poly2point_iter `line_loop) in
			let draw_path path =
				Cnt.may gc.fill_color (draw_iter_prim (Path.iter path prec) `line_strip) in
			let draw_point point =
				Cnt.may gc.fill_color (draw_iter_prim (fun f -> f point) `points) in
			match elmt with
				| Poly poly -> draw_poly poly
				| Path path -> draw_path path
				| Dot point -> draw_point point in
		List.iter draw_elmt pic

	let bbox _pic = Point.empty_bbox (* TODO *)
end
