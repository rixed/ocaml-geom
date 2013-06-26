module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)

module Point = Geom_shapes.Point (G.V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Draw = Geom_path.Draw (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let draw_poly polys =
	let draw_single poly =
		let varray = G.make_vertex_array (Poly.length poly) in
		let idx = ref 0 in
		Poly.iter (fun point ->
			G.vertex_array_set varray !idx point ; incr idx) poly ;
		G.render G.Line_loop varray (G.Uniq G.white) in
	List.iter draw_single polys

let background = View.make_viewable "bg" (fun () -> G.clear ~color:G.black ()) View.identity

let letters_view =
	let box = Draw.box
		[| G.K.of_float (-10.) ; G.K.of_float (-5.) |]
		[| G.K.of_float 10. ; G.K.of_float 5. |]
		(G.K.of_float 0.3) in
	let ppolys = List.map (fun (radius, y) ->
		let pos = [| G.K.zero ; G.K.of_float y |]
		and radius = G.K.of_float radius in
		let paths = Path.rounded ~radius box in
		let prec = G.K.of_float 0.01 in
		pos, Algo.polys_of_paths paths prec) [ 0.1, -15. ; 1., 0. ; 2.5, 15. ]  in
	let painter = (fun () ->
		List.iter (fun (pos, polys) ->
			G.push_modelview () ;
			G.mult_modelview (G.M.translate pos.(0) pos.(1) G.K.zero) ;
			draw_poly polys ;
			G.pop_modelview ()) ppolys) in
	View.make_viewable ~parent:background "box"
		painter (View.scaler (fun () -> (G.K.of_float 0.04, G.K.of_float 0.04, G.K.one)))

let camera_pos = ref (G.K.of_float 0., G.K.of_float 0., G.K.of_float 0.5)
let camera = View.make_viewable ~parent:background "camera"
	(fun () -> ()) (View.translator (fun () -> !camera_pos))

let () = View.display [ (fun () -> View.draw_viewable camera) ]

