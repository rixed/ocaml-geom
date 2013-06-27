module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)

module Point = Geom_shapes.Point (G.V)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Draw = Geom_path.Draw (Path)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let draw_poly poly =
    let varray = G.make_vertex_array (Poly.length poly) in
    let idx = ref 0 in
    Poly.iter (fun point ->
        G.vertex_array_set varray !idx point ; incr idx) poly ;
    G.render G.Line_loop varray (G.Uniq G.white)

let background = View.make_viewable "bg" (fun () -> G.clear ~color:G.black ()) View.identity

let box = Algo.poly_of_ascii_repr [
    "   a     b" ;
    "" ;
    "f           c" ;
    "" ;
    "   e     d" ]

let _ = Format.printf "box = %a@." Poly.print box

let box_view =
	let painter = (fun () ->
        G.push_modelview () ;
        draw_poly box ;
        G.pop_modelview ()) in
	View.make_viewable ~parent:background "box"
		painter (View.scaler (fun () -> (G.K.of_float 0.1, G.K.of_float 0.1, G.K.one)))

let camera_pos = ref (G.K.of_float 0., G.K.of_float 0., G.K.of_float 1.)
let camera = View.make_viewable ~parent:background "camera"
	(fun () -> ()) (View.translator (fun () -> !camera_pos))

let () =
    let on_event = function
        | G.Clic (x, y, w, h) ->
            Format.printf "clic @@ %d,%d,%d,%d@." x y w h ;
            let m = View.get_transform ~src:box_view ~dst:camera () in
            let m = G.M.mul_mat (G.get_projection ()) m in
            let p = G.unproject (0,0,w,h) m x (h-y) in
            let ins = Algo.is_inside_poly box p in
            Format.printf "%a %a -> %s@." G.K.print p.(0) G.K.print p.(1) (if ins then "inside" else "outside")
        | _ -> () in
    View.display ~on_event [ fun () -> View.draw_viewable camera ]

