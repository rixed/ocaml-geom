open Mlrocket

type color = G.color_specs

type gc = {
	fill_color : color option ;
	outline_color : color option
}

type elmt = Poly of Poly.t | Path of Path.t | Dot of Poly.Point.t | Clear
type t = (elmt * gc) list

let draw ?(prec=Point.K.one) pic =
    let draw_elmt (elmt, gc) =
        let draw t length iter prim =
            let varray = G.make_vertex_array (length t) in
            let idx = ref 0 in
            iter (fun point -> G.vertex_array_set varray !idx point ; incr idx) t ;
            (* Bricabrac.may gc.fill_color    (G.render G.Line_what? varray) ; *)
            Bricabrac.may gc.outline_color (G.render prim varray) in
        let draw_poly poly =
            draw poly Poly.length Poly.iter G.Line_loop in
        let draw_path path =
            let len = ref 0 in
            Path.iter prec path (fun _ -> incr len) ;
            draw path (fun _ -> !len) (fun f p -> Path.iter prec p f) G.Line_strip in
        let draw_point point =
            draw point (fun _ -> 1) (fun f t -> f t) G.Dot in
        let clear () =
            Bricabrac.may gc.fill_color (function
            | G.Uniq color -> G.clear ~color ()
            | G.Array _ -> ()) in
        match elmt with
            | Poly poly -> draw_poly poly
            | Path path -> draw_path path
            | Dot point -> draw_point point
            | Clear -> clear () in
    List.iter draw_elmt pic

let bbox _pic = Point.Bbox.empty (* TODO *)
