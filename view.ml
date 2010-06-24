module type PAINTER =
sig
	module Poly : Geom.POLYGON

	val draw_vector : Poly.Point.t -> Poly.Point.t -> unit
	val draw_point : Poly.Point.t -> unit
	val draw_single_poly : Poly.t -> unit
	val draw_poly : Poly.t list -> unit
	val draw_background : unit -> unit
	val draw_turtle : Poly.Point.t -> Poly.Point.t -> unit
end

type painter = unit -> unit

type positioner = unit -> float array array
type viewable = {
	name : string ; painter : painter ; parent : viewable option ;
	positioner : positioner ; mutable children : viewable list
}

let make_viewable ?parent name painter positioner = 
	let viewable = {
		name = name ;
		painter = painter ;
		parent = parent ;
		positioner = positioner ;
		children = []
	} in
	let add_child parent = parent.children <- viewable :: parent.children in
	Cnt.may parent add_child ;
	viewable

let inverse pos = [|
	(* Transpose rotation part *)
	[| pos.(0).(0) ; pos.(1).(0) ; pos.(2).(0) ; 0. |] ;
	[| pos.(0).(1) ; pos.(1).(1) ; pos.(2).(1) ; 0. |] ;
	[| pos.(0).(2) ; pos.(1).(2) ; pos.(2).(2) ; 0. |] ;
	(* Compute translation part *)
	[|
		-.pos.(0).(0)*.pos.(3).(0) -. pos.(0).(1)*.pos.(3).(1) -. pos.(0).(2)*.pos.(3).(2) ;
		-.pos.(1).(0)*.pos.(3).(0) -. pos.(1).(1)*.pos.(3).(1) -. pos.(1).(2)*.pos.(3).(2) ;
		-.pos.(2).(0)*.pos.(3).(0) -. pos.(2).(1)*.pos.(3).(1) -. pos.(2).(2)*.pos.(3).(2) ;
		1.
	|]
|]

let rec draw_viewable camera =
	let rec to_root pos =
		match pos.parent with
			| None -> pos
			| Some parent ->
				GlMat.mult (GlMat.of_array (inverse (pos.positioner ()))) ;
				to_root parent in
	let rec aux pos =
		GlMat.push () ;
		GlMat.mult (GlMat.of_array (pos.positioner ())) ;
		pos.painter () ;
		List.iter (fun child ->
			aux child)
			pos.children ;
		GlMat.pop () in
	GlMat.load_identity () ;
	aux (to_root camera)

(* Some simple positioners : *)

let identity () = [|
	[| 1. ; 0. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. ; 0. |] ;
	[| 0. ; 0. ; 1. ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
|]

let translator get_pos () = let x, y, z = get_pos () in [|
	[| 1. ; 0. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. ; 0. |] ;
	[| 0. ; 0. ; 1. ; 0. |] ; [| x ; y ; z ; 1. |]
|]

let scaler get_scale () = let x, y, z = get_scale () in [|
	[| x ; 0. ; 0. ; 0. |] ; [| 0. ; y ; 0. ; 0. |] ;
	[| 0. ; 0. ; z ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
|]

let orientor get_orient () = let c, s = get_orient () in [|
	[| c ; s ; 0. ; 0. |] ; [| -.s ; c ; 0. ; 0. |] ;
	[| 0. ; 0. ; 0. ; 1. |] ; [| 0. ; 0. ; 0. ; 1. |]
|]

let trans_orientor get_pos get_orient () =
	let x, y, z = get_pos () in
	let c, s = get_orient () in [|
	[| c ; s ; 0. ; 0. |] ; [| -.s ; c ; 0. ; 0. |] ;
	[| 0. ; 0. ; 0. ; 1. |] ; [| x ; y ; z ; 1. |]
|]

let rotator get_angle () =
	let a = get_angle () in
	let c, s = cos a, sin a in orientor (fun () -> c, s) ()

let trans_rotator get_pos get_angle () =
	let a = get_angle () in
	let c, s = cos a, sin a in trans_orientor get_pos (fun () -> c, s) ()

(* FIXME: protect with a mutex ? *)
let last_clics = ref []

let win_size = ref (640, 480)
let screen_to_coord = ref 1.
let unproject (xs, ys) =
	float (xs - fst !win_size / 2) *. !screen_to_coord,
	float (snd !win_size / 2 - ys) *. !screen_to_coord

let display ?onclic ?timer ?(fps=5) painters =
	ignore (Glut.init ~argv:Sys.argv) ;
	Glut.initDisplayMode ~alpha:false ~double_buffer:true ~depth:false () ;
	Glut.initWindowSize ~w:(fst !win_size) ~h:(snd !win_size) ;
	ignore (Glut.createWindow ~title:"Geometry Viewer") ;
    Glut.reshapeFunc ~cb:(fun ~w ~h ->
		win_size := w, h ;
		GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h ;
		GlMat.mode `projection ;
		GlMat.load_identity () ;
		if w > h then (
			let r = float w /. float h in
			GlMat.ortho ~x:(-.r, r) ~y:(-1., 1.) ~z:(0.1, 10.) ;
			screen_to_coord := 2. /. float h
		) else (
			let r = float h /. float w in
			GlMat.ortho ~x:(-1., 1.) ~y:(-.r, r) ~z:(0.1, 10.) ;
			screen_to_coord := 2. /. float w
		) ;
		GlMat.mode `modelview ;
		GlMat.load_identity()) ;
	Glut.mouseFunc ~cb:(fun ~button ~state ~x ~y ->
		if button = Glut.LEFT_BUTTON && state = Glut.DOWN then (
			let xc, yc = unproject (x, y) in
			last_clics := (xc, yc) :: !last_clics ;
			Cnt.may onclic (fun f -> f (xc, yc)) ;
			Glut.postRedisplay ()
		)) ;
    Glut.displayFunc ~cb:(fun () ->
		List.iter (fun painter -> painter ()) painters ;
		Glut.swapBuffers()) ;
	(match timer with Some cb ->
		let ms = 1000/fps in
		let rec timerFunc ~value =
			Glut.timerFunc ~ms:ms ~cb:timerFunc ~value:value ;
			cb () ;
			Glut.postRedisplay () in
		Glut.timerFunc ~ms:ms ~cb:timerFunc ~value:()
	| _ -> ()) ;
	Glut.mainLoop ()

