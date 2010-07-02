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

(* A positionner gives the position of a view into its parent.
 * It's thus the transformation from the view to its parent coord system.
 * Also, matrix are stored a la GL, ie first column as first vector. *)
type transfo_dir = View_to_parent | Parent_to_view
type positioner = transfo_dir -> float array array
type viewable = {
	name             : string ;
	painter          : painter ;
	mutable parent   : viewable option ;
	positioner       : positioner ;
	mutable children : viewable list
}

let rec viewable_set_parent ?parent view =
	match parent with
	| None ->
		(match view.parent with
		| None -> ()
		| Some prev_parent ->
			view.parent <- None ;
			prev_parent.children <- List.filter (fun child -> child == view) prev_parent.children)
	| Some new_parent ->
		(match view.parent with
		| None ->
			view.parent <- parent ;
			new_parent.children <- (view::new_parent.children)
		| Some _ ->
			viewable_set_parent view ;
			viewable_set_parent ~parent:new_parent view)

let make_viewable ?parent name painter positioner = 
	let viewable = {
		name = name ;
		painter = painter ;
		parent = None ;
		positioner = positioner ;	(* matrix transforming coords from this viewable to parent *)
		children = []
	} in
	viewable_set_parent ?parent viewable ;
	viewable

(* Transpose the rotation parte and inverse the translation part,
 * thus inversing an orthonormal matrix *)
let transverse pos = [|
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

(* Sets the modelview to transform from root to dst, and returns root *)
let root_to_viewable dst =
	let rec to_root pos = match pos.parent with
		| None -> pos
		| Some parent ->
			GlMat.mult (GlMat.of_array (pos.positioner Parent_to_view)) ;
			to_root parent in
	to_root dst

let viewable_to_root src =
	let rec prepend_next_view root2src view = match view.parent with
		| None -> root2src
		| Some parent -> prepend_next_view (view::root2src) parent in
	let root2src = prepend_next_view [] src in
	List.iter (fun view -> GlMat.mult (GlMat.of_array (view.positioner View_to_parent))) root2src

(* Returns the matrix that transform point coordinates in src to coordinates in dst *)
let get_transform ?src ?dst () =
	GlMat.push () ;
	GlMat.load_identity () ;
	(* start from current transfo = identity matrix
	 * then mult current transfo by all transverse position from dst to root -> gives
	 * the transfo from root to dst *)
	Cnt.may dst (fun d -> ignore (root_to_viewable d)) ;
	(* then mult this by transfo from root to src, ie all positioners from root to src *)
	Cnt.may src viewable_to_root ;
	(* modelview is then :
	 * (VN->dst) o ... o (root->V1) o (vN->root) o ... o (v1->v2) o (src->v1)
	 * then read modelview with : 
	 * glGetFloatv (GL_MODELVIEW_MATRIX, matrix);
	 *)
	let m = GlMat.to_array (GlMat.get_matrix `modelview_matrix) in
	GlMat.pop () ;
	m

let draw_viewable camera =
	let rec aux pos =
		GlMat.push () ;
		GlMat.mult (GlMat.of_array (pos.positioner View_to_parent)) ;
		pos.painter () ;
		List.iter (fun child ->
			aux child)
			pos.children ;
		GlMat.pop () in
	GlMat.load_identity () ;
	aux (root_to_viewable camera)

(* Some simple positioners : *)

let identity _ = [|
	[| 1. ; 0. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. ; 0. |] ;
	[| 0. ; 0. ; 1. ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
|]

let translator get_pos dir =
	let x, y, z = get_pos () in
	let m = [|
		[| 1. ; 0. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. ; 0. |] ;
		[| 0. ; 0. ; 1. ; 0. |] ; [| x ; y ; z ; 1. |]
	|] in
	if dir = View_to_parent then m else transverse m

let scaler get_scale dir =
	let x, y, z = get_scale () in
	match dir with
	| View_to_parent -> [|
			[| x ; 0. ; 0. ; 0. |] ; [| 0. ; y ; 0. ; 0. |] ;
			[| 0. ; 0. ; z ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
		|]
	| Parent_to_view -> [|
			[| 1./.x ; 0. ; 0. ; 0. |] ; [| 0. ; 1./.y ; 0. ; 0. |] ;
			[| 0. ; 0. ; 1./.z ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
		|]

let orientor get_orient dir =
	let c, s = get_orient () in
	let m = [|
		[| c ; s ; 0. ; 0. |] ; [| -.s ; c ; 0. ; 0. |] ;
		[| 0. ; 0. ; 1. ; 0. |] ; [| 0. ; 0. ; 0. ; 1. |]
	|] in
	if dir = View_to_parent then m else transverse m

let trans_orientor get_pos get_orient dir =
	let x, y, z = get_pos () in
	let c, s = get_orient () in
	let m = [|
		[| c ; s ; 0. ; 0. |] ; [| -.s ; c ; 0. ; 0. |] ;
		[| 0. ; 0. ; 1. ; 0. |] ; [| x ; y ; z ; 1. |]
	|] in
	if dir = View_to_parent then m else transverse m

let rotator get_angle dir =
	let a = get_angle () in
	let c, s = cos a, sin a in orientor (fun () -> c, s) dir

let trans_rotator get_pos get_angle dir =
	let a = get_angle () in
	let c, s = cos a, sin a in trans_orientor get_pos (fun () -> c, s) dir

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

