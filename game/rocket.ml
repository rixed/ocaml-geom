open Mlrocket

type t =
	{ name  : string ;
	  poly  : Poly.t ;
	  mutable pos      : Point.t ;
      mutable prev_pos : Point.t ;
	  mutable orient   : G.V.t ;
	  mutable thrust   : K.t ;
	  mutable speed    : G.V.t ;
      mutable viewable : View.viewable option }

let make_shape () =
	let (++) p1 p2 = Path.extend p1 p2 [] Path.make_straight_line in
	let width = Point.K.half Point.K.one in
	let triangle_path = 
		(Path.empty (Point.make_unit 0)) ++
		[| Point.K.neg Point.K.one ; width |] ++
		[| Point.K.neg Point.K.one ; Point.K.neg width |] in
	Algo.poly_of_path triangle_path Point.K.one

let make =
    let nb_rockets = ref 0 in
    fun init_pos ->
        incr nb_rockets ;
        mlog "\tBuilding rocket %d..." !nb_rockets ;
        { name = "TheRocket!"^
                     (if !nb_rockets > 1 then " ("^ string_of_int !nb_rockets ^")" else "");
          poly = make_shape () ;
          pos = init_pos ;
          prev_pos = init_pos ;
          orient = G.V.make_unit 0 ;
          thrust = K.zero ;
    	  speed  = G.V.zero ;
          viewable = None }

let poly rocket = rocket.poly
let pos rocket = rocket.pos
let prev_pos rocket = rocket.prev_pos
let orient rocket = rocket.orient
let speed rocket = rocket.speed
let set_orient rocket orient = rocket.orient <- orient
let set_thrust rocket thrust = rocket.thrust <- thrust
let set_viewable rocket v = rocket.viewable <- Some v
let viewable rocket = Bricabrac.unopt rocket.viewable

let run gravity dt rocket =
	let s = K.mul dt rocket.thrust in
	let thrust = G.V.mul s rocket.orient in
	let gravity' = Point.K.mul gravity dt in
	let g = G.V.mul gravity' (G.V.normalize rocket.pos) in
    rocket.prev_pos <- rocket.pos ;
	rocket.speed <- G.V.add rocket.speed (G.V.add thrust g) ;
	rocket.pos   <- G.V.add rocket.pos rocket.speed ;
	(* loose 9/10th of your thrust every second *)
    let r = 0.1 ** K.to_float dt in
	rocket.thrust <- K.mul (K.of_float r) rocket.thrust
