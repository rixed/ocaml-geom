open Algen_intf

module Point (Vector : VECTOR)
  : Geom.POINT with module K = Vector.K =
struct
  include Vector
  open K.Infix

  let origin = zero

  let area a b = (a.(0) *~ b.(1)) -~ (a.(1) *~ b.(0))

  let compare_left p0 p1 p =
    let v0 = sub p1 p0 in
    let v1 = sub p p0 in
    K.compare (area v0 v1) K.zero
  
  let compare_segment ?(closed=true) p0 p1 p =
    let vl = sub p0 p1 in
    let vp = sub p0 p in
    let scal = scalar_product vl vp in
    let cmp1 = K.compare scal K.zero in
    if cmp1 < 0 then false else
    if cmp1 = 0 then closed else
    let cmp2 = K.compare (K.square scal) (norm2 vl) in
    if cmp2 > 0 then false else
    if cmp2 = 0 then closed else
    true
  
  let compare_coord c p0 p1 =
    K.compare p0.(c) p1.(c)

  let compare_x = compare_coord 0
  
  let compare_y = compare_coord 1

  let distance2 p0 p1 = norm2 Infix.(p1 -~ p0)
  let distance p0 p1 = K.sqrt (distance2 p0 p1)

  let intersect ?(closed=true) p0 p1 q0 q1 =
    let (^^) a b = (* Exclusive logical or *)
      if a then not b else b in
    let at_left a b c = (compare_left a b c) > 0 in
    if p0 == q0 || p1 == q1 then closed else
    (* Only one of {q0,q1} is at left of (p0,p1) *)
    ((at_left p0 p1 q0) ^^ (at_left p0 p1 q1)) &&
    (* Only one of {p0,p1} is at left of (q0,q1) *)
    ((at_left q0 q1 p0) ^^ (at_left q0 q1 p1))

  let turn_right = function
    | [| x; y |] -> [| y; ~-~ x |]
    | _ -> assert false

  let turn_left = function
    | [| x; y |] -> [| ~-~ y; x |]
    | _ -> assert false

  let determinant a b c d = (a *~ d) -~ (b *~ c)

  let intersection ?(epsilon=K.of_float 1e-8) p0 p1 q0 q1 =
    let x1 = p0.(0) and y1 = p0.(1)
    and x2 = p1.(0) and y2 = p1.(1)
    and x3 = q0.(0) and y3 = q0.(1)
    and x4 = q1.(0) and y4 = q1.(1) in
    let den = determinant (x1 -~ x2) (y1 -~ y2) (x3 -~ x4) (y3 -~ y4) in
    if K.abs den <=~ epsilon then None else
    let det1 = determinant x1 y1 x2 y2
    and det2 = determinant x3 y3 x4 y4 in
    Some [|
      (determinant det1 (x1 -~ x2) det2 (x3 -~ x4)) /~ den ;
      (determinant det1 (y1 -~ y2) det2 (y3 -~ y4)) /~ den |]

  let center p1 p2 =
    let p = add p1 p2 in
    mul (K.half K.one) p

  let scale ?center ratio p =
    match center with
    | None -> mul ratio p
    | Some c ->
      add c (mul ratio (sub p c))
end

module Polygon
  (Point : Geom.POINT)
  : Geom.POLYGON with module Point = Point =
struct
  module Point = Point

  module Ring = Ring_impl.Ring
  type 'a ring = 'a Ring.t
  include (Ring : Pfds_intf.RING_GEN with type 'a t := 'a ring)

  type t = Point.t ring
  
  let iter_pairs f t =
    let rec aux t1 f n =
      if n > 0 then (
        f t1 ;
        aux (next t1) f (n-1)
      ) in
    let len = ref (length t) in
    iterr (fun t0 ->
      decr len ;
      aux (next t0) (f t0) !len) t

  let iter_edges t f =
    iterr (fun t ->
      let p0 = get t and p1 = get (next t) in
      f p0 p1) t

  let print ff t =
    let focus = get t in
    Format.pp_open_box ff 0 ;
    Format.pp_print_string ff "{" ; Format.pp_print_space ff () ;
    iter
      (fun point ->
        if point == focus then Format.pp_print_string ff "*" ;
        Point.print ff point ; Format.pp_print_space ff ())
      t ;
    Format.pp_print_string ff "}" ;
    Format.pp_close_box ff ()

  let map_edges f t =
    (* Move prev_stop, last prev_ctrls next_start and first next_ctrls
     * to make prev_stop and next_start equal: *)
    let pp = Point.print in
    let connect prev_start prev_stop
                next_start next_stop =
      if Point.eq prev_stop next_start then true else (
          (* Do not try hard to use the intersection if it's too far away.
           * In that case we'd rather draw a nice and clean straight line
           * in between prev_stop and next_start: *)
          match Point.intersection ~epsilon:(Point.K.of_float 0.5)
                                   prev_start prev_stop
                                   next_start next_stop with
          | None -> false
          | Some inters ->
            Point.copyi prev_stop inters ;
            Point.copyi next_start inters ;
            Format.printf "Moved prev_stop and next_start to %a@." pp inters ;
            true
      ) in
    let p =
      fold_leftr (fun p t ->
          let start = get t and stop = get (next t) in
          let start', stop' = f start stop in
          Format.printf "Mapping %a,%a into %a,%a@."
            pp start pp stop pp start' pp stop' ;
          if is_empty p then (
            (* If we haven't inserted anything yet, no question asked,
             * but we will get back to it at the end: *)
            insert_after (insert_after p start') stop'
          ) else (
            let prev_stop = get p and prev_start = get (prev p) in
            if connect prev_start prev_stop start' stop' then
              insert_after p stop'
            else
              (* No intersection? Let's just keep all our points as they
               * are. *)
              insert_after (insert_after p start') stop'
          )
        ) empty t in
    (* We haven't "connected" the first and the last points.
     * After fold_leftr we end up with p cursor on the last inserted
     * point. So we must _merge_ it with the next one (for so far,
     * we have one extra point): *)
    let prev_start = get (prev p) and prev_stop = get p
    and next_start = get (next p) and next_stop = get (next (next p)) in
    if connect prev_start prev_stop next_start next_stop then
      (* So now prev_stop and next_start have been moved to the same
       * position, and we must get rid of one of them. We remove prev_stop so
       * we end up with the cursor on the starting point, which is nice:*)
      remove p
    else
      (* We failed to connect? Simply keep both of them then! But still,
       * let's focus on the starting point: *)
      next p

  let translate v t =
    map (fun p -> Point.add p v) t

  let scale ?center ratio poly =
    map (Point.scale ?center ratio) poly

  let rotate ?(center=Point.origin) ang poly =
    let c = Point.K.(of_float (cos (to_float ang)))
    and s = Point.K.(of_float (sin (to_float ang))) in
    map (fun p ->
      let open Point.K.Infix in
      let p = Point.sub p center in
      let p = [| c *~ p.(0) -~ s *~ p.(1) ;
                 s *~ p.(0) +~ c *~ p.(1) |] in
      Point.add p center) poly

  module IsInside = Geom.MakeIsInside (Point.K)
  let is_inside t point =
    IsInside.is_inside (iter_edges t) point
end
