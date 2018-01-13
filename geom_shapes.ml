open Algen_intf

module Point (Vector : VECTOR)
  : Geom.POINT with module K = Vector.K and module Bbox = Vector.Bbox =
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

  let rotate ?(center=origin) ang =
    let c = K.(of_float (cos (to_float ang)))
    and s = K.(of_float (sin (to_float ang))) in
    fun p ->
      let p = sub p center in
      let p = [| c *~ p.(0) -~ s *~ p.(1) ;
                 s *~ p.(0) +~ c *~ p.(1) |] in
      add p center

  let determinant a b c d = (a *~ d) -~ (b *~ c)

  (* TODO: a K.epsilon *)
  let intersection ?(epsilon=K.zero) p0 p1 q0 q1 =
    let x1 = p0.(0) and y1 = p0.(1)
    and x2 = p1.(0) and y2 = p1.(1)
    and x3 = q0.(0) and y3 = q0.(1)
    and x4 = q1.(0) and y4 = q1.(1) in
    let den = determinant (x1 -~ x2) (y1 -~ y2) (x3 -~ x4) (y3 -~ y4) in
    (* den will be zero if lines are parallel.
     * FIXME: In this case we should check if the lines are actually
     * the same and return a chosen value in that case (caller might want
     * to consider identical lines as sequent or not). *)
    if K.abs den <=~ epsilon then None else
    let det1 = determinant x1 y1 x2 y2
    and det2 = determinant x3 y3 x4 y4 in
    Some [|
      (determinant det1 (x1 -~ x2) det2 (x3 -~ x4)) /~ den ;
      (determinant det1 (y1 -~ y2) det2 (y3 -~ y4)) /~ den |]

  type segment_intersection =
    | Parallel | IntersectInside of t | IntersectOutside of t

  let segment_intersection ?(epsilon=K.zero) p0 p1 q0 q1 =
    let den = p0.(0) *~ (q1.(1) -~ q0.(1)) +~
              p1.(0) *~ (q0.(1) -~ q1.(1)) +~
              q1.(0) *~ (p1.(1) -~ p0.(1)) +~
              q0.(0) *~ (p0.(1) -~ p1.(1)) in
    (* FIXME: same as above, but additionally we want to return the actual
     * intersection of the segments when they are indeed intersecting. *)
    if den <=~ epsilon then Parallel else
    let num1 = p0.(0) *~ (q1.(1) -~ q0.(1)) +~
               q0.(0) *~ (p0.(1) -~ q1.(1)) +~
               q1.(0) *~ (q0.(1) -~ p0.(1)) in
    let s = num1 /~ den in
    let num2 = p0.(0) *~ (q0.(1) -~ p1.(1)) +~
               p1.(0) *~ (p0.(1) -~ q0.(1)) +~
               q0.(0) *~ (p1.(1) -~ p0.(1)) |>
               K.neg in
    let t = num2 /~ den in
    let inside = K.zero <=~ s && s <=~ K.one &&
                 K.zero <=~ t && t <=~ K.one
    and intersection = Infix.(p0 +~ Vector.mul s (p1 -~ p0)) in
    if inside then IntersectInside intersection
              else IntersectOutside intersection

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
  
  let debug = false

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

  let print_list ff lst =
    Format.fprintf ff "[@[" ;
    List.iter (fun p ->
      Format.fprintf ff "%a@," print p) lst ;
    Format.fprintf ff "@]]"

  let map_edges ?(min_dist2=Point.K.one) f t =
    let pp = Point.print in
    (* Move prev_stop, last prev_ctrls next_start and first next_ctrls
     * to make prev_stop and next_start equal: *)
    let connect prev_start prev_stop
                next_start next_stop =
      let dist2 = Point.distance2 prev_stop next_start in
      if Point.K.compare dist2 min_dist2 <= 0 then (
          (* Points are close, merge them to avoid generating a self-crossing
           * poly: *)
          true
      ) else (
          match Point.segment_intersection prev_start prev_stop
                                           next_start next_stop with
          | Point.Parallel | Point.IntersectOutside _ ->
            (* TODO: if the line intersection is not too far away, use it
             * as below to save one point? *)
            (*let inters_dist2 = Point.distance2 inters prev_stop in
            if inters_dist2 > Point.K.double dist2 then false else ... *)
            if debug then
              Format.printf "No intersection between@ %a - %a and@ %a %a@."
                Point.print prev_start Point.print prev_stop
                Point.print next_start Point.print next_stop ;
            false
          | Point.IntersectInside inters ->
            Point.copyi prev_stop inters ;
            Point.copyi next_start inters ;
            if debug then
              Format.printf "Moved prev_stop and next_start to %a@." pp inters ;
            true
      ) in
    let p =
      fold_leftr (fun p t ->
          let start = get t and stop = get (next t) in
          let start', stop' = f start stop in
          if debug then Format.printf "Mapping %a,%a into %a,%a@."
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

  let translate v = map (fun p -> Point.add p v)

  let scale ?center ratio = map (Point.scale ?center ratio)

  let rotate ?center ang = map (Point.rotate ?center ang)

  module IsInside = Geom.MakeIsInside (Point.K)
  let is_inside t point =
    IsInside.is_inside (iter_edges t) point

  (* Remove identical consecutive points: *)
  let simplify t =
    let same_pos p0 p1 = Point.eq p0 p1 in
    let t', changed =
      fold_leftr (fun (t', changed) t ->
        let p = get t in
        if same_pos p (get (next t)) then t', true
        else (insert_after t' p, changed)
      ) (empty, false) t in
    if changed then t' else t
end
