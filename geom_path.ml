module Make (Point : Geom.POINT)
  : Geom.PATH with module Point = Point =
struct
  module Point = Point
  open Point.Infix
  type point = Point.t
  type interpolator = point -> point -> point list -> Point.K.t -> point list

  type t = {
    start : point ;
    edges : (point * point list * interpolator) list
  }

  let print ff path =
    Format.pp_open_box ff 0 ;
    Format.pp_print_string ff "<" ; Format.pp_print_space ff () ;
    let rec aux start edges =
      Point.print ff start ;
      (match edges with
      | [] -> ()
      | (e, _, _)::edges' -> aux e edges') in
    aux path.start path.edges ;
    Format.pp_print_string ff ">" ;
    Format.pp_close_box ff ()

  (*
   * Path manipulation
   *)

  let start start = { start ; edges = [] }
  let empty = start

  let is_empty path = path.edges = []

  let is_closed path =
    let rec last_point = function
      | [] -> path.start
      | [ p, _, _ ] -> p
      | _ :: e' -> last_point e' in
    let last = last_point path.edges and first = path.start in
    Point.eq first last

  let extend next ctrls interp path =
    (match path.edges with
    | (pt, [], _) :: _ -> if 0 = Point.compare pt next then
      Format.printf "Path edge of no length at point %a@\n" Point.print pt
    | _ -> ()) ;
    { path with edges = path.edges @ [next, ctrls, interp] }

  let concat path1 path2 =
    { start = path1.start ; edges = path1.edges @ path2.edges }

  let length path = List.length path.edges

  let map f path =
    { start = f path.start ;
      edges = List.map (fun (p, ctrls, i) ->
        f p, List.map f ctrls, i) path.edges }

  let translate disp = map ((+~) disp)

  let reverse path =
    let reverse_edge start (target, ctrls, interp) =
      target, (start, List.rev ctrls, interp) in
    let rec loop rev_edges tail = function
    | [] -> { start = tail ; edges = rev_edges }
    | edge :: edges ->
      let tail, rev_edge = reverse_edge tail edge in
      loop (rev_edge :: rev_edges) tail edges in
    loop [] path.start path.edges

  let center path =
    (* FIXME: we should add the center of each edge instead of adding the starting point and every edge's last *)
    let add_pos p (n, _, _) = p +~ n in
    Point.mul (Point.K.inv (Point.K.of_int (length path))) (List.fold_left add_pos path.start path.edges)

  let scale ?center scale path =
    let scale_me = Point.scale ?center scale in
    map scale_me path

  let scale_along ?(center=Point.origin) ~axis ratio path =
    let scale_me p =
      (* decompose vector from center to p as one vector along axis and one perpendicular component. *)
      let t = Point.sub p center in
      let t_axis = Point.mul (Point.scalar_product axis t) axis in
      let t_perp = Point.sub t t_axis in
      (* now rescale axis component *)
      let new_t_axis = Point.mul ratio t_axis in
      (* and rebuild new p from these *)
      Point.add center (Point.add new_t_axis t_perp) in
    map scale_me path

  let clip p0 p1 path =
    let is_left p = Point.compare_left p0 p1 p >= 0 in
    let rec aux res start start_is_left = function
      | [] -> res
      | (stop, ctrls, interp) :: edges ->
        let stop_is_left = is_left stop in
        let res =
          if start_is_left then
            (* add this point to previous path *)
            match res with
            | [] -> [ empty start |> extend stop ctrls interp ]
            | p::res' -> extend stop ctrls interp p :: res'
          else if stop_is_left || List.exists is_left ctrls then
            (* start a new path *)
            (empty start |> extend stop ctrls interp) :: res
          else res in
        aux res stop stop_is_left edges in
    let start_is_left = is_left path.start in
    let res = if start_is_left then [ empty path.start ] else [] in
    aux res path.start start_is_left path.edges

  let rotate ?center ang = map (Point.rotate ?center ang)

  (*
   * Interpolators
   *)

  (* Whatever the resolution, a straight line need no intermediary points *)
  let make_straight_line _start _stop _control _res = []

  let debug = false

  let format_array p fmt a =
    Format.fprintf fmt "@[[" ;
    for i = 0 to Array.length a - 1 do
      Format.fprintf fmt "%a%," p a.(i)
    done ;
    Format.fprintf fmt "]@]"

  (* TODO: faster impl for 3 and 4 points *)
  let rec bezier ?(depth=0) ctrls res =
    if debug then Format.printf "bezier loop depth=%d, pts=%a@." depth (format_array Point.print) ctrls ;
    let len = Array.length ctrls in
    (* Is this a flat line? Comparing the mid point with the middle controller
     * is not reliable since they could be very close while still the curve
     * very far away at other control points. So we loop over all control
     * points, compute their distance with the straight line, and only if their
     * are all close enough do we consider this a straight line. *)
    try
      let v = ctrls.(len-1) -~ ctrls.(0) in
      let len_1 = Point.K.inv (Point.K.of_int (len-1)) in
      for i = 1 to len - 2 do
        let proj =
          Point.(ctrls.(0) +~ mul len_1 (mul (Point.K.of_int i) v)) in
        let d = Point.(norm2 (proj -~ ctrls.(i))) in
        if debug then Format.printf "... mid_point=%a, ctrls.(i)=%a@." Point.print proj Point.print ctrls.(i) ;
        if d > Point.K.square res then raise Exit
      done ;
      (* So this is flat enough: *)
      [ ctrls.(len-1) ]
    with Exit ->
      (* Not flat, let's interpolate further: *)
      let ctrls_l = Array.make len Point.zero
      and ctrls_r = Array.make len Point.zero in
      let rec split n ctrls =
        let len' = Array.length ctrls in
        ctrls_l.(n) <- ctrls.(0) ;
        ctrls_r.((len - 1) - n) <- ctrls.(len' - 1) ;
        if len' > 1 then (
          let ctrls' = Array.init (len' - 1) (fun i ->
            Point.(half ctrls.(i) +~ half ctrls.(i+1))) in
          split (n + 1) ctrls'
        )
      in
      split 0 ctrls ;
      (bezier ~depth:(depth + 1) ctrls_l res) @ (bezier ~depth:(depth + 1) ctrls_r res)

  let make_bezier_curve start stop ctrls res =
    let len = List.length ctrls in
    let ctrls_arr = Array.init (len+2) (fun i ->
      if i = 0 then start
      else if i = len+1 then stop
      else List.nth ctrls (i-1)) in
    bezier ctrls_arr res

  let straight_to point path =
    extend point [] make_straight_line path

  let bezier_to point ctrls path =
    extend point ctrls make_bezier_curve path

  let iter res path f =
    let rec aux start edges =
      f start ;
      match edges with
      | [] -> ()
      | (stop, ctrls, interp) :: rest ->
        List.iter f (interp start stop ctrls res) ;
        aux stop rest
    in
    aux path.start path.edges

  let iter_edges path f =
    let rec aux prec = function
      | [] -> ()
      | (stop, _, _) :: e' -> f prec stop ; aux stop e'
    in
    if not (is_empty path) then aux path.start path.edges

  let map_pts f path =
    let edges =
      List.map (fun (stop, ctrls, interp) ->
        let stop', ctrls' = f stop ctrls in
        stop', ctrls', interp) path.edges in
    { path with edges }

  let area_min path =
    let s = ref Point.K.zero in
    let add_edge a b = s := Point.K.add !s (Point.area a b) in
    iter_edges path add_edge ;
    Point.K.half !s

  module IsInside = Geom.MakeIsInside (Point.K)
  let is_inside res path point =
    let rec aux start edges f =
      match edges with
      | [] ->
        if start != path.start then f start path.start
      | (stop, [], _interp) :: edges' ->
        f start stop ;
        aux stop edges' f
      | (stop, ctrls, interp) :: edges' ->
        let bbox = Point.Bbox.make start in
        let bbox = Point.Bbox.add bbox stop in
        let bbox = List.fold_left Point.Bbox.add bbox ctrls in
        (match bbox with
        | Point.Bbox.Box ([| _xmi;ymi |], [| xma;yma |]) ->
          if Point.K.compare point.(0) xma <= 0 &&
             Point.K.compare point.(1) yma <= 0 &&
             Point.K.compare point.(1) ymi >= 0
          then ( (* iter on this edge *)
            let last =
              List.fold_left (fun prev next ->
                  f prev next ;
                  next
                ) start (interp start stop ctrls res) in
              f last stop
              )
        | _ -> assert false) ;
        aux stop edges' f in
    IsInside.is_inside (aux path.start path.edges) point

  let bbox path =
    let union_ctls bbox ctl = Point.Bbox.add bbox ctl in
    let union_edges bbox (dest, ctls, _) =
      List.fold_left union_ctls (Point.Bbox.add bbox dest) ctls in
    List.fold_left union_edges (Point.Bbox.make path.start) path.edges

  let rounded ?(radius=Point.K.one) paths =
    let first_stop path = match List.hd path.edges with stop, _, _ -> stop in
    let shorten a b d =
      let v = Point.sub b a in
      let r = Point.K.div d (Point.norm v) in
      let ratio = Point.K.sub Point.K.one r in
      Point.add a (Point.mul ratio v) in
    let rec aux path path' start = function
      | [] -> path'
      | [stop, ctrls, interp] ->
        if Point.eq stop path.start then (
          let d1 = shorten start stop radius
          and d2 = shorten (first_stop path) path.start radius in
          let path' = extend d1 ctrls interp path' in
          let path' = extend d2 [ stop ] make_bezier_curve path' in
          { path' with start = d2 }
        ) else path'
      | (stop1, ctrls1, interp1)::(stop2, ctrls2, interp2)::edges ->
        let d1 = shorten start stop1 radius
        and d2 = shorten stop2 stop1 radius in
        let path' = extend d1 ctrls1 interp1 path' in
        let path' = extend d2 [ stop1 ] make_bezier_curve path' in
        aux path path' d2 ((stop2, ctrls2, interp2)::edges) in
    let rounded_single path = aux path (empty path.start) path.start path.edges in
    List.map rounded_single paths

  let circle ?(center=Point.origin) radius =
    let l = Point.K.one and o = Point.K.zero
    and c = Point.K.of_float 0.55191502449 in
    let _l = Point.K.neg l and _o = Point.K.neg o
    and _c = Point.K.neg c in
    empty [| l;o |] |>
    bezier_to [| o;l |] [ [|l;c|]; [|c;l|] ] |>
    bezier_to [| _l;o |] [ [|_c;l|]; [|_l;c|] ] |>
    bezier_to [| o;_l |] [ [|_l;_c|]; [|_c;_l|] ] |>
    bezier_to [| l;o |] [ [|c;_l|]; [|l;_c|] ] |>
    scale radius |>
    translate center

  let rect min max =
    empty min |>
    straight_to [| max.(0) ; min.(1) |] |>
    straight_to max |>
    straight_to [| min.(0); max.(1) |] |>
    straight_to min

  let of_bbox = function
    | Point.Bbox.Empty -> failwith "of_box"
    | Point.Bbox.Box (min, max) -> rect min max
end

module Draw (Path : Geom.PATH) =
struct
  open Path
  module K = Point.K

  let extend_straight t next =
    extend next [] make_straight_line t

  let ( -- ) = extend_straight

  let rectangle corner00 corner10 =
    let corner01 = [| corner10.(0) ; corner00.(1) |]
    and corner11 = [| corner00.(0) ; corner10.(1) |] in
    (empty corner00) -- corner01 -- corner10 -- corner11 -- corner00

  let rectangle_of_size ?(center=Point.origin) width height =
    let diag = [| K.half width ; K.half height |] in
    let corner00 = Point.sub center diag
    and corner10 = Point.add center diag in
    rectangle corner00 corner10

  let box corner00 corner10 width =
    let thickness = [| width ; width |] in
    let corner00' = Point.add corner00 thickness
    and corner10' = Point.sub corner10 thickness in
    [ rectangle corner00 corner10 ; reverse (rectangle corner00' corner10') ]
end
