open Text_intf
open Freetype

let debug = false

let font_files =
  let well_known = [
    "/usr/share/fonts/truetype/Isabella.ttf" ;
    "/usr/share/fonts/corefonts/arial.ttf" ;
    "/usr/share/fonts/ttf-bitstream-vera/Vera.ttf" ;
    "/opt/X11/share/fonts/TTF/Vera.ttf" ;
    "/usr/share/fonts/alee-fonts/Bandal.ttf" ;
    "/usr/share/fonts/truetype/freefont/FreeSans.ttf" ;
    "/usr/share/fonts/tahoma.ttf" ;
    "/usr/local/lib/X11/fonts/bitstream-vera/Vera.ttf" ] in
  List.filter (fun f ->
    try let _ = Unix.stat f in true
    with Unix.(Unix_error (ENOENT, _, _)) -> false
  ) well_known |>
  ref

let get_face =
  let face = ref None in
  fun () ->
    match !face with
    | Some f -> f
    | None ->
        let lib = Freetype.init () in
        let rec init_first = function
          | [] -> failwith "No working font file !"
          | file :: others ->
            try
              let ret = new_face lib file 0 in
              Format.printf "Using true type font file %s@." file ;
              face := Some ret
            with Failure _ -> (
              Format.printf "Skip font file %s@." file ;
              init_first others ) in
        init_first !font_files ;
        match !face with
        | Some f -> f
        | None ->
          Printf.eprintf "Cannot find any font file.\n" ;
          exit 1

module Glyph
  (Poly : Geom.POLYGON)
  (Path : Geom.PATH with module Point = Poly.Point)
  : GLYPH with module Poly = Poly and module Path = Path =
struct
  module Poly = Poly
  module Path = Path
  module Point = Poly.Point
  module Algo = Geom_algo.Algorithms (Poly) (Path)

  type t = {
    index : char_index ;
    paths : Path.t list ;
    advance_x : float ;
    advance_y : float ;
    metrics : glyph_metrics }

  let make unicode =
    let face, _face_info = get_face () in
    let index = get_char_index face unicode in
    let advance_x, advance_y = load_glyph face index [ Load_no_scale ; Load_no_hinting ] in
    let metrics = get_glyph_metrics face in
    let outline = get_outline_contents face in
    let to_point (x, y) =
      let xs = Poly.Point.K.of_float x in
      let ys = Poly.Point.K.of_float y in
      [| xs ; ys |] in
    let rec next_point first last n =
      if n <= last then n else next_point first last (first + n - last - 1) in
    let rec path_of_contour next first last path =
      if next > last then path else (
        match outline.tags.(next) with
        | On_point ->
          path_of_contour (next+1) first last
            (Path.extend (to_point outline.points.(next))
              [] Path.make_straight_line path)
        | Off_point_conic ->
          let next_s = next_point first last (next+1) in
          let middle_point (x1,y1) (x2,y2) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.) in
          let next_current, next_next = (match outline.tags.(next_s) with
            | On_point -> (* normal case : on,  off, on *)
              (outline.points.(next_s), next+1)
            | Off_point_conic -> (* special case : on, off, off, on *)
              (middle_point outline.points.(next) outline.points.(next_s), next+1)
            | Off_point_cubic -> (* bug *)
              failwith "Bad font encoding") in
          path_of_contour next_next first last
            (Path.extend (to_point next_current)
              [ to_point outline.points.(next) ]
              Path.make_bezier_curve path)
        | Off_point_cubic ->
          let next_s  = next_point first last (next+1)
          and next_ss = next_point first last (next+2) in
          path_of_contour (next+3) first last
            (Path.extend (to_point outline.points.(next_ss))
              [ to_point outline.points.(next) ;
                to_point outline.points.(next_s) ]
              Path.make_bezier_curve path)
      ) in
    let get_path c =
      let first = if c = 0 then 0 else outline.contours.(c-1)+1 in
      let last = outline.contours.(c) in
      path_of_contour (first+1) first last (Path.empty (to_point outline.points.(first))) in
    let is_cclockwise paths =
      (*0 <> (outline.flags land 4) (* Too bad we can't trust this flag *) *)
      let area = Algo.area_paths_min paths in
      Point.K.compare area Point.K.zero < 0 in
    let get_all_paths () =
      let paths = ref [] in
      for c = 0 to outline.n_contours-1 do paths := get_path c :: !paths done ;
      if is_cclockwise !paths then Algo.reverse_paths !paths else !paths in
    { index = index ;
      paths = get_all_paths () ;
      advance_x = advance_x ;
      advance_y = advance_y ;
      metrics }

  (* We keep a cache of all generated glyphs : *)
  type used_glyph =
    { mutable nb_use : int ;
      mutable idx    : int ;
              polys  : Poly.t list }

  let max_kept_glyphs = 200 (* we keep only this amount of glyphs per cache key *)

  (* Array is ordered in descending nb_use count *)
  let used_glyphs = Array.init max_kept_glyphs (fun i -> { nb_use = 0 ; idx = i ; polys = [] })

  let glyph_cache = Hashtbl.create max_kept_glyphs (* hash from char_index, prec to used_glyph *)

  let promote_in_cache idx =
    let nb_use = used_glyphs.(idx).nb_use in
    let rec promote_once idx =
      if idx > 0 && nb_use > used_glyphs.(idx-1).nb_use then (
        (* swap entry idx-1 with entry idx *)
        let old = used_glyphs.(idx-1) in
        used_glyphs.(idx-1) <- used_glyphs.(idx) ;
        used_glyphs.(idx) <- old ;
        used_glyphs.(idx-1).idx <- idx-1 ;
        used_glyphs.(idx).idx <- idx ;
        promote_once (idx-1)
      ) in
    promote_once idx

  let get_cached key =
    let used = Hashtbl.find glyph_cache key in
    used.nb_use <- used.nb_use + 1 ;
    promote_in_cache used.idx ;
    used.polys

  let add_cache key polys =
    (* Replace the last element (TODO: keep the idx of the first unused element)
     * and then bubble-it up: *)
    let idx = max_kept_glyphs-1 in
    if used_glyphs.(idx).nb_use > 0 then
      Hashtbl.remove glyph_cache key ;
    let entry = { nb_use = 1 ; idx = idx ; polys = polys } in
    used_glyphs.(idx) <- entry ;
    promote_in_cache idx ;
    Hashtbl.add glyph_cache key entry ;
    if debug then Format.printf "%d entries in the glyph cache@." (Hashtbl.length glyph_cache)

  let to_paths glyph = glyph.paths

  let to_polys ~res glyph =
    let key = glyph.index, res in
    try get_cached key
    with Not_found ->
      let polys = Algo.polys_of_paths ~res glyph.paths in
      add_cache key polys ;
      polys

  let bbox glyph =
    let rec extend_bbox current = function
      | [] -> current
      | path :: other ->
        extend_bbox (Point.Bbox.union current (Path.bbox path)) other in
    extend_bbox Point.Bbox.empty glyph.paths

  (* Use the glyph metrics *)
  let fast_bbox ?(orientation=Horizontal) glyph =
    let bearing =
      match orientation with
      | Horizontal -> glyph.metrics.gm_hori
      | Vertical   -> glyph.metrics.gm_vert in (* TODO: check that *)
    let f = Point.K.of_float in
    let mi = [| f bearing.bearingx ;
                f (bearing.bearingy -. glyph.metrics.gm_height) |]
    and ma = [| f (bearing.bearingx +. glyph.metrics.gm_width) ;
                f bearing.bearingy |] in
    Point.Bbox.(add (make mi) ma)

  let advance ?(orientation=Horizontal) prev_glyph next_glyph =
    match orientation with
    | Horizontal ->
      let face, _face_info = get_face () in
      let kern_vec = get_kerning face prev_glyph.index next_glyph.index Kerning_unscaled in
      [| Point.K.of_float (prev_glyph.advance_x +. kern_vec.ft_x) ;
         Point.K.of_float kern_vec.ft_y |]
    | Vertical ->
      [| Point.K.zero ;
         Point.K.of_float prev_glyph.advance_y |]
end

module Word
  (Glyph : GLYPH)
  : WORD with module Glyph = Glyph =
struct
  module Glyph = Glyph
  module Poly = Glyph.Poly
  module Point = Poly.Point

  (* First position will be Point.zero *)
  type t = (Point.t * Glyph.t) list

  let make ?orientation str =
    (* FIXME: BatUtf8.fold *)
    let i = ref 0 in
    let unicodes = Array.make (BatUTF8.length str) 0 in
    BatUTF8.iter (fun c ->
      unicodes.(!i) <- BatUChar.code c ;
      incr i
    ) str ;
    let rec add_char i word pos =
      if i >= Array.length unicodes then
        word
      else
        let c = unicodes.(i) in
        (* TODO: use also previous char to choose a better glyph for 2 successive chars *)
        let glyph = Glyph.make c in
        let offset = match word with
        | [] -> pos
        | (_, prev_g) :: _ ->
          let advance = Glyph.advance ?orientation prev_g glyph in
          Point.add pos advance in
        let next_word = (offset, glyph) :: word in
        add_char (i+1) next_word offset in
    add_char 0 [] Point.zero

  (* Use the glyph metrics *)
  let fast_bbox ?orientation word =
    List.fold_left (fun b (p, g) ->
      let g_bbox = Glyph.fast_bbox ?orientation g in
      let g_bbox = Point.Bbox.translate g_bbox p in
      Point.Bbox.union b g_bbox
    ) Point.Bbox.empty word

  (* Use actual geometry *)
  let bbox word =
    (* A glyph has no position since it's only the "pure", abstract representation of a symbol.
       But glyphs in words are positioned. So we must compute the bbox as the union of all
       glyph's bboxes translated to match glyph position in word. *)
    let rec aux bbox = function
      | [] -> bbox
      | (pos, glyph) :: others ->
        let g_bbox = Point.Bbox.translate (Glyph.bbox glyph) pos in
        let new_bbox = Point.Bbox.union bbox g_bbox in
        aux new_bbox others in
    aux Glyph.Poly.Point.Bbox.empty word

  let to_polys ~res word =
    List.map (fun (p, g) -> p, Glyph.to_polys ~res g) word

  let to_paths word =
    List.map (fun (p, g) -> p, Glyph.to_paths g) word

  let lower_left_to_origin ?orientation word =
    let bbox = fast_bbox ?orientation word in
    match bbox with
    | Point.Bbox.Empty -> Point.zero
    | Point.Bbox.Box (mi, _ma) ->
      (* TODO: What to do if not Horizontal? *)
      Point.opposite mi
end
