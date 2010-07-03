(* Some FIELDs to play with. *)

module FloatField : Geom.FIELD with type t = float =
struct
	type t = float
	let zero = 0.
	let one = 1.
	let compare = compare	(* From Pervasive *)
	let min = min
	let max = max
	let add = (+.)
	let sub = (-.)
	let mul = ( *.)
	let div = (/.)
	let neg x = -.x
	let inv x = 1./.x
	let of_float f = f
	let to_float v = v
	let to_string = string_of_float
	let of_int = float_of_int
	let half s = s *. 0.5
	let double s = s *. 2.
	let square s = s *. s
	let sqrt = sqrt
	let abs = abs_float
	let print ff s = Format.pp_print_float ff s
end

(* Fixed size precision *)
module IntField (Prec : sig val v : int end)
	: Geom.FIELD with type t = int (* FIXME: then the printer will be used for all ints :-< *)=
struct
	type t = int
	let zero = 0
	let one = 1 lsl Prec.v
	let compare = compare
	let min = min
	let max = max
	let add = (+)
	let sub = (-)
	let mul a b =
		let m = (Int64.shift_right_logical (Int64.mul (Int64.of_int a) (Int64.of_int b)) Prec.v) in
		Int64.to_int m
	let div a b =
		let m = Int64.div (Int64.shift_left (Int64.of_int a) Prec.v) (Int64.of_int b) in
		Int64.to_int m
	let neg s = -s
	let inv s = div one s
	let of_float f = int_of_float (f *. (float_of_int one))
	let to_float s = (float_of_int s) /. (float_of_int one)
	let to_string = string_of_int
	let of_int i = i lsl Prec.v
	let half s = s/2
	let double s = s lsl 1
	let square s = mul s s
	let sqrt s = of_float (sqrt (to_float s))
	let abs = abs
	let print ff s = Format.fprintf ff "%d.%d" (s asr Prec.v) (s land (one-1))
end

(* Some VECTORs *)

module Vector2D (K_ : Geom.FIELD)
	: Geom.VECTOR with module K = K_ =
struct
	module K = K_
	type scalar = K.t
	type t = (scalar * scalar)

	let zero = K.zero, K.zero
	let make_unit = function
		| 0 -> K.one, K.zero
		| 1 -> K.zero, K.one
		| _ -> zero
	let nth v = function 0 -> fst v | 1 -> snd v | _ -> failwith "No such dimension"
	let of_2scalars v = v
	let of_3scalars (x, y, _) = x, y
	let to_3scalars (x, y) = x, y, K.zero
	let add (x1, y1) (x2, y2) = K.add x1 x2, K.add y1 y2
	let sub (x1, y1) (x2, y2) = K.sub x1 x2, K.sub y1 y2
	let mul (x, y) s = K.mul s x, K.mul s y
	let scalar_product (x1, y1) (x2, y2) = K.add (K.mul x1 x2) (K.mul y1 y2)
	let norm2 v = scalar_product v v
	let norm v = K.sqrt (norm2 v)
	let normalize v =
		let n = norm v in
		try mul v (K.inv n) with _ -> v
	let oposite (x, y) = (K.neg x, K.neg y)
	let to_point3 (x, y) = (K.to_float x, K.to_float y, 0.)
	let area (x1, y1) (x2, y2) = K.sub (K.mul x1 y2) (K.mul x2 y1)
	let half (x, y) = K.half x, K.half y
	let right_turn (x, y) = (K.neg y, x)
	let eq (x1, y1) (x2, y2) = K.compare x1 x2 = 0 && K.compare y1 y2 = 0
	
	let to_upper, to_lower =
		let select choose vecs =
			let rec aux ((best_x, best_y) as best) = function
				| [] -> best
				| (x, y) :: others ->
					aux (choose best_x x, choose best_y y) others in
			aux (List.hd vecs) (List.tl vecs) in
		select K.max, select K.min

	(* BBOX *)
	
	type bbox = (t * t) option
	
	let empty_bbox = None
	
	let make_bbox v = Some (v, v)
	
	let bbox_union b1 b2 = match b1 with
		| None -> b2
		| Some (min1, max1) -> (match b2 with
			| None -> b1
			| Some (min2, max2) ->
				Some (to_lower [ min1 ; min2 ], to_upper [ max1 ; max2 ]))
	
	let bbox_add_vec bbox vec = bbox_union bbox (make_bbox vec)

	let bbox_translate bbox vec = match bbox with
		| None -> None
		| Some (min, max) -> Some (add min vec, add max vec)
	
	(* Formater *)

	let print ff (x, y) =
		Format.pp_open_box ff 0 ;
		Format.pp_print_string ff "(" ;
		K.print ff x ;
		Format.pp_print_string ff ", " ;
		K.print ff y ;
		Format.pp_print_string ff ")" ;
		Format.pp_close_box ff ()
end

