(* Some FIELDs to play with. *)

module FloatField : Geom.FIELD with type t = float =
struct
	type t = float
	let zero = 0.
	let one = 1.
	let compare = compare	(* From Pervasive *)
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
	let sqrt s = s (*sqrt*)
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
	let add = (+)
	let sub = (-)
	let mul a b = (a * b) asr Prec.v
	let div a b = (a lsl Prec.v) / b
	let neg s = -s
	let inv s = div 1 s
	let of_float f = int_of_float (f *. (float_of_int one))
	let to_float s = (float_of_int s) /. (float_of_int one)
	let to_string = string_of_int
	let of_int = (lsl) Prec.v
	let half s = s/2
	let double s = s lsl 1
	let square s = mul s s
	let sqrt s = of_float (sqrt (to_float s))
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
	let of_2scalars v = v
	let of_3scalars (x, y, _) = x, y
	let to_3scalars (x, y) = x, y, K.zero
	let add (x1, y1) (x2, y2) = K.add x1 x2, K.add y1 y2
	let sub (x1, y1) (x2, y2) = K.sub x1 x2, K.sub y1 y2
	let mul (x, y) s = K.mul s x, K.mul s y
	let scalar_product (x1, y1) (x2, y2) = K.add (K.mul x1 x2) (K.mul y1 y2)
	let norm2 v = scalar_product v v
	let to_point3 (x, y) = (K.to_float x, K.to_float y, 0.)
	let area (x1, y1) (x2, y2) = K.sub (K.mul x1 y2) (K.mul x2 y1)
	let half (x, y) = K.half x, K.half y
	let right_turn (x, y) = (K.neg y, x)
	let eq (x1, y1) (x2, y2) = K.compare x1 x2 = 0 && K.compare y1 y2 = 0

	let print ff (x, y) =
		Format.pp_open_box ff 0 ;
		Format.pp_print_string ff "(" ;
		K.print ff x ;
		Format.pp_print_string ff ", " ;
		K.print ff y ;
		Format.pp_print_string ff ")" ;
		Format.pp_close_box ff ()
end

