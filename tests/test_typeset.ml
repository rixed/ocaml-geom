open Typeset_intf

module K = Geom.CheckedField (Geom_algebr.FloatField)
module Vec = Geom_algebr.Vector2D (K)
module Point = Geom_shapes.Point (Vec)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)

module TestBloc =
struct
	module K = K
	type t = unit
	let make () = ()
	let size _bloc _dim = K.one
end

module TS = Typeset_impl.Typeset_raw (Poly) (TestBloc)

(* ouf ! *)

let check_line () =
	let bloc = TestBloc.make () in
	(* Check an empty bloc list gives an empty offset list *)
	assert (TS.typeset_line 0 K.one [] = []) ;
	(* Check that a single bloc gives a single offset of zero *)
	assert (TS.typeset_line ~justification:Justified 0 K.one [ TS.Stuck, bloc ] = [ Vec.zero ]) ;
	assert (TS.typeset_line ~justification:Justified 0 K.one [ TS.Space (K.zero, K.one), bloc ] = [ Vec.zero ]) ;
	(* Check that two blocs that fit the line gives two offsets and are justified *)
	let offsets = TS.typeset_line ~justification:Justified 0 (K.of_int 3) [
		TS.Stuck, bloc ; TS.Space (K.zero, K.one), bloc
	] in
	List.iter (fun offset -> Format.printf "offset = %a@?" Vec.print offset) offsets ;
	Format.print_newline () ;
	assert (offsets = [ Vec.zero ; Vec.mul (Vec.make_unit 0) 2. ])

let () =
	check_line ()

