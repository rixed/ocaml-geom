open Typeset_intf
module G = View.Glop

module Point = Geom_shapes.Point (G.V)
module Ring = Cnt_impl.GenRing (struct type t = Point.t end)
module Poly = Geom_shapes.Polygon (Point) (Ring)

module TestBloc =
struct
	module K = G.K
	type t = unit
	let make () = ()
	let size _bloc _dim = K.one
end

module TS = Typeset_impl.Typeset_raw (Poly) (TestBloc)

(* ouf ! *)

let check_line () =
	let bloc = TestBloc.make () in
	(* Check an empty bloc list gives an empty offset list *)
	assert (TS.typeset_line 0 G.K.one [] = []) ;
	(* Check that a single bloc gives a single offset of zero *)
	assert (TS.typeset_line ~justification:Justified 0 G.K.one [ TS.Stuck, bloc ] = [ G.V.zero ]) ;
	assert (TS.typeset_line ~justification:Justified 0 G.K.one [ TS.Space (G.K.zero, G.K.one), bloc ] = [ G.V.zero ]) ;
	(* Check that two blocs that fit the line gives two offsets and are justified *)
	let offsets = TS.typeset_line ~justification:Justified 0 (G.K.of_int 3) [
		TS.Stuck, bloc ; TS.Space (G.K.zero, G.K.one), bloc
	] in
	List.iter (fun offset -> Format.printf "offset = %a@?" G.V.print offset) offsets ;
	Format.print_newline () ;
	assert (offsets = [ G.V.zero ; G.V.double (G.V.make_unit 0) ])

let () =
	check_line ()

