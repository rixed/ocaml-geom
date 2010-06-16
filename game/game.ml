open Mlrocket

let game_clic _world (x, y) =
	Printf.printf "Clic at %f, %f\n%!" x y

let game_painter _world () =
	Printf.printf "Paint!\n%!"

let play world =
	View.display ~onclic:(game_clic world) [game_painter world]
