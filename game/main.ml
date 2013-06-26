let () =
	(* Init random number generator without seed *)
	Random.self_init () ;
	Arg.parse
		[ "-seed", Arg.Int Random.init, "Use this seed to init random generator" ]
		(fun p -> failwith ("Unknown parameter '"^p))
		"MLRocket v0, a space exploration program\nOptions :" ;
	let world = World.make ~radius:150 in
	Game.play world
