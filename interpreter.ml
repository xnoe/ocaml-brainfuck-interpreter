open Printf

let getchar () =
	let termio = Unix.tcgetattr Unix.stdin in
	 Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_icanon = false };
	let res = input_char stdin in
	Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
	res

let readfile file =
  let ic = open_in file in
  try
		let n = in_channel_length ic in
		let s = Bytes.create n in
		really_input ic s 0 n;
		close_in ic;
		Bytes.to_string s
  with e ->
		close_in_noerr ic;
		raise e

let explode s = List.init (String.length s) (String.get s)

type op =
	| ODec
	| OInc
	| OLeft
	| ORight
	| OBlock of op list
	| OPList of op list
	| OInput
	| OOutput

let parse tokens =
	let rec buildast tokens ast =
		match tokens with
			[] -> (ast,[])
			|']'::tl -> (ast,tl)
			|'+'::tl -> buildast tl (ast@[OInc])
			|'-'::tl -> buildast tl (ast@[ODec])
			|'<'::tl -> buildast tl (ast@[OLeft])
			|'>'::tl -> buildast tl (ast@[ORight])
			|','::tl -> buildast tl (ast@[OInput])
			|'.'::tl -> buildast tl (ast@[OOutput])
			|'['::tl -> let join,ntl = buildast tl [] in buildast ntl (ast@[OBlock join])
			|_::tl -> buildast tl ast
	in let (ast,_) = buildast tokens [] in OPList ast

let eval ast =
	let tape = Array.make 30000 0 in
	let ptr = ref 0 in

	let rec eval_i ast =
		match ast with
			OPList l -> List.iter eval_i l
			|OBlock b -> while tape.(!ptr) <> 0 do List.iter eval_i b done
			|OLeft -> decr ptr
			|ORight -> incr ptr
			|OInc -> tape.(!ptr)<-tape.(!ptr)+1;
			|ODec -> (tape.(!ptr)<-tape.(!ptr)-1; if tape.(!ptr)<0 then tape.(!ptr)<-255)
			|OInput -> tape.(!ptr)<-Char.code (getchar ())
			|OOutput -> (tape.(!ptr) |> char_of_int |> String.make 1 |> print_string; Format.print_flush ())
	in eval_i ast

let _ =
	Sys.argv.(1) |> readfile |> explode |> parse |> eval
