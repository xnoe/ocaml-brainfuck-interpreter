#load "unix.cma"
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
	| OAdd of int
	| OMove of int
	| OBlock of op
	| OPList of op list
	| OInput
	| OOutput

let parse tokens =
	let index = ref 0 in
	let cur () = List.nth tokens !index in

	let rec buildast (): op =
		let ast = ref [] in
		while !index <> (List.length tokens-1) && cur () <> ']' do
			let adda t = ast := !ast @ [t]; incr index in
			match cur () with
				'+' -> adda (OAdd 1)
				|'-' -> adda (OAdd (-1))
				|'<' -> adda (OMove (-1))
				|'>' -> adda (OMove 1)
				|',' -> adda OInput
				|'.' -> adda OOutput
				|'[' -> incr index; adda ( OBlock ( buildast () ) );
				|_->incr index
		done;
		OPList (!ast)
	in
	buildast ()

let eval ast =
	let tape = Array.make 30000 0 in
	let ptr = ref 0 in

	let rec eval_i ast =
		match ast with
			OPList l -> List.iter eval_i l
			|OBlock b -> while tape.(!ptr) <> 0 do eval_i b done
			|OMove n -> ptr := !ptr + n
			|OAdd n -> (
				tape.(!ptr)<-(tape.(!ptr)+n);
				if tape.(!ptr) < 0 then tape.(!ptr)<-tape.(!ptr)+256;
				if tape.(!ptr) > 255 then tape.(!ptr)<-tape.(!ptr)-256;
			)
			|OInput -> tape.(!ptr)<-Char.code (getchar ())
			|OOutput -> (print_string (String.make 1 (Char.chr tape.(!ptr)));Format.print_flush ())
	in eval_i ast

let () =
	Sys.argv.(1) |> readfile |> explode |> parse |> eval
