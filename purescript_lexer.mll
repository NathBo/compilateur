{
	open Lexing
	open Purescript_parser

	(* exception à lever pour signaler une erreur lexicale *)
	exception Lexing_error of string

	(* note : penser à appeler la fonction Lexing.new_line
		 à chaque retour chariot (caractère '\n') *)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']


rule next_tokens = parse
	| "\n"    { Printf.printf "newLine\n"; new_line lexbuf; [NEWLINE] }
	| "module Main where"  { Printf.printf "read module\n"; [MODULE] }
	| "import Prelude\nimport Effect\nimport Effect.Console" {Printf.printf "read import\n"; [IMPORT] }
	| eof {	Printf.printf "fin\n"; [EOF] }
	| _ {	Printf.printf "non reconnu\n"; [] }


{

	let next_token =
		let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
			fun lb ->
				if Queue.is_empty tokens then begin
					let l = next_tokens lb in
					List.iter (fun t -> Queue.add t tokens) l
				end;
		Printf.printf "print\n";
		Queue.iter (fun x -> match x with
			| MODULE -> Printf.printf "MODULE ; "
			| IMPORT -> Printf.printf "IMPORT ; "
			| EOF -> Printf.printf "EOF ; "
			| NEWLINE -> Printf.printf "NEWLINE ; "
		) tokens; Printf.printf "\n";
		Queue.pop tokens
}

