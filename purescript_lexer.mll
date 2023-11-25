{
	open Lexing
	open Purescript_parser

	(* exception à lever pour signaler une erreur lexicale *)
	exception Lexing_error of string

	(* note : penser à appeler la fonction Lexing.new_line
		 à chaque retour chariot (caractère '\n') *)
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let other = lower | upper | digit | '\''
let lident = lower (other *)
let uident = upper (other | '.')*
let integer = ['1'-'9'] digit*

let commentInline = "--" [^'\n']*
let space = ' ' | '\t'



rule next_tokens = parse
	| "\n"    { Printf.printf "newLine\n"; new_line lexbuf; [NEWLINE] }
	| commentInline | space  { next_tokens lexbuf }
	| "module Main where"  { Printf.printf "read module\n"; [MODULE] }
	| "import Prelude\nimport Effect\nimport Effect.Console" {Printf.printf "read import\n"; [IMPORT] }
	| eof {	Printf.printf "fin\n"; [EOF] }
	| '=' {[EQUAL]}
	| '-' {[MINUS]}
	| '+' {[PLUS]}
	| '*' {[TIMES]}
	| '/' {[DIVIDE]}
	| integer as nb { [CONSTANT (Purescript_ast.Cint (int_of_string nb))] }
	| lident as lid { Printf.printf "read lindent : %s" lid; [LIDENT lid] }
	| _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }




{

	let next_token =
		let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
			fun lb ->
				if Queue.is_empty tokens then begin
					let l = next_tokens lb in
					List.iter (fun t -> Queue.add t tokens) l
				end;
		(*Queue.iter (fun x -> match x with
			| MODULE -> Printf.printf "MODULE ; "
			| IMPORT -> Printf.printf "IMPORT ; "
			| EOF -> Printf.printf "EOF ; "
			| NEWLINE -> Printf.printf "NEWLINE ; "
			| LIDENT l -> Printf.printf "LIDENT %s ; " l
			| EQUAL -> Printf.printf "= ; "
			| CONSTANT _ -> Printf.printf "constante ; "
		) tokens; Printf.printf "\n"; *)
		Queue.pop tokens
}

