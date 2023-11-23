{
	open Lexing
	open Parser

	(* exception à lever pour signaler une erreur lexicale *)
	exception Lexing_error of string

	(* note : penser à appeler la fonction Lexing.new_line
		 à chaque retour chariot (caractère '\n') *)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']


rule scan = parse
	| "Module"  { [T_Module] }
	| _ { assert false (* À COMPLÉTER *) }



