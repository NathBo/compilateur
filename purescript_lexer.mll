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


rule next_token = parse
	| "module Main where"  { [MODULE] }
	| "import Prelude" { [IMPORT] }
	| _ { [] }


{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_token lb in
	List.iter (fun t -> Printf.printf "lance\n" ; Queue.add t tokens) l
      end;
      Queue.pop tokens
}

