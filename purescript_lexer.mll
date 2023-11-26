{
	open Lexing
	open Purescript_parser

	exception Lexing_error of string

	let stack = Stack.create ()
	let () = Stack.push (0,MIDLE_BLOCK) stack

	let close n mode =
		if mode then begin
			let result = ref [] in
			while not (Stack.is_empty stack) && (fst (Stack.top stack) > n) do
				result := RIGHT_BLOCK :: !result;
				ignore (Stack.pop stack)
			done ;
			if not (Stack.is_empty stack) && (fst(Stack.top stack) = n) then
				!result @ [MIDLE_BLOCK]
			else !result
		end else []
	
	let close_without_midle n mode =
		let r = close n mode in
		List.filter (fun t -> match t with | MIDLE_BLOCK -> false | _ -> true) r
	
	let unstack_until symb =
(*		Printf.printf "UNSTACK !!!!\n";
		Printf.printf "   : "; Stack.iter (fun (x,y) -> Printf.printf "%d " x) stack; Printf.printf "\n"; *)
		let result = ref [] in
		while not (Stack.is_empty stack) && (snd(Stack.top stack) <> symb) do
			result := RIGHT_BLOCK :: !result;
			ignore (Stack.pop stack)
		done;
		if Stack.is_empty stack then begin
			raise (Lexing_error "erreur d'indentation")
		end else (ignore (Stack.pop stack) ; !result)



	

	let curCol l =  (l.lex_curr_p.pos_cnum - l.lex_curr_p.pos_bol)
	let string_buffer = Buffer.create 1024
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
	| '\n' { new_line lexbuf; next_tokens lexbuf }
	| commentInline | space  { next_tokens lexbuf }
	| "module Main where"  { [MODULE,curCol lexbuf -17] }
	| "import Prelude\nimport Effect\nimport Effect.Console" {new_line lexbuf; new_line lexbuf ;[IMPORT, curCol lexbuf] }
	| eof {[EOF,0] }
	| '=' { [EQUAL, curCol lexbuf -1]}
	| '-' {[MINUS, curCol lexbuf -1]}
	| '+' {[PLUS, curCol lexbuf -1]}
	| '*' {[TIMES, curCol lexbuf -1]}
	| '/' {[DIVIDE, curCol lexbuf -1]}
	| '(' {[LEFT_PAR, curCol lexbuf -1 ]}
	| ')' {[RIGHT_PAR, curCol lexbuf -1]}
	| '|' {[VBAR, curCol lexbuf -1]}
	| "true" {[TRUE, curCol lexbuf -4]}
	| "false" {[FALSE, curCol lexbuf -5]}
	| "if" { [IF, curCol lexbuf -2] }
	| "then" {[THEN, curCol lexbuf -4]}
	| "else" {[ELSE, curCol lexbuf -4]}
	| "do" { [DO, curCol lexbuf -2] }
	| "let" { [LET, curCol lexbuf -3] }
	| "in" { [IN, curCol lexbuf -2] }
	| "case" { [CASE, curCol lexbuf -4] }
	| "of" { [OF, curCol lexbuf -2] }
	| "->" { [ARROW, curCol lexbuf -2] }
	| "data" { [DATA, curCol lexbuf -4] }
	| '"' { let deb = curCol lexbuf in [STRING (string lexbuf), deb-1] }
	| integer as nb { [CONST_INT (int_of_string nb), curCol lexbuf - String.length nb] }
	| lident as lid { [LIDENT lid, curCol lexbuf - String.length lid] }
	| uident as uid { [UIDENT uid, curCol lexbuf - String.length uid] }
	| _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and string = parse
	| '"'
		{ let s = Buffer.contents string_buffer in Buffer.reset string_buffer; s }
	| "\\n"
		{ Buffer.add_char string_buffer '\n'; string lexbuf }
	| "\\\""
		{ Buffer.add_char string_buffer '"'; string lexbuf }
	| "\\" {string_ignore lexbuf}
	| eof | "\n"
		{ raise (Lexing_error "unterminated string") }
	| _ as c
		{ Buffer.add_char string_buffer c; string lexbuf }
and string_ignore = parse
	| "\\" {string lexbuf}
	| _ {string_ignore lexbuf}
	| eof
		{ raise (Lexing_error "unterminated string") }


{

	let next_token_pair =
		let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
		fun lb ->
			if Queue.is_empty tokens then begin
				let l = next_tokens lb in
				List.iter (fun t -> Queue.add t tokens) l
			end;
			Queue.pop tokens

	let next_token =
		let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
		let rec addQueue = function
			| [] -> ()
			| a::b -> Queue.add a tokens ; addQueue b
		in
		fun lb ->
			if Queue.is_empty tokens then begin
				let nxtT = next_token_pair lb in
				let rec add (t,c) mode = begin
					match t with
					| DO ->	addQueue (close c mode) ;
								addQueue [DO;LEFT_BLOCK] ;
								let (t',c') = next_token_pair lb in
								addQueue (close c' mode); 
								Stack.push (c',DO) stack;
								add (t',c') false
					| LET | OF ->	
								let (t',c') = next_token_pair lb in


								if t=OF then
									addQueue (unstack_until CASE);
								addQueue (close c mode) ;
								addQueue (close c' mode);    (* quel mode ? mode ou true *)
								if t=LET then 
									Stack.push (c,LET) stack;
								addQueue [t;LEFT_BLOCK] ;
								
								
								Stack.push (c',t') stack;
								add (t',c') false

					| THEN | ELSE | IN ->
						let attente = match t with | IN -> LET | THEN -> IF | ELSE -> THEN | _ -> failwith "erreur interne au lexer" in
						addQueue (unstack_until attente);
						if t=THEN then
							Stack.push (c,t) stack;
						addQueue [t]
					| IF | CASE -> 
								addQueue (close c mode) ;
								Stack.push (c,t) stack;
								addQueue [t]


					| EOF -> addQueue (close_without_midle c mode) ; Queue.add t tokens
					| _ -> addQueue (close c mode) ; Queue.add t tokens
				end in
				add nxtT true
			end;

			Printf.printf "etat de la queue :\n   ";
			Queue.iter (fun x -> print_string ((match x with
				| MODULE -> "MODULE"
				| IMPORT -> "IMPORT"
				| EOF -> "EOF"
				| LIDENT _ -> "LIDENT"
				| EQUAL -> "="
				| CONST_INT _ -> "const int"
				| IF -> "IF"
				| THEN -> "THEN"
				| ELSE -> "ELSE"
				| TRUE | FALSE -> "BOOLEEN"
				| LEFT_BLOCK -> "__{__"
				| RIGHT_BLOCK -> "__}__"
				| MIDLE_BLOCK -> "__;__"
				| DO -> "DO"
				| STRING _ -> "string"
				| IN -> "in"
				| LET -> "let"
				| CASE -> "CASE"
				| ARROW -> "_->__"
				| OF -> "OF"
				| PLUS -> "PLUS"
				| _ -> "??????"
			)^" ; ")) tokens; Printf.printf "\n"; 
			Printf.printf "etat de la pile : "; Stack.iter (fun (x,y) -> Printf.printf "%d " x) stack; Printf.printf "\n"; 
			Queue.pop tokens
}

