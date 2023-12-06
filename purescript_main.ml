open Format
open Lexing

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Nom du fichier source *)
let ifile = ref ""

let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
	["--parse-only", Arg.Set parse_only,
	 "Pour ne faire que l'analyse syntaxique";

	 "--type-only", Arg.Set type_only,
	  "Pour ne faire que l'analyse syntaxique puis le typage"]

let usage = "compilateur de purscript"
(* définition des couleurs pour l'affichage *)
let colorRed = "\o033[31m"
let colorGreen = "\o033[32m"
let colorDefault = "\o033[0m"
(* localise une erreur en indiquant la ligne et la colonne *)
let get_line n =
	let c = Stdlib.open_in !ifile in
	for iLine = 0 to n-2 do
		ignore (input_line c)
	done;
	let result = input_line c in
	Stdlib.close_in c;
	result

let localisation buf =
	let pos = Lexing.lexeme_start_p buf in
	let nextLexem = Lexing.lexeme buf in
	let l = pos.pos_lnum in
	let c_1 = pos.pos_cnum - pos.pos_bol in
	let c_2 = c_1 + (String.length nextLexem) in
	eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l c_1 c_2;
	try
		let line = get_line l in
		eprintf "%s%s%s%s%s\n" (String.sub line 0 c_1) colorGreen (String.sub (line^" ")  c_1 (c_2-c_1)) colorDefault (String.sub (line^" ") c_2 (max (String.length line -c_2) 0));
		eprintf "%s%s%s%s\n" (String.make c_1 ' ' ) colorGreen (String.make (c_2-c_1) '^') colorDefault

	with
		|End_of_file -> eprintf "%s\n" (colorGreen ^ "End_of_file" ^ colorDefault)


	

let () =
	(* Parsing de la ligne de commande *)
	Arg.parse options (set_file ifile) usage;

	(* On vérifie que le nom du fichier source a bien été indiqué *)
	if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

	(* Ce fichier doit avoir l'extension .purs *)
	if not (Filename.check_suffix !ifile ".purs") then begin
		eprintf "Le fichier d'entrée doit avoir l'extension .purs\n@?";
		Arg.usage options usage;
		exit 1
	end;

	let f = open_in !ifile in

	let buf = Lexing.from_channel f in

	try
		let p = Purescript_parser.file Purescript_lexer.next_token buf in
		close_in f;
		Purescript_ast.print_file Format.std_formatter p;

		if !parse_only then exit 0;
		
		Purescript_typage.typfile p;
		
		if !type_only then exit 0;

		(* ajouter ici la production de code assembleur *)

		exit 0

	with
		| Purescript_lexer.Lexing_error c ->
			localisation buf; 
			eprintf "%s" (colorRed^"Erreur lexicale : "^colorDefault^c^"\n");
			exit 1
		| Purescript_parser.Error ->
			localisation buf;
			eprintf "%s\n" (colorRed ^ "Erreur syntaxique" ^ colorDefault);
			exit 1
		| Purescript_typage.TypingError s -> (* TODO afficher e et afficher le numero de ligne *)
			eprintf "%s\n" (colorRed ^ "Erreur typage : "^s ^ colorDefault);
			exit 1



			
