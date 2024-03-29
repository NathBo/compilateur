open Format
open Lexing
open Purescript_ast

(* Option de compilation *)
let parse_only = ref false
let type_only = ref false
let show_algebraic = ref false

(* Nom du fichier source *)
let ifile = ref ""

let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
	["--parse-only", Arg.Set parse_only,
	 "Pour ne faire que l'analyse syntaxique";

	 "--type-only", Arg.Set type_only,
      "Pour ne faire que l'analyse syntaxique puis le typage";
     
     "--show-algebraic", Arg.Set show_algebraic,
     "Pour afficher la représentation mémoire des types algébriques"
    ]

let usage = "compilateur de purscript\nutilisation : ./ppurs myfile.purs"
(* définition des couleurs pour l'affichage *)
let colorRed = "\o033[31m"
let colorGreen = "\o033[32m"
let colorDefault = "\o033[0m"
(* localise une erreur en indiquant la ligne et la colonne *)
let get_line n1 =
	let c = Stdlib.open_in !ifile in
	for iLine = 0 to n1-2 do
		ignore (input_line c)
	done;
	let result = input_line c in
	Stdlib.close_in c;
	result

let calcPosition buf =
	let start = Lexing.lexeme_start_p buf in
	let nextLexem = Lexing.lexeme buf in
	let longueurLexem = String.length nextLexem in
	{startpos = start; endpos =
		{pos_fname = start.pos_fname ; pos_lnum = start.pos_lnum; pos_bol = start.pos_bol ; pos_cnum = start.pos_cnum + longueurLexem} }


let localisation pos =
	let l = pos.startpos.pos_lnum in
	let c_1 = pos.startpos.pos_cnum - pos.startpos.pos_bol in
	try
		let line = get_line pos.startpos.pos_lnum in
		let c_2 =
			if pos.startpos.pos_lnum = pos.endpos.pos_lnum then pos.endpos.pos_cnum - pos.endpos.pos_bol
			else String.length line in
		eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l c_1 c_2;
		eprintf "%s%s%s%s%s\n" (String.sub line 0 c_1) colorGreen (String.sub (line^" ")  c_1 (c_2-c_1)) colorDefault (String.sub (line^" ") c_2 (max (String.length line -c_2) 0));
		eprintf "%s%s%s%s\n" (String.make c_1 ' ' ) colorGreen (String.make (c_2-c_1) '^') colorDefault

	with
		|End_of_file -> eprintf "File \"%s\", line %d, characters %d-%d:\n%s\n" !ifile l c_1 c_1 (colorGreen ^ "End_of_file" ^ colorDefault)

let str_replace pattern repl text =
        let result = ref text in
        let patternLen = String.length pattern in
        for beg = 0 to (String.length text - patternLen) do
                if String.equal pattern (String.sub text beg patternLen) then
                        result := (String.sub text 0 beg)^repl^(String.sub text (beg+patternLen) (String.length text - patternLen - beg))
	done;
	!result


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

		if !parse_only then exit 0;

		(*Purescript_ast.print_file Format.std_formatter p;*)
		
		let typ = Purescript_typage.typfile p in

		(*Purescript_typage.print_tfile Format.std_formatter typ;*)
		
		if !type_only then exit 0;
                
        let programe = Purescript_production_code.genere_code typ.tvdecls !show_algebraic in
        let fout = open_out (str_replace ".purs" ".s" !ifile) in
        X86_64.print_program (Format.formatter_of_out_channel fout) programe;
        close_out fout;

		exit 0

	with
		| Purescript_lexer.Lexing_error c ->
			localisation (calcPosition buf); 
			eprintf "%s" (colorRed^"Erreur lexicale : "^colorDefault^c^"\n");
			exit 1
		| Purescript_parser.Error ->
			localisation (calcPosition buf);
			eprintf "%s\n" (colorRed ^ "Erreur syntaxique" ^ colorDefault);
			exit 1
		| Purescript_typage.TypingError (s,pos) ->
			localisation pos;
			eprintf "%s\n" (colorRed ^ "Erreur typage ligne "^(string_of_int pos.startpos.pos_lnum) ^ " colonne "^(string_of_int (pos.startpos.pos_cnum-pos.startpos.pos_bol)) ^" : "^s ^ colorDefault);
			exit 1
