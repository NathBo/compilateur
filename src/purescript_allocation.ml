open X86_64
open Purescript_ast
open Purescript_typage

type a_file = a_tdecl list

and a_tdecl =
        | A_defn of a_defn
        | A_rien
        (* TODO *)

and a_defn =
        {a_ident : ident; a_patargs : a_patargs list ; a_expr : a_expr}
and a_patargs =
        | A_constant of a_constant
        | A_lident of ident
        (* TODO *)
and a_constant =
        | A_bool of bool
        | A_int of int
        | A_string of string
and a_expr =
        | A_atom of a_atom * typ
        | A_lident of ident * a_atom list * typ
and a_atom =
        | A_constant of a_constant * typ
        | A_lident of ident * typ
        | A_expr of a_expr * typ



let rec typage_to_alloc t = (List.filter_map traduit_tvdecl t) 

and traduit_tvdecl = function
        | TDdefn x -> Some (A_defn (traduit_tdefn x))
        | TDtdecl x -> None
        | _ -> failwith "pas encore def 1"

and traduit_tdefn (x:tdefn) : a_defn =
        {a_ident = x.tident; a_patargs = (List.map traduit_tpatarg x.tpatargs) ; a_expr = traduit_texpr x.texpr}

and traduit_tpatarg = function
        | TPconstant x -> A_constant (traduit_tconstant x)
        | TPlident x -> A_lident x
        | _ -> failwith "pas encore def 2"

and traduit_texpr = function
        | TEatom (x,y) -> A_atom ((traduit_atom x),y)
        | TElident (x,y,z) -> A_lident (x, (List.map traduit_atom y), z)
        | _ -> failwith "pas encore def 3"

and traduit_tconstant = function
        | TCbool x -> A_bool x 
        | TCstring x -> A_string x
        | TCint x -> A_int x

and traduit_atom = function
        | TAconstant (x,y) -> A_constant (traduit_tconstant x,y)
        | TAlident (x,y) -> A_lident (x,y)
        | TAexpr (x,y) -> A_expr (traduit_texpr x, y)
        | _ -> failwith "pas encore def 5"







