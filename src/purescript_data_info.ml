open Purescript_ast
open Purescript_typage
open Format

module Smap = Map.Make(String)
type local_env = int Smap.t

type class_info = {hash : local_env ; size : int }

let add_possi (nom,donnees) info id =
        let nouv_hash = Smap.add nom id info.hash in
        let cur_size = List.length donnees in
        {hash = nouv_hash ; size = max info.size cur_size}

let creer_compteur () =
        let i = ref (-1) in
        (fun () -> (incr i; !i))

let build_class_info possibilites =
        let cmpt = creer_compteur () in
        let info = ref {hash = Smap.empty ; size = 0} in 
        List.iter (fun possi -> info := add_possi possi !info ( cmpt ())) possibilites ;
        !info

let build_class_dico prog =
        let dico = ref Smap.empty in
        List.iter (fun x ->
                match x with
                        | TDdata (nom, _, possibilites) -> dico := Smap.add nom (build_class_info possibilites) !dico
                        | _ -> ()
        ) prog ;
        !dico

let print_class_dico fmt dico = begin
        fprintf fmt "Class dico :\n" ;
        Smap.iter (fun nom info ->
                fprintf fmt "%s (taille %d) devient :\n" nom info.size ;
                Smap.iter (fun lettre id -> fprintf fmt "     %s -> %d\n" lettre id) info.hash
        ) dico
end






