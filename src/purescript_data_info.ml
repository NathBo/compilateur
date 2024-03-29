open Purescript_ast
open Purescript_typage
open Format

module Smap = Map.Make(String)

type data_info = {hash : int ; size : int }
type class_info = data_info Smap.t


let build_class_info possibilites =
        let cmpt = Compteur.make 1 0 in
        let info = ref Smap.empty in 
        List.iter (fun possi -> info := Smap.add (fst possi) {hash = Compteur.get cmpt ; size = List.length (snd possi)} !info) possibilites;
        !info

let build_class_dico prog =
        let dico = ref Smap.empty in
        List.iter (fun x ->
                match x with
                        | TDdata (_, _, possibilites) -> dico := Smap.union (fun nom g d -> failwith "the same name is provided two times in data definition") !dico (build_class_info possibilites)
                        | _ -> ()
        ) prog ;
        !dico

let print_class_dico fmt dico = begin
        fprintf fmt "types algébriques :\n" ;
        Smap.iter (fun nom info ->
                fprintf fmt "   %s devient : %d, taille : %d\n" nom info.hash info.size
        ) dico
end
