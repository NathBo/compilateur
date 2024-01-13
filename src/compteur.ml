type compteur = {
  mutable valeur : int;
  saut : int;
  mutable dispo : bool ;
  mutable dependances : compteur list
}

let make saut init =
        (* creer un compteur innitialisé à pas saut*init *)
        {valeur = init ; saut = saut; dispo = true; dependances = []}
let get cmpt =
        assert(cmpt.dispo) ;
        let result = cmpt.valeur in
        cmpt.valeur <- cmpt.valeur + cmpt.saut;
        result
let size cmpt =
        cmpt.valeur

let copy cmpt1 =
        let cmpt2 = {valeur = cmpt1.valeur ; saut = cmpt1.saut ; dispo = true; dependances = []} in
        cmpt1.dispo <- false ;
        cmpt1.dependances <- cmpt2::cmpt1.dependances ;
        cmpt2
let rec union cmpt =
        let maxi = ref cmpt.valeur in
        List.iter (fun c -> 
                union c ;
                if ((c.valeur > !maxi && cmpt.saut >= 0) || (c.valeur < !maxi && cmpt.saut <= 0)) then maxi := c.valeur;
                c.dispo <- false ; ()
        ) cmpt.dependances ;
        cmpt.dispo <- true ;
        cmpt.valeur <- !maxi
