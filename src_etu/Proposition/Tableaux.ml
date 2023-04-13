open Formule

(** Teste si une formule est satisfaisable, selon la méthode des tableaux. *)
let tableau_sat : formule -> bool = fun _ -> failwith "to do"

(** Teste si une formule est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let tableau_ex_sat : formule -> (string * bool) list option =
 fun _ -> failwith "to do"

(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon la méthode des tableaux.*)
let tableau_all_sat : formule -> (string * bool) list list =
 fun _ -> failwith "to do"
