open Formule
(* open Tableaux *)
(* open RandomFormule *)

(** fonction convertissant un témoin de type (string * bool) list en
   une interprétation, étendant les résultats manquants par des Booléens
   aléatoires. *)
let to_alea_inter : (string * bool) list -> interpretation =
 fun _ -> failwith "to do"

(** test_valid n réalise :
— la génération d’une formule aléatoire f avec n opérateurs sur l’alphabet [ "a"; "b"; "c"; "d" ] ;
— l’affichage de cette formule sur la sortie standard ;
— la validation des résultats des fonctions tableau_ex_sat et tableau_all_sat de Tableaux.ml, en
vérifiant que chaque témoin obtenu peut être converti en une interprétation évaluant la formule f comme
vraie et retournant true si tous les résultats sont corrects, false sinon. *)
let test_valid : int -> bool = fun _ -> failwith "to do"
