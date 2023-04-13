(* open Formule *)
open Random
let () = Random.self_init();;
(** random_form atoms k renvoie une formule pseudo-aléatoire
   avec k opérateurs et des atomes de la liste atoms, liste
   supposée non vide *)
let rec random_form alphabet = function
     0 -> Atome (List.nth alphabet (Random.int (List.length alphabet)))
   | 1 ->
      let nullaire = if Random.bool () then Top else Bot in
      let n = Random.int 3 in 
      (match n with 
       0 -> nullaire
      |1 -> Non (Atome (List.nth alphabet (Random.int (List.length alphabet))))
      |_ ->  Ou (Atome (List.nth alphabet (Random.int (List.length alphabet))))
            (Atome (List.nth alphabet (Random.int (List.length alphabet))))
            )
   | _ -> Atome "f"




let a =["b"; "d"; "c"; "e"];;