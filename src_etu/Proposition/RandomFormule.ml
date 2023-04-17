(* open Formule *)
open Random
let () = Random.self_init();;

let random_atom (alphabet):formule = Atome (List.nth alphabet (Random.int (List.length alphabet)))

let nullaire = if Random.bool () then Top else Bot ;;

(** random_form atoms k renvoie une formule pseudo-aléatoire
   avec k opérateurs et des atomes de la liste atoms, liste
   supposée non vide *)
let rec random_form sigma  n =
    match n with 
   | 0 -> random_atom sigma
   | 1 ->
      (match Random.int 3 with
       | 0 -> nullaire
       | 1 -> Non (random_atom sigma)
       | _ ->
          let m = Random.int 5 in
          (match m with
           | 0 -> Ou ((random_atom sigma) ,(random_atom sigma))
           | 1 -> Xor ((random_atom sigma) ,(random_atom sigma))
           | 2 -> Equiv ((random_atom sigma), (random_atom sigma))
           | 3 -> Imp ((random_atom sigma), (random_atom sigma))
           | _ -> Et ((random_atom sigma), (random_atom sigma))
          )
      )
   | _ ->
      if Random.bool() then
         Non (random_form sigma (n-1))
      else
         let k = Random.int n in
         let choix_op = Random.int 5 in 
         match choix_op with
         | 0 ->
             Et (random_form sigma k, random_form sigma (n-k-1))
         | 1 ->
             Ou (random_form sigma k, random_form sigma (n-k-1))
         | 2 ->
             Imp (random_form sigma k, random_form sigma (n-k-1))
         | 3 ->
             Xor (random_form  sigma k, random_form sigma (n-k-1))
         | _ ->
             Equiv (random_form sigma k, random_form sigma (n-k-1))
       ;;


let a =["b"; "d"; "c"; "e"];;