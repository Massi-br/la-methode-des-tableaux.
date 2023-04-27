(* open Formule *)
(* open Tableaux *)
(* open RandomFormule *)

let rec string_of_couples_listes l =
   match l with
   | [] -> ""
   | (s,b)::t -> "(" ^ s ^ "," ^ string_of_bool b ^ ")" ^ string_of_couples_listes t
 ;;
 
 let string_of_list_of_list l =
    let string_of_pair (s, b) = "(" ^ s ^ ", " ^ string_of_bool b ^ ")" in
    let string_of_list l = "[" ^ String.concat "; " (List.map string_of_pair l) ^ "]" in
    "[" ^ String.concat "; " (List.map string_of_list l) ^ "]"
 ;;


(** fonction convertissant un témoin de type (string * bool) list en
   une interprétation, étendant les résultats manquants par des Booléens
   aléatoires. *)
let to_alea_inter (l2:(string * bool) list ):interpretation =
   let rec aux l s =
   match l with
   | [] -> Random.bool()
   | (s2,b)::t -> if s = s2 then b else aux t s
   in
   fun s -> aux l2 s ;;

(** test_valid n réalise :
— la génération d’une formule aléatoire f avec n opérateurs sur l’alphabet [ "a"; "b"; "c"; "d" ] ;
— l’affichage de cette formule sur la sortie standard ;
— la validation des résultats des fonctions tableau_ex_sat et tableau_all_sat de Tableaux.ml, en
vérifiant que chaque témoin obtenu peut être converti en une interprétation évaluant la formule f comme
vraie et retournant true si tous les résultats sont corrects, false sinon. *)
let test_valid_ex  n =
   let alphabet = [ "a"; "b"; "c"; "d" ] in
   let f = random_form alphabet n in 
   let () = print_endline (string_of_formule f) in
   let temoin = tableau_ex_sat f in
   let () = match temoin with
             | None -> print_endline "Pas de temoin \n"
             | Some l -> print_endline (string_of_couples_listes l)
   in
   match temoin with
   | None -> false
   | Some l -> eval (to_alea_inter l) f
;;

let test_valid_all  n =
   let alphabet = [ "a"; "b"; "c"; "d" ] in
   let f = random_form alphabet n in 
   let () = print_endline (string_of_formule f) in
   let temoin = tableau_all_sat f in
   let () = match temoin with
             | [] -> print_endline " Pas de temoin \n"
             | (h::t) as l -> print_endline (string_of_list_of_list l)
   in
   let rec aux acc =function
      | [] -> List.rev acc
      | h::t -> aux ((eval (to_alea_inter h ) f)::acc) t
   in aux [] temoin
;;
