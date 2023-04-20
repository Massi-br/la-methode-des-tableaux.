(* open Formule *)

(** Teste si une formule est satisfaisable, selon la méthode des tableaux. *)
let t = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")), Imp (Ou (Atome "a", Atome "b"), Atome "c")));;

let contradiction = Et(Et(Atome "a" ,Atome "c"), Ou(Et(Atome "b",Non (Atome "a")),Imp(Ou(Atome "a",Atome "b"),Non (Atome "c"))));;

let rec aux_tableau_sat (l1:formule list) (formules:formule list):bool =
      match formules with
      | []-> true
      | h::t-> 
            match h with
            | Bot -> false
            | Top -> aux_tableau_sat l1  t
            | Atome a ->
                  if List.mem (Non (Atome a)) l1 then false (* Si le littéral et sa négation sont présents, la branche est fermée *)
                  else 
                      if List.mem (Atome a) l1 then
                        aux_tableau_sat l1 t (*  on passe à la formule suivante *)
                      else
                        let new_l1 = (Atome a) :: l1 in (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                        aux_tableau_sat new_l1 t
            | Non (Atome a) ->
                  if List.mem (Atome a) l1 then false (* Si le littéral et sa négation sont présents, la branche est fermée *)
                  else
                        if List.mem (Non (Atome a)) l1 then 
                              aux_tableau_sat l1 t (* Si le littéral est présent, on passe à la formule suivante *)
                        else
                              let new_l1 = (Non (Atome a)) :: l1 in (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                              aux_tableau_sat new_l1 t (* On passe à la formule suivante *)
                              
            | Et (f, g) -> aux_tableau_sat l1 (f :: g :: t) (* On ajoute les deux sous-formules dans la liste à traiter *)
            | Ou (f, g) -> aux_tableau_sat l1 (f :: t) || aux_tableau_sat l1 (g :: t) (* On traite les deux cas en parallèle *)
            |Equiv(f, g) ->  aux_tableau_sat l1 (f::g::t) || aux_tableau_sat l1 ((Non f)::(Non g)::t)  (* car a=b <=> a&b | ¬a&¬b *)
            |Imp (f,g) -> aux_tableau_sat l1 ((Non f)::t) || aux_tableau_sat l1 (g::t)
            |Xor (f, g) -> aux_tableau_sat l1 ((Non f)::g::t) || aux_tableau_sat l1 (f::(Non g)::t) (* car a⊕b <=> a&¬b | ¬a&b *)
            |Non (f) ->
                  match f with
                  | Bot -> true
                  | Top -> false
                  | Atome a ->
                        if List.mem (Atome a) l1 then false (* Si le littéral et sa négation sont présents, la branche est fermée *)
                        else
                              if List.mem (Non (Atome a)) l1 then 
                                    aux_tableau_sat l1 t (* Si le littéral est présent, on passe à la formule suivante *)
                              else
                                    let new_l1 = (Non (Atome a)) :: l1 in (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                                    aux_tableau_sat new_l1 t (* On passe à la formule suivante *)
                  | Et (f, g) -> aux_tableau_sat l1 ((Non f)::t) || aux_tableau_sat l1 ((Non g)::t)
                  | Ou (f, g) -> aux_tableau_sat l1 ((Non f)::(Non g)::t)
                  | Imp (f, g) -> aux_tableau_sat l1 (f::(Non g)::t)
                  | Equiv (f, g) -> aux_tableau_sat l1 ((Non f)::g::t) || aux_tableau_sat l1 (f::(Non g)::t) (* a vérifier*)
                  | Xor (f, g) -> aux_tableau_sat l1 (f::g::t) || aux_tableau_sat l1 ((Non f)::(Non g)::t) (* a vérifier*)
                  | _ -> aux_tableau_sat l1 (f::t)
      ;;
let tableau_sat (f:formule):bool =
      aux_tableau_sat [] [f];;

      let rec aux (l1:formule list) (formules:formule list) (l2:(string * bool) list): (string * bool) list option=
            match formules with
            | []-> Some l2
            | h::t-> 
                  match h with
                  | Bot -> None
                  | Top -> aux l1 t l2
                  | Atome a ->
                        if List.mem (Non (Atome a)) l1 then None (* Si le littéral et sa négation sont présents, la branche est fermée *)
                        else 
                            if List.mem (Atome a) l1 then
                              aux l1 t l2 (* on passe à la formule suivante *)
                            else
                              let new_l1 = (Atome a) :: l1 in (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                              aux new_l1 t ((a, true)::l2) 
                  | Non (Atome a) ->
                        if List.mem (Atome a) l1 then None (* Si le littéral et sa négation sont présents, la branche est fermée *)
                        else
                              if List.mem (Non (Atome a)) l1 then 
                                    aux l1 t l2 (* Si le littéral est présent, on passe à la formule suivante *)
                              else
                                    let new_l1 = (Non (Atome a)) :: l1 in (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                                    aux new_l1 t ((a, false)::l2) (* On passe à la formule suivante *)
                                    
                  | Et (f, g) -> aux l1 (f :: g :: t) l2  
                  | Ou (f, g) ->
                        ( match (aux l1 (f :: t) l2, aux l1 (g :: t) l2) with
                        | (Some res, _) -> Some res
                        | (_, Some res) -> Some res
                        | _ -> None
                        )
                    
                  |Imp (f,g) ->
                        ( match (aux l1 (Non f :: t) l2, aux l1 (g :: t) l2) with
                          | (Some res, _) -> Some res
                          | (_, Some res) -> Some res
                          | _ -> None
                        )
                  |Equiv(f, g) ->  (* a=b <=> a && b || ¬a && ¬b *)
                        ( match (aux l1 (f::g::t) l2, aux l1 ((Non f)::(Non g)::t) l2) with
                              | (Some res, _) -> Some res
                              | (_, Some res) -> Some res
                              | _ -> None
                        )
                  |Xor (f, g) -> (* a⊕b <=> a&¬b | ¬a&b *)
                        ( match (aux l1 ((Non f)::g::t) l2, aux l1 (f::(Non g)::t) l2) with
                        | (Some res, _) -> Some res
                        | (_, Some res) -> Some res
                        | _ -> None
                        )
                  | Non (f) ->
                        (match (aux l1 (f :: t) l2) with
                              | Some res -> Some (List.map (fun (s,b) -> if s = (Non f) then (s, not b) else (s, b)) res)
                              | _ -> None
                        )
            ;;
(** Teste si une formule est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let  tableau_ex_sat (f:formule): (string * bool) list option  =
            aux [] [f] [] ;;



(* (** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
    selon la méthode des tableaux.*)
let tableau_all_sat : formule -> (string * bool) list list =
 fun _ -> failwith "to do" *)
