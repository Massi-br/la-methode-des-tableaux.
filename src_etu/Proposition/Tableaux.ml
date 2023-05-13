(* open Formule *)

 let rec aux_tableau_sat (l1:formule list) (formules:formule list):bool =
      match formules with
      | []-> true
      | h::t-> 
            (match h with
            | Bot -> false
            | Top -> aux_tableau_sat l1  t
            | Atome a ->
                  if List.mem (Non (Atome a)) l1 then false             (* Si le littéral et sa négation sont présents, la branche est fermée *)
                  else 
                      if List.mem (Atome a) l1 then
                        aux_tableau_sat l1 t                            (*  on passe à la formule suivante *)
                      else
                        let new_l1 = (Atome a) :: l1 in                 (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                        aux_tableau_sat new_l1 t                        (*  et on passe à la formule suivante *)
            | Et (f, g) -> aux_tableau_sat l1 (f :: g :: t)                                             (* On ajoute les deux sous-formules dans la liste à traiter *)
            | Ou (f, g) -> aux_tableau_sat l1 (f :: t) || aux_tableau_sat l1 (g :: t)                   (* On traite les deux cas en parallèle *)
            | Equiv(f, g) ->  aux_tableau_sat l1 (f::g::t) || aux_tableau_sat l1 ((Non f)::(Non g)::t)  (* car a=b <=> a&&b || ¬a&&¬b *)
            | Imp (f,g) -> aux_tableau_sat l1 ((Non f)::t) || aux_tableau_sat l1 (g::t)
            | Xor (f, g) -> aux_tableau_sat l1 ((Non f)::g::t) || aux_tableau_sat l1 (f::(Non g)::t)    (* car a⊕b <=> a&&¬b || ¬a&&b *)
            | Nand (f,g) -> aux_tableau_sat l1 ((Non f) :: t) || aux_tableau_sat l1 ((Non g) :: t)
            | Non (f) ->
                  (match f with
                  | Bot -> aux_tableau_sat l1  t
                  | Top -> false
                  | Atome a ->
                        if List.mem (Atome a) l1 then false 
                        else
                              if List.mem (Non (Atome a)) l1 then 
                                    aux_tableau_sat l1 t 
                              else
                                    let new_l1 = (Non (Atome a)) :: l1 in 
                                    aux_tableau_sat new_l1 t 
                                    
                  | Et (f, g) -> aux_tableau_sat l1 ((Non f)::t) || aux_tableau_sat l1 ((Non g)::t)
                  | Ou (f, g) -> aux_tableau_sat l1 ((Non f)::(Non g)::t)
                  | Imp (f, g) -> aux_tableau_sat l1 (f::(Non g)::t)
                  | Equiv (f, g) -> aux_tableau_sat l1 ((Non f)::g::t) || aux_tableau_sat l1 (f::(Non g)::t) 
                  | Xor (f, g) -> aux_tableau_sat l1 (f::g::t) || aux_tableau_sat l1 ((Non f)::(Non g)::t) 
                  | Nand (f,g) -> aux_tableau_sat l1 (f::g::t) 
                  | _ -> aux_tableau_sat l1 (f::t)
                  )
            )
;;
(** Teste si une formule est satisfaisable, selon la méthode des tableaux. *)
let tableau_sat (f:formule):bool =
      aux_tableau_sat [] [f];; 


let rec aux (l1:formule list) (formules:formule list) (l2:(string * bool) list): (string * bool) list option=
      match formules with
      | []-> Some l2
      | h::t-> 
            (match h with
            | Bot -> None
            | Top -> aux l1 t l2
            | Atome a ->
                  if List.mem (Non (Atome a)) l1 then None                          (* Si le littéral et sa négation sont présents, la branche est fermée *)
                  else 
                        if List.mem (Atome a) l1 then
                              aux l1 t l2                                           (* on passe à la formule suivante *)
                        else
                              let new_l1 = (Atome a) :: l1 in                       (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                              aux new_l1 t ((a, true)::l2) 
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
            |Equiv(f, g) ->  
                  ( match (aux l1 (f::g::t) l2, aux l1 ((Non f)::(Non g)::t) l2) with
                        | (Some res, _) -> Some res
                        | (_, Some res) -> Some res
                        | _ -> None
                  )
            |Xor (f, g) -> 
                  ( match (aux l1 ((Non f)::g::t) l2, aux l1 (f::(Non g)::t) l2) with
                        | (Some res, _) -> Some res
                        | (_, Some res) -> Some res
                        | _ -> None
                  )
            |Nand (f, g) -> 
                  ( match (aux l1 ((Non f)::t) l2, aux l1 ((Non g)::t) l2) with
                  | (Some res, _) -> Some res
                  | (_, Some res) -> Some res
                  | _ -> None
                  )
            | Non (f) ->
                  (match f with
                        | Bot -> aux l1 t l2
                        | Top -> None
                        | Atome a ->
                              if List.mem (Atome a) l1 then None                                      (* Si le littéral et sa négation sont présents, la branche est fermée *)
                              else
                                    if List.mem (Non (Atome a)) l1 then 
                                          aux l1 t l2                                                 (* Si le littéral est présent, on passe à la formule suivante *)
                                    else
                                          let new_l1 = (Non (Atome a)) :: l1 in                       (* sinon, on ajoute le littéral à la liste de littéraux rencontrés *)
                                          aux new_l1 t ((a, false)::l2)                               (* On passe à la formule suivante *)
                        | Et (f, g) ->
                              ( match (aux l1 ((Non f) :: t) l2, aux l1 ((Non g) :: t) l2) with
                                    | (Some res, _) -> Some res
                                    | (_, Some res) -> Some res
                                    | _ -> None
                              )
                        | Ou (f, g) -> aux l1 ((Non f)::(Non g)::t) l2
                        | Imp (f, g) -> aux l1 (f::(Non g)::t) l2
                        | Equiv (f, g) ->                                                             
                              ( match (aux l1 (f::(Non g)::t) l2, aux l1 ((Non f)::g::t) l2) with
                                    | (Some res, _) -> Some res
                                    | (_, Some res) -> Some res
                                    | _ -> None
                              )
                        | Xor (f, g) ->
                              ( match (aux l1 (f::g::t) l2, aux l1 ((Non f)::(Non g)::t) l2) with
                                    | (Some res, _) -> Some res
                                    | (_, Some res) -> Some res
                                    | _ -> None
                              )
                        | Nand (f, g) -> aux l1 (f :: g :: t) l2 
                        | Non (f) -> aux l1 (f::t) l2 
                  )  
            )         
;;
(** Teste si une formule est satisfaisable, renvoyant None si ce n'est pas le cas
      et Some res sinon, où res est une liste de couples (atome, Booléen)
      suffisants pour que la formule soit vraie. *)
let  tableau_ex_sat (f:formule): (string * bool) list option  =
            aux [] [f] [] ;;  

(* 

let rec aux1 (l1:formule list) (formules:formule list) (l2:(string * bool) list list): (string * bool) list list=
      match formules with
      | []-> l2
      | l ->
      (match (List.hd l) with
            | Bot -> []
            | Top -> aux1 l1 t l2
            | Atome a ->
                  if List.mem (Non (Atome a)) l1 then [] 
                  else 
                        if List.mem (Atome a) l1 then 
                              aux1 l1 t l2 
                        else
                              let new_l1 = (Atome a) :: l1 in 
                              aux1 new_l1 t ([(a, true)]::l2) 
            | Et (f, g) -> aux1 l1 (f :: g :: t) l2 
            | Ou (f, g) ->
                  (aux1 l1 (f :: t) l2) @ (aux1 l1 (g :: t) l2 )
            | Imp (f,g) ->
                        let branche1 = aux1 l1 (Non f :: t) l2 in 
                              let branche2 = aux1 l1 (g :: t) l2 in
                               branche1 @ branche2 
            | Equiv(f, g) ->  
                        let branche1 = aux1 l1 (f::g::t) l2 in 
                              let branche2 = aux1 l1 ((Non f)::(Non g)::t) l2 in 
                                    [(List.concat branche1);(List.concat branche2) ]
            | Xor (f, g) -> 
                        let branche1 = aux1 l1 ((Non f)::g::t) l2 in
                              let branche2 = aux1 l1 (f::(Non g)::t) l2 in 
                                    [(List.concat branche1);(List.concat branche2) ]
            | Nand (f, g) ->
                        let branche1 = aux1 l1 (Non (f) :: t) l2 in
                              let branche2 = aux1 l1 (Non (g) :: t) l2 in
                                branche1 @ branche2 
            | Non (f) ->
                  (match f with
                        | Bot -> aux1 l1 t l2
                        | Top -> []
                        | Atome a ->
                              if List.mem (Atome a) l1 then [] 
                              else
                                    if List.mem (Non (Atome a)) l1 then 
                                          aux1 l1 t l2 
                                    else
                                    let new_l1 = (Non (Atome a)) :: l1 in 
                                    aux1 new_l1 t ([(a, false)]::l2) 

                        | Ou (f, g) -> aux1 l1 ((Non f)::(Non g)::t) l2 
                        | Imp (f, g) -> aux1 l1 (f::(Non g)::t) l2
                        | Nand (f,g) -> aux1 l1 (f :: g :: t) l2    

                        | Et (f, g) ->(
                              let branche1 = aux1 l1 ((Non f) :: t) l2 in
                                    let branche2 = aux1 l1 ((Non g) :: t) l2 in
                                    [(List.concat branche1);(List.concat branche2)]
                              )
                        | Equiv (f, g) -> (
                              let branche1 = aux1 l1 (f::(Non g)::t) l2 in
                                    let branche2 = aux1 l1 ((Non f)::g::t) l2 in
                                    [(List.concat branche1);(List.concat branche2) ]
                              )
                        | Xor (f, g) -> (
                              let branche1 = aux1 l1 (f::g::t) l2 in
                                    let branche2 = aux1 l1 ((Non f)::(Non g)::t) l2 in
                                    [(List.concat branche1);(List.concat branche2) ]
                              )
                        | Non (f) -> aux1 l1 (f::t) l2 
                  )   
      )     
;;
(** Renvoie la liste des listes de couples (atome, Booléen) suffisants pour que la formule soit vraie,
          selon la méthode des tableaux.*)
let tableau_all_sat (f:formule): (string * bool) list list =
      if tableau_sat f = false
             then failwith "la formule n'est pas satisfaisable" 
      else 
            aux1 [] [f] [];;   *)



let tableau_all_sat (f:formule): (string * bool) list list =
      if tableau_sat f = false then failwith "la formule n'est pas satisfaisable" 
      else
            let rec aux1 l1 l2 res acc=
                  match (List.hd l2) with
                  | Bot -> []
                  | Top -> if (List.tl l2) == [] then res::acc
                           else
                              aux1 l1 (List.tl l2) res acc
                  | Atome a ->
                        if List.mem (Non (Atome a)) l1 then [] 
                        else 
                              if List.mem (Atome a) l1 then
                                    if ((List.tl l2) == []) then res::acc
                                    else
                                          aux1 l1 (List.tl l2) res acc
                              else 
                                    if ((List.tl l2) == []) then ((a, true)::res)::acc
                                    else
                                          let new_l1 = (Atome a) :: l1 in 
                                          aux1 new_l1 (List.tl l2) ((a, true)::res) acc    

                  | Et(f,g) -> aux1 l1 ([f;g]@(List.tl l2)) res  acc
                  | Ou(f,g) -> (aux1 l1 ([f]@(List.tl l2)) res acc) @ (aux1 l1 ([g]@(List.tl l2)) res acc )
                  | Imp(f,g) -> aux1 l1 (Ou((Non f),g)::(List.tl l2)) res acc
                  | Xor(f,g) -> aux1 l1 (Ou(Et(f,(Non g)),Et((Non f), g))::(List.tl l2)) res  acc           
                  | Equiv(f,g) -> aux1 l1 ((Et(Ou(f,(Non g)),Ou((Non f),g)))::(List.tl l2)) res acc
                  | Nand(f,g) ->aux1 l1 (Non(Et(f,g))::(List.tl l2)) res acc
                  | Non(f) ->
                        (
                              match f with
                              | Bot -> aux1 l1 (Top::(List.tl l2)) res acc
                              | Top -> aux1 l1 (Bot::(List.tl l2)) res acc
                              | Atome a ->
                                    if List.mem (Atome a) l1 then [] 
                                    else
                                          if List.mem (Non (Atome a)) l1 then
                                                if ((List.tl l2) == []) then res::acc
                                                else
                                                      aux1 l1 (List.tl l2) res acc
                                          else
                                                if ((List.tl l2) == []) then ((a, false)::res)::acc
                                                else 
                                                      let new_l1 = (Non (Atome a)) :: l1 in 
                                                      aux1 new_l1 (List.tl l2) ((a, false)::res)  acc 

                              | Et(f,g) -> aux1 l1 (Ou((Non f),(Non g))::(List.tl l2)) res  acc
                              | Ou(f,g) -> aux1 l1 (Et((Non f),(Non g))::(List.tl l2)) res  acc
                              | Imp(f,g) -> aux1 l1 ( (Non(Ou((Non f),g)))::(List.tl l2)) res acc
                              | Xor(f,g) -> aux1 l1 ((Non(Ou(Et(f,(Non g)),Et((Non f), g))))::(List.tl l2)) res   acc   
                              | Equiv(f,g) -> aux1 l1 ((Non((Et(Ou(f,(Non g)),Ou((Non f),g)))))::(List.tl l2)) res acc
                              | Nand(f,g) ->aux1 l1 ((Et(f,g))::(List.tl l2)) res acc
                              | Non (f) -> aux1 l1 (f::List.tl l2) res acc
                        )
            in aux1 [] [f] [] [] ;;

