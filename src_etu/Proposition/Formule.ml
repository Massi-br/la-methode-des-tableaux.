(** Type des formules. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule
  (* ajout d'opérateurs étendus *)
  | Xor of (formule * formule)
  | Equiv of (formule * formule)
  (*nouveau opérateur*)
  | Nand of (formule * formule)

(** Conversion d'une formule en chaîne de caractères. *)
let rec string_of_formule = function
  | Atome s -> s
  | Et (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; "∧"; string_of_formule g; ")" ]
  | Ou (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " ∨ "; string_of_formule g; ")" ]
  | Imp (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; " => "; string_of_formule g; ")" ]
  | Xor (f , g) ->String.concat ""
        ["(" ; string_of_formule f ; " ⊕ " ; string_of_formule g ; ")" ]
  |Equiv (f,g) -> String.concat ""
        ["("; string_of_formule f ; " = " ; string_of_formule g ; ")"]
  | Nand (f,g) -> String.concat ""
        ["("; string_of_formule f ; " ↑ " ; string_of_formule g ; ")"]
  | Non f -> String.concat "" [ " ¬"; string_of_formule f ]
  | Top -> "1"
  | Bot -> "0"

(** Type des interprétations. *)
type interpretation = string -> bool;;

(** Évalue une formule en fonction d'une interprétation. *)
let rec eval (i:interpretation): formule -> bool = function
  | Bot -> false
  | Top -> true
  | Atome s -> i s
  | Et (f, g) -> eval i f && (eval i g)
  | Ou (f, g) -> eval i f || eval i g
  | Non f -> not (eval i f)
  | Imp (f, g) -> (not (eval i f)) || eval i g
  | Xor (f ,g) ->  (eval i f && (not (eval i g))) || ((not (eval i f)) && (eval i g))
  | Equiv (f,g) ->  (eval i f && (eval i g)) || ((not (eval i f)) && (not (eval i g)))
  | Nand (f,g) -> not (eval i f && (eval i g))
;;

let i x =List.mem x ["a";"c"];; 

(* Définition de quelques formules pour les tests *)

(* let f0 = Ou (Nand (Atome "a", Atome "c") , Atome "b");; 
let f1 = Et(Atome "q", Non (Atome "q"));;
let f2 = Imp (Atome "p", Atome "q");;
let f3 = Equiv (Atome "p", Atome "q");;
let f4 = Ou (Et (Atome "p", Atome "q"), Non (Atome "p")) ;;
let f5 = Xor (Atome "p", Atome "q");;
let f6 = Imp (Ou (Et (Atome "a", Atome "b"), Atome "c"), Non (Atome "d"));; 
let f8 = Non (Ou (Et (Atome "a", Atome "b"), Atome "c"));;
let f9 = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")), Atome "c"));;
let f10 = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")), Ou (Atome "a", Atome "b")));; 
let f11 = Imp (Xor (Atome "b", Non (Atome "b")), Top);;
let f12 = Ou(Ou(Atome"a",Atome"b"), Ou(Atome"c",Atome"d") );;
let formule_enoncé = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")), Imp (Ou (Atome "a", Atome "b"), Atome "c")));;
let contradiction = Et(Et(Atome "a" ,Atome "c"), Ou(Et(Atome "b",Non (Atome "a")),Imp(Ou(Atome "a",Atome "b"),Non (Atome "c"))));;
 *)
