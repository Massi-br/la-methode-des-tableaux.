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

(** Conversion d'une formule en chaîne de caractères. *)
let rec string_of_formule = function
  | Atome s -> s
  | Et (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; "∧"; string_of_formule g; ")" ]
  | Ou (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; "∨"; string_of_formule g; ")" ]
  | Imp (f, g) ->
      String.concat ""
        [ "("; string_of_formule f; "=>"; string_of_formule g; ")" ]
  | Xor (f , g) ->String.concat ""
        ["(" ; string_of_formule f ; " ⊕ " ; string_of_formule g ; ")" ]
  |Equiv (f,g) -> String.concat ""
        ["("; string_of_formule f ; "=" ; string_of_formule g ; ")"]
  | Non f -> String.concat "" [ "¬ "; string_of_formule f ]
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


  (* Définition de quelques formules *)
let f1 = Et (Atome "q", Non (Atome "q"));;
let f2 = Imp (Atome "p", Atome "q");;
let f3 = Equiv (Atome "p", Atome "q");;
let f4 = Ou (Et (Atome "p", Atome "q"), Non (Atome "p")) ;;
let f5 = Xor (Atome "p", Atome "q");;
let f6 = Imp (Ou (Et (Atome "a", Atome "b"), Atome "c"), Non (Atome "d"));; (*a gerer *)
let f7 = Ou (Et (Atome "a", Atome "b"), Atome "c");;
let test2 = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")),  Atome "c"));;
let test3 = Et (Et (Atome "a", Atome "c"), Ou (Et (Atome "b", Non (Atome "a")), Ou (Atome "a", Atome "b")));; (*a gerer *)


let i x =List.mem x ["a";"c";"p"];; 