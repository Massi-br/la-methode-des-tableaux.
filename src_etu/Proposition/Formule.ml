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
let string_of_formule : formule -> string = fun _ -> failwith "to do"

type interpretation = string -> bool
(** Type des interprétations. *)

(** Évalue une formule en fonction d'une interprétation. *)
let eval : interpretation -> formule -> bool = fun _ _ -> failwith "to do"
