(* Exercice 1 - Signature *)
module type tARITH = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val opp : t -> t
  val of_int : int -> t
  val to_string : t -> string
end 
      
(* Exercice 2 - Modules implémentant une signature *)
module INT : tARITH = struct
  type t = int
  let zero = 0
  let one = 1
  let add a b = a + b
  let mul a b = a * b
  let opp a = -a
  let of_int a = a
  let to_string a = string_of_int a
end
 
(*Attention: les mod en ocaml gardent le signe*)
module M3 : tARITH = struct
  type t = Zero | Un | Deux
  let zero = Zero
  let one = Un
  let add n1 n2 =
    match n1, n2 with
    | Zero, _ -> n2
    | _, Zero -> n1
    | Un, Un -> Deux
    | Deux, Deux -> Un
    | _, _ -> Zero
  let mul entier1 = function
    | Zero -> Zero
    | Un -> entier1
    | Deux -> add entier1 entier1
  let opp = function
    | Zero -> Zero
    | Un -> Deux
    | Deux -> Un
  let of_int entier =
    match entier mod 3 with
    | 0 -> Zero
    | 1 | -2 -> Un
    | 2 | -1 -> Deux
    | _ -> failwith "Not an M3 type"
  let to_string = function
    | Zero -> "0"
    | Un -> "1"
    | Deux -> "2" 
end  
(* Exercice 3 - Signature paramétrée *)
module type tEXP = functor (A : tARITH) -> sig
  type t
  val cst : A.t -> t
  val opp : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val compute : t -> A.t
end
  
(* Exercice 4 - Foncteur implémentant une signature paramétrée *)
module EXP : tEXP = functor (A : tARITH) -> struct 
  type t = Cst of A.t | Opp of t | Add of t*t | Mul of t*t
  let cst t = Cst t
  let opp t = Opp t
  let add a b = Add (a,b)
  let mul a b = Mul (a,b)
  let rec compute = function
    | Cst(a) -> a
    | Opp(a) -> A.opp (compute a) 
    | Add(a,b) -> A.add (compute a) (compute b)
    | Mul(a,b) -> A.mul (compute a) (compute b) 
end

(* Exercice 5 - Instanciation *)
module EXP_INT = EXP(INT);;
module EXP_M3 = EXP(M3);;

let expr_int = 
  let deuxInt = EXP_INT.cst(INT.of_int(2)) in
  EXP_INT.(mul deuxInt (add deuxInt deuxInt));;

let expr_m3 = 
  let deuxM3 = EXP_M3.cst(M3.of_int(2)) in
  EXP_M3.(mul deuxM3 (add deuxM3 deuxM3));;
  
(*
val expr_int : EXP_INT.t = <abstr>
val expr_m3 : EXP_M3.t = <abstr>
le mot abstr signifie que c'est un type caché
   *)

(* Exercice 6 - Polynômes *)
module type tPOLY = functor (A : tARITH) -> sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val opp : t -> t
  val of_int : int -> t
  val to_string : t -> string
  val cst : A.t -> t
  val varx : t
  val evalx : t -> A.t -> A.t
  val def : int -> (int -> A.t) -> t
end

(*on peut ajouter de nouvelles fonctions dans limplementation qui 
nest pas dans la signature*)
module POLY : tPOLY = functor (A : tARITH) -> struct
  type t = A.t list
  let zero = [A.of_int 0]
  let one = [A.of_int 1]
  let rec add poly1 poly2 = 
    match (poly1, poly2) with
    | (tete1 :: reste1, tete2 :: reste2) -> 
        A.add tete1 tete2 :: add reste1 reste2
    | (zero, poly2) -> poly2
    | (poly1, zero) -> poly1
    | (_, _) -> zero
  let rec mul poly1 poly2 =
    match (poly1, poly2) with
    | (tete1 :: reste1, tete2 :: reste2) -> 
        A.mul tete1 tete2 :: mul reste1 reste2
    | (zero, poly2) -> zero
    | (poly1, zero) -> zero
    | (_, _) -> zero
  let rec opp = function
    | tete :: reste ->
        -tete :: opp reste
    | zero -> zero
  let of_int entier =
    [entier]
  let rec to_string = function
    | [] -> "0"
    | tete :: reste -> 
        String.concat (string_of_int tete) (to_string reste)
  
end


