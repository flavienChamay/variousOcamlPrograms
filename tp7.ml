(* Prelude - pas besoin de recopier ce code *)
type 'a liste = Nil | Cons of 'a * 'a liste;;
type m3 = Zero | Un | Deux;;

(*Partie A*)

let liste_12 = Cons (1, Cons (2, Nil));;

(*Comprends pas......*)
let liste_liste0 = Cons (Nil, Nil);;

let rec longueur = function
  | Nil -> 0
  | Cons(tete, reste) -> 1 + longueur reste;;
  
let rec somme = function
  | Nil -> 0
  | Cons (tete, reste) -> tete + somme reste;;

let rec dernier = function
  | Cons (der, Nil) -> der
  | Cons(tete, reste) -> dernier reste
  | _ -> failwith "Erreur liste vide";; 

let rec append liste1 liste2 = 
  match liste1 with
  | Nil -> liste2
  | Cons(tete, reste) -> Cons(tete, append reste liste2);;

(*Partie B*)

(*Arithmetique*)

let m3s = function
  | Zero -> Un
  | Un -> Deux
  | Deux -> Zero;;

let m3p = function
  | Zero -> Deux
  | Un -> Zero
  | Deux -> Un;;

let m3plus entier1 = function
  | Zero -> entier1
  | Un -> m3s entier1
  | Deux -> m3s (m3s entier1);;

let rec m3mult entier1 = function
  | Zero -> Zero
  | Un -> entier1
  | Deux -> m3plus entier1 entier1;;

type exp = Constante of m3 | Somme of exp * exp | Produit of exp * exp;;

let rec calculer = function
  | Constante(exp1) -> exp1
  | Somme(exp1, exp2) -> m3plus (calculer exp1) (calculer exp2)
  | Produit(exp1, exp2) -> m3mult (calculer exp1) (calculer exp2);;

let e1 = Constante(Deux);;
let e2 = Somme(Constante(Un), Constante(Un));;


  
(*Mobiles*)

type figure = Cube of int | Sphere of int | Pyramide of int;;

type mobile = Figure of figure | Tige of mobile*int*int*mobile;;

let m1 = Tige(Figure(Pyramide(1)),2 ,4 ,
              Tige(Figure(Sphere(1)),3 ,4 , Figure(Cube(1))));;

let m2 = Figure(Cube(2));;

let rec nbSphere = function
  | Figure(Sphere(_)) -> 1
  | Tige(mobile1, _, _,mobile2) -> nbSphere mobile1 + nbSphere mobile2
  | _ -> 0;;

let max number1 number2 =
  if(number1 > number2) then number1
  else if(number2 > number1) then number2
  else number1;;

(*la hauteur est nbTiges + 1*)
let rec hauteur = function
  | Tige(mobile1, _, _, mobile2) -> 
      (max (hauteur mobile1) (hauteur mobile2)) + 1
  | _ -> 1;;

let rec echanger mobile f1 f2 =
  match mobile with
  | Tige(mobile1, a, b, mobile2) ->
      Tige(echanger mobile1 f1 f2, a, b, echanger mobile2 f1 f2)
  | Figure(a) -> 
      if a = f1 then
        Figure(f2)
      else 
        Figure(a);;

let rec listeFigure = function
  | Tige(mobile1, _, _, mobile2) ->
      (listeFigure mobile1) @ (listeFigure mobile2)
  | Figure(a) -> [Figure(a)];;

let pi = 4.*.atan 1.;;

let masseFigure = function
  | Cube(a) -> float_of_int(a)*.float_of_int(a)*.float_of_int(a)*.9.
  | Pyramide(a) -> (float_of_int(a)*.float_of_int(a)*.float_of_int(a)/.3.)*.9.
  | Sphere(a) -> (4./.3.)*.pi*.float_of_int(a)*.float_of_int(a)*.float_of_int(a);;

let rec masseMobile = function
  | Tige(mob1,_,_,mob2) -> masseMobile(mob1) +. masseMobile(mob2)
  | Figure(fig) -> masseFigure(fig);;

let equilibreLocal = function
  | Tige(mob1,a ,b, mob2) -> 
      (masseMobile mob1) *. float_of_int(a) = (masseMobile mob2) *. float_of_int(b)
  | Figure(fig) -> true;;

(*masseMobile traite deja de maniere recursive tout le mobile*)
let equilibreGlobal mobile = equilibreLocal mobile;;

let mobilesEquiv mobile1 mobile2 =
  equilibreGlobal mobile1 = equilibreGlobal mobile2;;


let mob1 = Tige(Figure(Sphere(1)) , 2, 2, Figure(Pyramide(1)));; 
let mob2 = Tige(Figure(Pyramide(1)) , 2, 3, mob1);;
let mob3 = Tige(mob2, 6, 4, Figure(Cube(1)));;
let mob4 = Tige(Figure(Pyramide(1)) , 2, 1, Figure(Cube(1)));;
let mob5 = Tige(mob4, 3, 1, Figure(Sphere(1)));;
let mob6 = Tige(Figure(Cube(1)) , 2, 5, mob5);; 
let m0 = Tige(mob3, 8, 7, mob6);;
