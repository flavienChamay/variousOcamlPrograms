(* Partie A - Fonctions et n-uplets *)
let e1 = ((4, true), 4);;
let e2 = ((fun x -> x),'w');;

let f1 i b =
  (i+0, b && true);;

let f2 a =
  true;;

let f3  a b = 	
  3;;

let f4 (a,b) =
  (b, a);;

let f5 fu1 fu2 fu3 d e =
  let (b, c) = fu1 d in
  let d = fu2 b c in
  let f = fu3 (d, true) in
  f;;

let f6 pf1 pf2 a = 
  let b = pf1 a in
  let c = pf2 a b in
  c;;

let f7 (fu1 , fu2, a)  =
  let b = fu1 a in
  let c = fu2 (a,b) in
  c;;

let f8 p1 p2 p3 p4 = 
  let var = if (p1 = p2) then
      p3
    else
      p4
  in (p1, var);;

let cube a =
  a * a * a;;

let quad a =
  4 * a;;

let estPair a =
  a mod 2 = 0;;

let max3 n1 n2 n3 =
  if n1 >= n2 && n1 >= n3 then
    n1
  else if n2 >= n1 && n2 >= n3 then
    n2
  else
    n3;;

let discriminant a b c = 
  b * b - 4 * a * c;;

let estImpair a = 
  not (estPair a);; 

let choix a =
  if estPair a then
    cube a
  else
    quad a;;

let nbRac a b c =
  let delta = discriminant a b c in
  if delta = 0 then
    1
  else if delta > 0 then
    2
  else 
    0;;

let xor a b =
  (a || b) && not (a && b);;

let nor a b =
  not (a || b);;

let nand a b =
  not (a && b);;


(* Partie B - Récursion *)
let dernierCh entier =
  entier mod 10;;

let toutSaufDer entier =
  entier/10;;

let sommeChiffres entier =
  let rec calcul = function
    | 0 -> 0
    | n -> dernierCh n + calcul (toutSaufDer n) in 
  if entier < 0 then
    failwith "Erreur: argument doit être positif ou nul"
  else calcul entier;;

let rec sommeIteree entier =
  if entier < 0 then
    failwith "Erreur: argument doit etre positif ou nul"
  else if entier < 10 then 
    entier 
  else
    sommeIteree (sommeChiffres entier);;

let rec premierCh entier=
  if entier < 10 then
    entier
  else
    premierCh (toutSaufDer entier);;

let rec toutSaufPrem entier =
  if entier < 10 then
    0
  else
    toutSaufPrem (toutSaufDer entier) * 10  + dernierCh entier;;

let rec estPalindrome entier =
  (entier < 10) || (premierCh entier = dernierCh entier) && estPalindrome (toutSaufPrem (toutSaufDer entier));;
  
let rec nbOcc chiffre entier =
  if entier < 10 then
    if entier = chiffre then 1 else 0
  else if dernierCh entier =  chiffre then 
    1 + nbOcc chiffre (toutSaufDer entier)
  else 
    0 + nbOcc chiffre (toutSaufDer entier);;

let rec iterer n f x =
  match n with
  | 0 -> x
  | n -> f (iterer (n-1) f x );;

let id param =
  param;;

let compose f1 f2 p1 =
  f1 (f2 p1);;

let rec iterer2 n f =
  match n with
  | 0 -> id
  | n -> compose f (iterer2 (n-1) f);;

let rec itererBis f p x =
  if p x then
    x
  else 
    itererBis f p (f x);;

let sommeIteree2 entier =
  itererBis sommeChiffres (fun entier -> entier < 10) entier;;

let rec qqsoit n p =
  match n with
  | 0 -> true
  | n -> p n && qqsoit (n-1) p;;

let rec ack = function
  | (0,n) -> n + 1
  | (m,0) -> ack ((m-1), 1)
  | (m, n) -> ack (m-1, ack (m, n-1));;
  
    

