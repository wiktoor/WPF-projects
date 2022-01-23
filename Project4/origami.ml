(*autor: Wiktor Chmielewski grupa 5*)
(*code review: Maurycy Wojda grupa 5*)
type point = float * float;;
type kartka = point -> int;;
let eps = Float.epsilon *. 1000000.;;

let square x = x *. x;;
(*dlugosc wektora (x,y)*)
let dlugosc ((x,y) : point) = sqrt(square x +. square y);;
(*wektor v1 + v2*)
let dodaj ((x1, y1) : point) ((x2, y2) : point) : point = (x1 +. x2, y1 +. y2);;
(*wektor przeciwny do wektora v*)
let przeciwny ((x, y) : point) : point = (-. x, -. y);;
(*wektor v1 - v2*)
let odejmij v1 v2 : point = dodaj v1 (przeciwny v2);;
(*wektor v pomnozony przez skalar k*)
let skalar ((x, y) : point) k : point = (x *. k, y *. k);;
let odwrotnosc k = 
  if k = 0. then failwith "dzielenie przez 0"
  else 1. /. k
;;
(*wektor v pomnozony przez skalar 1/k*)
let skalardzielenie ((x, y) : point) k : point = skalar (x, y) (odwrotnosc k);;

let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka = fun ((x,y) : point) -> 
  if x1 <= x +. eps && x <= x2 +. eps && y1 <= y +. eps && y <= y2 +. eps then 1 else 0
;;

let kolko (o : point) r : kartka = fun (v : point) ->
  if (dlugosc (odejmij o v)) <= r +. eps then 1 else 0
;;

(*iloczyn wektorowy wektorow v1 - v, v2 - v*)
(*v = (x,y) x1 = (x1, y1) v2 = (x2, y2)*)
let iwek ((x, y) : point) ((x1, y1) : point) ((x2, y2) : point) = (x1 -. x) *. (y2 -. y) -. (x2 -. x) *. (y1 -. y);;

(*iloczyn wektorowy wektorow v1 i v2*)
(*v1 = (x1, y1) v2 = (x2, y2)*)
let iska ((x1, y1) : point) ((x2, y2) : point) = (x1 *. x2) +. (y1 *. y2);;

(*zwraca v' : punkt symetryczny do v wzgl prostej v1 v2*)
let sym v v1 v2 =
  (*pomocniczy wektor jednostkowy v1 v2*)
  let jednostkowy = skalardzielenie (odejmij v2 v1) (dlugosc (odejmij v2 v1)) in
  (*iloczyn skalarny wektorow v1 p oraz jednostkowego*)
  let is = iska (odejmij v v1) jednostkowy in
  odejmij (skalar (dodaj v1 (skalar jednostkowy is)) 2.) v 
;;

let zloz (v1 : point) (v2 : point) (k : kartka) : kartka = fun (v : point) ->
  (*iloczyn wektorowy v2 - v1 oraz v - v1*)
  (*sprawdza po której stronie prostej v1 v2 jest v*)
  let iw = iwek v1 v2 v in 
  if Float.abs iw <= eps then k v 
  else if iw > 0. then ((k v) + (k (sym v v1 v2)))
  else 0
;;

let skladaj (lst : (point * point) list) (k : kartka) : kartka = List.fold_left (fun acc (v1, v2) -> zloz v1 v2 acc) k lst;; 