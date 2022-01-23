(*autor: Wiktor Chmielewski grupa 5*)
(*review: Daniel Mastalerz grupa 5*)
open Float;;
(*W rozwiązaniu zadania będziemy korzystać z faktu, że każdy z naszych modyfikatorów zmienia dany przedział na inny przedział*)
(*lub też jego dopełnienie, tj. zbiór R-przedział*)
type wartosc = Przedzial of (float*float) | Dopelnienie of (float*float) | Pusty;;

(*Funkcje pomocniczne*)
let min x y = 
  match (x,y) with
  | (a,_) when a=neg_infinity -> neg_infinity
  | (_,a) when a=neg_infinity -> neg_infinity
  | (_,_) -> if x<y then x else y
;;

let max x y = 
  match (x,y) with
  | (a,_) when a=infinity -> infinity
  | (_,a) when a=infinity -> infinity
  | (_,_) -> if x>y then x else y
;;

(*Zwraca zbiór elementów e, takich ze e nalezy do x i e nalezy do y*)
(*Korzystając z naszej pierwszej obserwacji, nie musimy rozważać przypadku, w którym otrzymamy sumę więcej niż dwóch rozłącznych przedziałów*)
let rec suma x y = 
  match (x,y) with
  | (Przedzial (a,b), Przedzial (c,d)) ->
    if (b>d) then suma (Przedzial(c,d)) (Przedzial(a,b))
    else if (b>=c) then
      if (a<=c) then (Przedzial (a,d))
      else (Przedzial (c,d))
    else (Dopelnienie(b,c)) 
  | (Przedzial(a,b), Dopelnienie(c,d)) ->
    if (a<=c)&&(b>=d) then Przedzial (neg_infinity, infinity)
    else if (a<=c) then Dopelnienie (b,d)
    else if (b>=d) then Dopelnienie (a,c)
    else Dopelnienie (c,d)
  | (Dopelnienie(a,b), Przedzial(c,d)) -> (suma (Przedzial(c,d)) (Dopelnienie(a,b)))
  | (Dopelnienie(a,b), Dopelnienie(c,d)) ->
    if (c>=b)||(d<=a) then Przedzial(neg_infinity,infinity)
    else if (a>c)&&(b<d) then Dopelnienie(a,b)
    else if (a>c) then Dopelnienie(a,d)
    else if (b<d) then Dopelnienie(c,b)
    else Dopelnienie(c,d)
  | (_,_) -> Pusty
;;

(*Konstruktory*)
let wartosc_dokladnosc x p =
  if x>=0. then Przedzial ((x *. (1. -. (p /. 100.))),(x *. (1. +. (p /. 100.))))
  else Przedzial ((x *. (1. +. (p /. 100.))),(x *. (1. -. (p /. 100.))))
;;

let wartosc_od_do x y =
  if x<=y then Przedzial (x,y)
  else Przedzial (y,x)
;;

let wartosc_dokladna x = 
  Przedzial (x,x)
;;

(*Selektory*)
let in_wartosc w x =
  match w with
  | Przedzial (a,b) -> (a<=x && x<=b)
  | Dopelnienie (a,b) -> (x<=a || b<=x)
  | Pusty -> false
;;

let min_wartosc x = 
  match x with
  | Przedzial (a,_) when a=neg_infinity -> neg_infinity
  | Przedzial (_,a) when a=neg_infinity -> neg_infinity
  | Przedzial (a,b) -> a
  | Dopelnienie (_,_) -> neg_infinity
  | Pusty -> nan
;;

let max_wartosc x =
  match x with
  | Przedzial (a,_) when a=infinity -> infinity
  | Przedzial (_,a) when a=infinity -> infinity
  | Przedzial (a,b) -> b
  | Dopelnienie (_,_) -> infinity 
  | Pusty -> nan
;;

let sr_wartosc x = 
  match x with
  | Przedzial (a,b) -> ((a +. b) /. 2.)
  | Dopelnienie (_,_) -> nan
  | Pusty -> nan
;;

(*Modyfikatory*)
let rec plus x y =
  match (x,y) with
  | ((Przedzial(a,b)),(Przedzial(c,d))) -> Przedzial(a +. c, b +. d)
  | ((Przedzial(a,b)),(Dopelnienie(c,d))) -> 
    if (b +. c)>=(a +. d) then Przedzial(neg_infinity, infinity)
    else Dopelnienie(b +. c, a +. d)
  | ((Dopelnienie(a,b)),(Przedzial(c,d))) -> plus (Przedzial(c,d)) (Dopelnienie(a,b))
  | ((Dopelnienie(a,b)),(Dopelnienie(c,d))) -> Przedzial(neg_infinity,infinity)
  | (Pusty,_) -> Pusty
  | (_,Pusty) -> Pusty
;;

let rec razy x y = 
  let mnozenie x y =
    match (x,y) with
    | (a,0.) when a=infinity -> infinity
    | (a,0.) when a=neg_infinity -> neg_infinity
    | (0.,a) when a=infinity -> infinity
    | (0.,a) when a=neg_infinity -> neg_infinity
    | (_,_) -> x *. y
  in
  match (x,y) with
  | (Pusty,_) -> Pusty
  | (_,Pusty) -> Pusty
  | (a,_) when a=Przedzial(0.,0.) -> wartosc_dokladna 0.
  | (_,a) when a=Przedzial(0.,0.) -> wartosc_dokladna 0.
  | (Przedzial (a,b), Przedzial (c,d)) -> 
    if (a=neg_infinity && b=infinity)||(c=neg_infinity && d=infinity) then Przedzial(neg_infinity, infinity)
    else if (a=neg_infinity && d=0.)||(b=0. && c=neg_infinity) then Przedzial(0.,infinity)
    else if (a<0. && b=0. && c>=0. && d=infinity)||(a>=0. && b=infinity && c<0. && d=0.) then Przedzial(neg_infinity,0.)
    else Przedzial(min (min (mnozenie a c) (mnozenie a d)) (min (mnozenie b c) (mnozenie b d)),max (max (mnozenie a c) (mnozenie a d)) (max (mnozenie b c) (mnozenie b d)))
  | (Przedzial (a,b), Dopelnienie (c,d)) -> suma (razy (Przedzial(a,b)) (Przedzial(neg_infinity,c))) (razy (Przedzial(a,b)) (Przedzial(d,infinity)))
  | (Dopelnienie (a,b), Przedzial(c,d)) -> razy (Przedzial(c,d)) (Dopelnienie(a,b))
  | (Dopelnienie (a,b), Dopelnienie(c,d)) -> 
    let przedzial1 = razy (Przedzial(neg_infinity,a)) (Przedzial(neg_infinity,c))
    and przedzial2 = razy (Przedzial(neg_infinity,a)) (Przedzial(d,infinity))
    and przedzial3 = razy (Przedzial(b,infinity)) (Przedzial(neg_infinity,c))
    and przedzial4 = razy (Przedzial(b,infinity)) (Przedzial(d,infinity))
    in suma (suma przedzial1 przedzial2) (suma przedzial3 przedzial4)
;;

let minus x y = plus x (razy (wartosc_dokladna (-.1.)) y);;

let rec podzielic x y = 
  match (x,y) with
  | (Pusty,_) -> Pusty
  | (_,Pusty) -> Pusty
  | (_,a) when a=(wartosc_dokladna 0.) -> Pusty
  | (Przedzial(a,b),Przedzial(c,d)) ->
    if (a=0.)&&(b=0.) then Przedzial(0.,0.)
    else if (a>=0.)&&(c>0.) then Przedzial(a /. d, b /. c)
    else if (a<0.)&&(b=0.)&&(c<0.)&&(d=0.) then Przedzial(0.,infinity)
    else if (a<0.)&&(b=0.)&&(c=0.) then Przedzial(neg_infinity,0.)
    else if (a=0.)&&(c<0.)&&(d=0.) then Przedzial(neg_infinity,0.)
    else if (a>=0.)&&(c<0.)&&(d=0.) then Przedzial(neg_infinity, a /. c)
    else if (a>=0.)&&(c=0.) then Przedzial(a /. d, infinity)
    else if (b<0.)&&(c<0.)&&(d=0.) then Przedzial(b /. c, infinity)
    else if (b<0.)&&(c=0.) then Przedzial(neg_infinity, b /. d)
    else if (b<0.)&&(d<0.) then Przedzial(b /. c, a /. d)
    else if (a>=0.)&&(d<0.) then Przedzial(b /. d, a /. c)
    else if (b<0.)&&(c>=0.) then Przedzial(a /. c, b /. d)
    else if (a>=0.)&&(c<0.)&&(d>=0.) then Dopelnienie(a /. c, a /. d)
    else if (a<0.)&&(b>=0.)&&(c>=0.) then Przedzial(a /. c, b /. c)
    else if (b<0.)&&(c<0.)&&(d>=0.) then Dopelnienie(b /. d, b /. c)
    else if (a<0.)&&(b>=0.)&&(d<0.) then Przedzial(b /. d, a /. d)
    else Przedzial(neg_infinity, infinity)
  | (Przedzial(a,b),Dopelnienie(c,d)) ->
    let przedzial1 = podzielic (Przedzial(a,b)) (Przedzial(neg_infinity,c))
    and przedzial2 = podzielic (Przedzial(a,b)) (Przedzial(d,infinity))
    in suma przedzial1 przedzial2
  | (Dopelnienie(a,b),Przedzial(c,d)) ->
    let przedzial1 = podzielic (Przedzial(neg_infinity,a)) (Przedzial(c,d))
    and przedzial2 = podzielic (Przedzial(b,infinity)) (Przedzial(c,d))
    in suma przedzial1 przedzial2
  | (Dopelnienie(a,b),Dopelnienie(c,d)) ->
    let przedzial1 = podzielic (Przedzial(neg_infinity,a)) (Przedzial(neg_infinity,c))
    and przedzial2 = podzielic (Przedzial(neg_infinity,a)) (Przedzial(d,infinity))
    and przedzial3 = podzielic (Przedzial(b,infinity)) (Przedzial(neg_infinity,c))
    and przedzial4 = podzielic (Przedzial(b,infinity)) (Przedzial(d,infinity))
    in suma (suma przedzial1 przedzial2) (suma przedzial3 przedzial4)
;;
