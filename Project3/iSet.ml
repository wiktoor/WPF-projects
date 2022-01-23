(*autor: Wiktor Chmielewski grupa 5*)
(*code review: w trakcie szukania:) *)

(* Wartości, które trzymamy w Node to opowiednio: *)
(* Lewe poddrzewo, wartość, prawe poddrzewo, wysokość, rozmiar*)
(* Gdzie rozmiar to liczba liczb całkowitych, które t zawiera*)
type t = 
  | Empty
  | Node of t * (int*int) * t * int * int
;;

let height = function
  | Node (_,_,_,h,_) -> h
  | Empty -> 0
;;

let size = function
  | Node (_,_,_,_,s) -> s
  | Empty -> 0
;;

(* tworzy niezbalansowany wierzchołek *)
let make l (p,k) r = 
  let pom1 a b =
    if a+b<0 then max_int
    else a+b 
  in
  let pom2 (a,b) = 
    if b-a+1<=0 then max_int
    else b-a+1
  in Node (l,(p,k),r, (max (height l) (height r))+1, pom1 (pom1 (size l) (size r)) (pom2 (p,k)))
;;

let rec bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll,lk,lr,_,_) ->
      if height ll >= height lr
        then make ll lk (bal lr k r)
      else 
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (bal lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) -> 
      if height rr >= height rl
        then make (bal l k rl) rk rr 
      else
        (match rl with
        | Node (rll, rlk, rlr, _, _) ->
          make (bal l k rll) rlk (make rlr rk rr)
        | Empty -> assert false)
    | Empty -> assert false  
  else make l k r 
;; 

(* wersja folda, która zaczyna w wierzchołku s *)
(* i kończy na wierzchołku z wartością <= n*)
let rec apply f acc n beg = 
  match beg with
  | Node (l, (p,k), r, _, _) ->
    if n<p then f (apply f acc n l) beg 
    else if n>k then f (apply f acc n r) beg
    else f acc beg
  | Empty -> f acc beg
;;

(*zwraca parę (mininalny element; drzewo bez minimalnego elementu)*)
let remove_min_elt beg = 
  apply (fun (acc1,acc2) beg ->
    match beg with
    | Empty -> (acc1,acc2)
    | Node (l,k,r,_,_) ->
      if l = Empty then (k,r)
      else (acc1, bal acc2 k r))
  ((min_int, min_int), Empty) min_int beg
;;

(*zwraca parę (maksymalny element; drzewo bez maksymalnego elementu)*)
let remove_max_elt beg = 
  apply (fun (acc1,acc2) beg ->
    match beg with
    | Empty -> (acc1,acc2)
    | Node (l,k,r,_,_) ->
      if r = Empty then (k,l)
      else (acc1, bal l k acc2))
  ((max_int,max_int),Empty) max_int beg
;;

let merge l r =
  match (l,r) with
  | (Empty,Empty) -> Empty
  | (_,Empty) -> l
  | (Empty,_) -> r 
  | _ -> 
    let (k,nr) = remove_min_elt r 
    in bal l k nr
;; 

let empty = Empty;;
let is_empty x = x = Empty;;

let split n beg = 
  (*acc1 trzyma elementy <= n*)
  (*acc2 trzyma czy beg zawiera n*)
  (*acc3 trzyma elementy >= n*)
  let f = fun (acc1,acc2,acc3) beg ->
    match beg with
    | Empty -> (acc1,acc2,acc3)
    | Node (l,(p,k),r,_,_) ->
      let newl =
        if p<n then bal l (p, min k (n-1)) acc1
        else if p=n then merge l acc1
        else acc1
      and newr = 
        if k>n then bal acc3 (max p (n+1), k) r
        else if k=n then merge r acc3
        else acc3 
      in 
      if (p<=n)&&(k>=n)
        then (newl,true,newr)
      else (newl,acc2,newr)
  in apply f (Empty,false,Empty) n beg
;;

let mem n s =
  apply (fun acc beg ->
    match beg with
    | Node (_,(p,k),_,_,_) ->
      if (p<=n)&&(n<=k) then true
      else acc
    | Empty -> false) false n s 
;;

(* dodawanie przedziału (p,k) polega na rozdzieleniu przedziałów na te <= p i te >= k i następnie złączeniu ich z (p,k) *)
let add (np, nk) beg = 
  let (l,_,rr) = split np beg in
  let (_,_,r) = split nk rr in
  let ((np,_), l) = 
    if (l!=Empty)&&(mem (np-1) l)
      then remove_max_elt l 
    else ((np,nk),l)
  and ((_,nk),r) = 
    if (r!=Empty)&&(mem (nk+1) r)
      then remove_min_elt r 
    else ((np,nk),r)
  in bal l (np,nk) r 
;;

(* usuwanie przedziału (p,k) polega na połączeniu przedziałów <= p i >= k*)
let remove (np,nk) beg = 
  let (l,_,rr) = split np beg in
  let (_,_,r) = split nk rr in
  merge l r 
;;

let rec iter f beg = 
  match beg with
  | Node (l,k,r,_,_) -> 
    iter f l;
    f k;
    iter f r
  | Empty -> ()
;;

let rec fold f beg acc = 
  match beg with
  | Empty -> acc
  | Node (l,k,r,_,_) -> fold f r (f k (fold f l acc))
;;

let elements s = 
  let rec loop acc s = 
    match s with
    | Node (l,k,r,_,_) -> loop (k::(loop acc r)) l
    | Empty -> acc
  in loop [] s
;;

let below n s = 
  let pom1 a b =
    if a+b<=0 then max_int
    else a+b 
  and pom2 (a,b) = 
    if b-a+1<0 then max_int
    else b-a+1
  in
  apply (fun acc beg ->
    match beg with
    | Node (l, (p,k),_,_,_) ->
      if n>=p 
        then pom1 acc (pom1 (pom2 (p, (min n k))) (size l))
      else acc
    | Empty -> acc) 0 n s
;;  