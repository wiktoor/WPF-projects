(* autor: Wiktor Chmielewski grupa 5 *)
(* code review: Przemysław Kusiak grupa 5*)

(* aktualny - aktualne poziomy wody*)
(* pozostaly - liczba szklanek z v.(i) <> g.(i) *)
type stan = { aktualny: int array; pozostaly: int ref };;

(* kopia aktualnego stanu *)
let akt_stan st = { aktualny = Array.copy st.aktualny; pozostaly = ref (!(st.pozostaly))};;

type specyfikacja = { v: int array; g: int array };;

(* zwraca możliwe stany po zrobieniu operacji w stanie st *)
let dozwolone_stany sp st = 
  let n = Array.length st.aktualny 
  and zmien_stan sp st i zmiana = 
    if st.aktualny.(i) = sp.g.(i) then incr st.pozostaly;
    st.aktualny.(i) <- st.aktualny.(i) + zmiana;
    if st.aktualny.(i) = sp.g.(i) then decr st.pozostaly;
    in
  (* stan, który otrzymamy poprzez wypełnienie i-tej szklanki *)
  let rec wypelnij i lst = 
    if i = n then lst 
    else
      let roznica = sp.v.(i) - st.aktualny.(i) in
      if roznica = 0 then wypelnij (i + 1) lst
      else
        let nowy_stan = akt_stan st in
        zmien_stan sp nowy_stan i roznica;
        wypelnij (i + 1) (nowy_stan::lst)
  (* stan, który otrzymamy poprzez opróżnienie i-tej szklanki *)
  and oproznij i lst = 
    if i = n then lst
    else 
      let roznica = - st.aktualny.(i) in
      if roznica = 0 then oproznij (i + 1) lst
      else
        let nowy_stan = akt_stan st in
        zmien_stan sp nowy_stan i roznica;
        oproznij (i + 1) (nowy_stan::lst)
  (* stan, który otrzymamy poprzez przelanie z i-tej szklanki do j-tej szklanki *)
  and przelej i j lst =
    if i = n then lst
    else if j = n then przelej (i + 1) 0 lst
    else if i = j then przelej i (j + 1) lst
    else
      let roznica = min st.aktualny.(i) (sp.v.(j) - st.aktualny.(j)) in
      if roznica = 0 then przelej i (j + 1) lst
      else 
        let nowy_stan = akt_stan st in
        zmien_stan sp nowy_stan i (-roznica);
        zmien_stan sp nowy_stan j roznica;
        przelej i (j + 1) (nowy_stan::lst)
  in przelej 0 0 (oproznij 0 (wypelnij 0 []))
;;

let przelewanka arr = 
  let rozwiaz sp st = 
    (* rozmiar tablicy hashującej: iloczyn objętości szklanek *)
    let s = Array.fold_left (fun a x -> a * x) 1 sp.v in
    (* tablica odwiedzonych *)
    let odw = Hashtbl.create (max 10000 (min 10000000 s)) in 
    let dist = fun v -> (try Hashtbl.find odw v with Not_found -> -1) in
    (* bfs po stanach *)
    let bfs sasiedzi start = 
      let q = Queue.create ()
      and cel st = !(st.pozostaly) = 0 in
      let x = ref (cel start)
      and odwiedz = fun v d -> Hashtbl.add odw v d in
      odwiedz start 0;
      Queue.push start q;
      while not (Queue.is_empty q) && not !x do
        let v = Queue.pop q in
        let d = dist v in
        let procesuj u = 
          if dist u = -1 then begin
            odwiedz u (d + 1);
            Queue.push u q;
            x := !x || cel u 
          end; in
          List.iter procesuj (sasiedzi v)
        done;
    in
    bfs (dozwolone_stany sp) st;
    dist ({aktualny = Array.copy sp.g ; pozostaly = ref 0})
  in
  let n = Array.length arr in
  (* czytamy specyfikację z arr *)
  let sp = { v = (Array.init n (fun i -> fst arr.(i))); g = (Array.init n (fun i -> snd arr.(i)))} in
  (* stan początkowy - wszystkie szklanki puste *)
  let st = { aktualny = Array.make n 0 ; pozostaly = ref (Array.fold_left (fun a x -> if x = 0 then a - 1 else a) n sp.g)} in
  rozwiaz sp st
;;