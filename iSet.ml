(*
    ISet - Interval sets
    Project basen on work of Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
    provided by University of Warsaw
    Copyright (C) 1996-2019
    
    Author: Grzegorz B. Zaleski
    Review: Dominik Wiśniewski
*)

(* Struktura przedzialu liczb całkowitych [x;y] *)
type interval = int * int

(* Struktura węzła w drzewie zawierająca pusty węzeł lub odpowiednio
    lewego syna - przedzial na węźle - prawego syna - odleglosc do najdalszego liscia - szerokość przedzialu (ilość liczb całkowitych na przedziale)
 Struktura spełnia załozenia:
    1. Drzew AVL - Różnica w wysokosci prawej i lewego podrzewa wynosi maksymalnie 2 
        (dokładnie to wartosci zmiennej ord) 
    2. Drzew BST - Wartosci w prawym podrzewie są większe niż na węźle, a wartosi na lewym są mniejsze. *)
type t =
    | Empty
    | Node of t * interval * t * int * int

(* The empty set *)
(* Pusty węzeł (niezawierający synów i przedziału) *)
let empty = Empty

(* Maksymalna roznica w wysokosci podrzew - warunek balansowania drzew *)
let ord = 2

(* Wysokosc drzewa *)
let height = function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

(* Liczby liczb całkowitych na przedziale w węźle *)
let get_width = function
    | Node (_, _, _, _, w) -> w
    | Empty -> 0

(* Funkcja obliczająca szerokość przedziału (ilość liczb całkowitych na nim) *)
let width (x, y) =
    if y - x < 0 then
        max_int
    else y - x + 1 
 
(* Returns true if the set is empty. *)
(* Funkcja sprawdzająca czy węzeł jest pusty *)
let is_empty x =
    x = Empty

(* Infixowe dodawanie liczb z zabepieczeniem na wypadek przekroczenia max_int *)
let (++) a b =
    if a + b < 0 then 
        max_int
    else 
        a + b

(* Tworzenie węzła zawierającego przedział k, lewego syna l i prawego syna r *)
let make l k r = 
    Node (l, k, r, max (height l) (height r) + 1, (width k) ++ (get_width l) ++ (get_width r))

(* Balansowanie drzewa tj. funkcja poprawia lewe i prawe poddrzewo tak by 
    roznica ich wysokosci wynosiła mniej niż stała ord *)
let rec bal l k r =
    let hl = height l and hr = height r in
    if hl > hr + ord
        then match l with
            | Node (ll, lk, lr, _, _) ->
                if height ll >= height lr
                    then make ll lk (bal lr k r)
                else 
                    (match lr with
                        | Node (lrl, lrv, lrr, _, _) ->
                            make (make ll lk lrl) lrv (bal lrr k r)
                        | Empty -> assert false)
            | Empty -> assert false
    else if hr > hl + ord
        then match r with
            | Node (rl, rv, rr, _, _) ->
                if height rr >= height rl
                    then make (bal l k rl) rv rr
                else 
                    (match rl with
                        | Node (rll, rlv, rlr, _, _) ->
                            make (bal l k rll) rlv (make rlr rv rr)
                        | Empty -> assert false)
            | Empty -> assert false
    else
        make l k r
        
(* Return the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order. *)
(* Zwraca lista wszystkich przedzialow na drzewie w porzadku rosnącym *)
let elements s =
    let rec loop acc s =
        match s with
            | Node (l, k, r, _, _) -> loop (k::(loop acc r)) l
            | Empty -> acc
    in loop [] s
    
(* [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
(* Sprawdza czy wartosc x znajduje sie w drzewie s *)
let mem x s =
    let rec loop s =
        match s with 
            | Node (l, (a, b), r, _, _) ->
                if a <= x && x <= b then
                    true
                else if x < a then 
                    loop l
                else
                    loop r
            | Empty -> false
    in loop s
    
(* [iter f s] applies [f] to all continuous intervals in the set [s].
    The intervals are passed to [f] in increasing order. *)
(* Funkcja wykonująca funkcje f na kazdym przedziale w drzewie w porzadku rosnacych*)
let iter f s =
    let rec loop = function
        | Empty -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r in
    loop s

(* Funkcja wykonująca funkcje f na drodze z korzenia do przedziału zawierającego x *)
let rec fold_path f acc x s =
    match s with
        | Node (l, (a, b), r, _, _) ->
            let res =
                if x < a then fold_path f acc x l
                else if x > b then fold_path f acc x r
                else acc
            in f res s
        | Empty -> f acc s

(* Funkcja zwracające pare - najmniejszy przedzial na s oraz s bez tego przedzialu *)
let remove_min s =
    fold_path (fun (v_acc, s_acc) s ->
        match s with
        | Node (l, k, r, _, _) ->
            if l = Empty then 
                (k, r)
            else 
                (v_acc, bal s_acc k r)
        | Empty -> (v_acc, s_acc)) ((min_int, min_int), Empty) min_int s
                            
(* Funkcja zwracające pare - największy przedzial na s oraz s bez tego przedzialu *)
let remove_max s =
    fold_path (fun (v_acc, s_acc) s ->
        match s with
        | Node (l, k, r, _, _) ->
            if r = Empty then 
                (k, l)
            else 
                (v_acc, bal l k s_acc)
        | Empty -> (v_acc, s_acc)) ((max_int, max_int), Empty) max_int s    

(* Funkcja pomocnicza do split i remove złączająca dwa drzewa w jedno *)
let join l r =
    match l, r with
        | Empty, _ -> r
        | _, Empty -> l
        | _ ->
            let (k, nr) = remove_min r in
            bal l k nr

(* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
(* Funkcja oblicza wartosc [(f xN ... (f x2 (f x1 a))...)] gdzie x1, ... xN to kolejne przedziały na drzewie s w porządku rosnących *)
let rec fold f s a =
    match s with
    | Node (l, k, r, _, _) -> fold f r (f k (fold f l a))
    | Empty -> a          

(* [below n s] returns the number of elements of [s] that are lesser
    or equal to [n]. If there are more than max_int such elemenrts, 
    the result should be max_int. *)
(* Funkcja zwraca ilosc elementów niewiększych niż n. Jeśli liczba takich elementów przekracza max_int, zwracana jest wartosc max_int *)
let below n s =
    fold_path (fun acc s ->
    match s with
        | Node (l, (x, y), _, _, _) ->
            if x <= n then 
                acc ++ (width (x, (min n y))) ++ (get_width l)
            else 
                acc
        | Empty -> acc) 0 n s

(* [split x s] returns a triple [(l, present, r)], where [l] is the set of elements of [s] that are strictly lesser than [x]; 
[r] is the set of elements of [s] that are strictly greater than [x]; [present] is [false] if [s] contains no element equal to [x], 
    or [true] if [s] contains an element equal to [x]. *)
(* Funkcja zwraca trójke (l, stan, r) taką że l jest drzewem przedziałow mniejszych niż x, r - większych a stan jest wartość stan jest [true] 
    jeśli wartosć x występuje w drzewie s, [false] jeśli nie. *)
let split x s =
    fold_path (fun (l_acc, is_val, r_acc) s ->
        match s with
            | Node (l, (a, b), r, _, _) ->
                let nl =
                    if a < x then 
                        bal l (a, min b (x - 1)) l_acc
                    else if a == x then 
                        join l l_acc
                    else 
                        l_acc
                and nr =
                if x < b then 
                    bal r_acc (max a (x + 1), b) r
                else if b == x then 
                    join r r_acc   
                else 
                    r_acc
                in
                if a <= x && x <= b then 
                    (nl, true, nr)
                else 
                    (nl, is_val, nr)
            | Empty -> (l_acc, is_val, r_acc)) (Empty, false, Empty) x s
            
(* [add (x, y) s] returns a set containing the same elements as [s], 
    plus all elements of the interval [[x, y]] including [x] and [y]. Assumes [x <= y]. *)
(* Dodaje przedział (x, y) do drzewa s *)
let add (x, y) s =
    let (l, _, tr) = split x s in
    let (_, _, r) = split y tr in
    let ((x, _), l) =
        if l != Empty && mem (x - 1) l then 
            remove_max l
        else 
            ((x, y), l)
    and ((_, y), r) =
        if r != Empty && mem (y + 1) r then 
            remove_min r
        else 
            ((x, y), r) in
    bal l (x, y) r

(* [remove (x, y) s] returns a set containing the same elements as [s], 
    except for all those which are included between [x] and [y]. Assumes [x <= y]. *)
(* Funkcja usuwa liczby z przedziału [x;y] z drzewa *)
let remove (x, y) s =
    let (l, _, tr) = split x s in
    let (_, _, r) = split y tr in
    join l r
               
