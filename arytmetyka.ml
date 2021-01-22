(* Zadanie Arytmetyka, 19-23 października 2019, Warszawa *)
(* Autor: Grzegorz B. Zaleski *)
(* Recenzent: Antoni Koszowski *)

(* Struktura do zapisywania wartosci*)
type przedzial = float * float
type wartosc = przedzial * przedzial

(* Funkcja sprawdzajaca czy liczba x jest nanem *)
let check_nan x = 
    compare x nan = 0  

(* Funkcja wybierająca mniejszą liczbę *)
let smaller x y =
    if check_nan x || x > y then y
    else x

(* Funkcja wybierająca wiekszą liczbę *)
let greater x y =
    if check_nan x || x < y then y
    else x

(*    Funkcje dodatkowe  *)
(* Funkcja tworząca przedzial [x,y] *)
let make_przedzial x y : przedzial = (x, y)


(* Funkcja tworząca wartosc = podwojny przedzial ("Normalne" przedzialy beda zapisywane jako [a,b] U [nan, nan])*)
let make_wartosc xab yab : wartosc = (xab, yab)

(* Pusty przedzial do porownan i konstrukucji wartosci*)
let empty = make_przedzial nan nan 

(* Przedzial do oddzielania ujemnych/dodatnich przy mnozeniu/dzieleniu*)
let for_split = make_przedzial 0.0 0.0 
    
(* Funkcja obliczająca wartosc bezwgledna z liczby *)
let _abs x : float =
    greater x (0.0-.x)

(* Obliczanie najmniejszej wartosci z 4 liczb *)
let min4 x y w z = smaller (smaller x y) (smaller w z)

(* Obliczanie najwiekszej wartosci z 4 liczb *)
let max4 x y w z = greater (greater x y) (greater w z)

(* Obliczanie najmniejszej wartosci na przedziale *)
let min_val ((x,y) : przedzial) = smaller x y

(* Obliczanie najwiekszej wartosci na przedziale *)
let max_val ((x,y) : przedzial) = greater x y

(* Sprawdzanie czy przedzial pusty jest *)
let check_empty ((x,y) : przedzial) =
    if check_nan x && check_nan y then true
    else false 

(*     Konstruktory: float ( * float) -> przedzial *)

(* Tworzenie wartosci reprezentujacej zbior liczb [x;y] *)
let wartosc_od_do x y =
    make_wartosc (make_przedzial x y) empty

(* Tworzenie wartosci reprezentujacej zbior liczb x +/- p% *)
let wartosc_dokladnosc x p =
    let a = (x *. (100.0 -. p) /. 100.0) and b = (x *. (100.0 +. p) /. 100.0)
    in 
        if (greater a b) = b then wartosc_od_do a b
        else wartosc_od_do b a

(* Tworzenie wartosci reprezentujacej zbior liczb [x;x] - tylko liczby x *)        
let wartosc_dokladna x =
    wartosc_od_do x x
  
(*   Selektory: przedzial ( * float) -> float/bool *)

(* Sprawdzanie czy liczba fval nalezy do przedzialy [x;y] *)
let in_przedzial (x,y : przedzial) fval =
    if check_empty (make_przedzial x y) then false
    else if x > fval then false
    else if y < fval then false
    else true

(* Sprawdzanie czy liczba y nalezy do wartosci x*)
let in_wartosc x y = 
    let _in_wartosc ((w,z) : wartosc) fval =
         if in_przedzial w fval then true
         else if in_przedzial z fval then true
         else false
    in _in_wartosc x y

(* Wybieranie najmniejszej liczby z wartosci *)    
let min_wartosc ((x,y) : wartosc) = smaller (min_val x) (min_val y)

(* Wybieranie najmniejszej liczby z wartosci *) 
let max_wartosc ((x,y) : wartosc) = greater (max_val x) (max_val y)

(* Obliczanie sredniej wartosci wartosci *) 
let sr_wartosc x = (min_wartosc x +. max_wartosc x) /. 2.0
    
(*  Fukcje dodatkowe pod modyfikatory *)   
(* Suma (taka "normalna" z teorii zbiorow) zbiorow = przedzialow [x1,y1] U [x2, y2] U [a,b] *)
let mergePrzedzial (((x1, y1), (x2, y2)) : wartosc) ((a, b) : przedzial) = 
    if (check_nan a || check_nan b) then make_wartosc (make_przedzial x1 y1) (make_przedzial x2 y2)
    else if in_przedzial (x1,y1) a then make_wartosc ((min x1 a), (max y1 b)) (x2, y2)
    else if in_przedzial (x1,y1) b then make_wartosc ((min x1 a), (max y1 b)) (x2, y2)
    else make_wartosc (x1, y1) ((min x2 a), (max y2 b)) 

(* Suma zbiorowa czterech przedzialow *)
let merge4 x y w z = mergePrzedzial (mergePrzedzial(make_wartosc x y) w) z

(* Suma zbiorowa dwoch wartosci *)
let merge_wartosc ((x1,y1) : wartosc) ((x2,y2) : wartosc) = merge4 x1 y1 x2 y2

(* Suma zbiorowa czterech wartosc *)
let merge_wartosc4 x y w z = merge_wartosc (merge_wartosc (merge_wartosc x y) w) z

(* Pomocnicza funkcja wykonujaca operacje dodawania *)
let __plus ((x, y) : przedzial) ((w, z) : przedzial) =
  make_przedzial (min4 (x +. w) (x +. z) (y +. w) (y +. z)) (max4 (x +. w) (x +. z) (y +. z) (y +. z))

(* Pomocnicza funkcja wykonujaca operacje odejmowania *)
let __minus ((x, y) : przedzial) ((w, z) : przedzial) =
  make_przedzial (min4 (x -. w) (x -. z) (y -. w) (y -. z)) (max4 (x -. w) (x -. z) (y -. w) (y -. z))
  
(* Funkcja tnaca przedzial przy zerze na wartosc [x;0] U [0;y] jesli 0 nalezy do przedzialu [x;y] *)
let split_zero ((x, y) : przedzial) =
    if (x, y) = for_split then make_wartosc for_split for_split
    else if (x < 0.0 && 0.0 < y) then  make_wartosc (make_przedzial x (-0.0)) (make_przedzial 0.0 y)
    else if (x < 0.0 && y = 0.0) then  make_wartosc (make_przedzial x (-0.0)) empty
    else make_wartosc (make_przedzial x y) empty

(* Pomocnicza funkcja wykonujaca operacje dzielenia *)    
let _podziel ((a, b) : przedzial) ((c, d) : przedzial) =
    if (c, d) = for_split then 
        make_wartosc empty empty
    else 
        let (x1, y1) = split_zero (a, b) and (x2, y2) = split_zero (c, d) and 
        __podziel (a, b) (c, d) = 
        make_przedzial (min4 (a /. c) (a /. d) (b /. c) (b /. d))  (max4 (a /. c) (a /. d) (b /. c) (b /. d))
    in merge4 (__podziel x1 x2) (__podziel x1 y2) (__podziel y1 x2) (__podziel y1 y2)

(* Pomocnicza funkcja wykonujaca operacje mnozenia *)
let _razy ((a, b) : przedzial) ((c, d) : przedzial) =
    if (c, d) = for_split then 
        make_wartosc for_split empty
    else 
        let (x1, y1) = split_zero (a, b) and (x2, y2) = split_zero (c, d) and 
        __razy (a, b) (c, d) = 
        make_przedzial (min4 (a *. c) (a *. d) (b *. c) (b *. d))  (max4 (a *. c) (a *. d) (b *. c) (b *. d))
    in merge4 (__razy x1 x2) (__razy x1 y2) (__razy y1 x2) (__razy y1 y2)   

(*  Modifikatory: przedzial * przedzial -> przedzial *) 

(* Operacja arytmetycznego dodania dwoch wartosci *)
let plus a b =
    let _plus ((x1,y1) : wartosc) ((x2,y2) : wartosc) = 
        merge4 (__plus x1 x2) (__plus x1 y2) (__plus y1 x2) (__plus y1 y2)
    in _plus a b
    
(* Operacja arytmetycznego odjecia dwoch wartosci *)
let minus a b =
    let _minus ((x1,y1) : wartosc) ((x2,y2) : wartosc) = 
        merge4 (__minus x1 x2) (__minus x1 y2) (__minus y1 x2) (__minus y1 y2)
    in _minus a b

(* Operacja arytmetycznego podzielenia wartosci przez druga wartosc *)
let podzielic a b =
    let _podziel ((x1, y1) : wartosc) ((x2, y2) : wartosc) = 
        merge_wartosc4 (_podziel x1 x2) (_podziel x1 y2) (_podziel y1 x2) (_podziel y1 y2)
    in _podziel a b
    
(* Operacja arytmetycznego przemnozenia wartosci przez druga wartosc *)
let razy a b = 
    let _razy ((x1, y1) : wartosc) ((x2, y2) : wartosc) =
        merge_wartosc4 (_razy x1 x2) (_razy x1 y2) (_razy y1 x2) (_razy y1 y2)
    in _razy a b
    
