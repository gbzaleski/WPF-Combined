(* 
        Origami
    Author: Grzegorz B. Zaleski
    Review: Kamil Zwierzchowski
    30 listopada 2019, Warszawa
*)

(* Punkt na płaszczyźnie *)
type point = float * float

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* Stała niedokładności przy porównaniach *)
let eps = 1e-9

(* Moduł z liczby zmiennoprzecinkowej *)
let abs_fl (x : float) = 
    max x (-. x)

(* Kwadrat liczby x*)
let sq x =
    x *. x
    
(* Kwadrat 0dległosc między punktami (ax, ay) (bx, by) *)
let dist (ax, ay) (bx, by) = 
    sq (ax -. bx) +. sq (ay -. by)

(* Funkcja sprawdzająca po której stronie wektora (x1, y1) (x2, y2) leży punkt (x3, y3) 
    1 - po lewej, 0 - na prostej wyznaczonej przez wektor, -1 - po prawej*)
let orient (x1, y1) (x2, y2) (x3, y3) =
    let v = (y2 -. y1) *. (x3 -. x2) -. (y3 -. y2) *. (x2 -. x1) in
        if abs_fl v <= eps then 0
        else if eps < v then -1
        else 1

(* Funkcja tworzaca postac kierunkowa prostej (y = ax + b) przechodzacej przez punkty (ax, ay) (bx, by) *)
let get_line (ax, ay) (bx, by) = 
    if ax <> bx then
        let a = (ay -. by) /. (ax -. bx) in
        let b = ay -. a *. ax in
        (a, b)
    else (max_float, ax)

(* Funkcja odbijająca punkt (x,y) wzgledem prostej y = mx + b *)
let _mirrorise (x, y) (m, b) = 
    if m <> max_float then 
        let low = sq m +. 1.0 in
            (((1.0 -. sq m) *. x +. 2.0 *. m *. y -. 2.0 *. m *. b) /. low, 
            ((sq m -. 1.0) *. y +. 2.0 *. m *. x +. 2.0 *. b) /. low)
    else 
        let d = b -. x in 
        (x +. d +. d, y)  
 
(* Odbicie punktu p wzgledem prostej przechodzacej przez punkty (ax, ay) (bx, by) *)
let mirrorise p (ax, ay) (bx, by) = 
    let line_arc = get_line (ax, ay) (bx, by) in 
    _mirrorise p line_arc

(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
    prostokąt o bokach równoległych do osi układu współrzędnych i lewym
    dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
    nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
    szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
    przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka =
    fun (px, py) ->
        if x1 -. px <= eps && px -. x2 <= eps && y1 -. py <= eps && py -. y2 <= eps then 1
        else 0

(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
    w punkcie [p] i promieniu [r] *)
let kolko (p : point) r : kartka =
    fun x ->
        if (dist p x) -. sq r <= eps then 1
        else 0

(* [zloz a b sh] składa kartkę [sh] wzdłuż prostej przechodzącej
    przez punkty [a] i [b] (muszą to być różne punkty). Papier jest
    składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
    od [a] do [b]) jest przekładany na lewą. Wynikiem funkcji jest
    złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
    zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo, 
    co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
    złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
    się na punkt przebicia. *)
let zloz (a : point) (b : point) (sh : kartka) =
    fun p ->
        let cur_orient = orient a b p in
            if cur_orient = 0 then sh p (* punkt znajduje się na zgięciu *)
            else if cur_orient = 1 then sh p + sh (mirrorise p a b) (* po lewej *)
            else 0 (* po prawej *)

(* [skladaj [(p1_1, p2_1);...;(p1_n, p2_n)] sh = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 sh)...)] 
    czyli wynikiem jest złożenie kartki [sh] kolejno wzdłuż wszystkich prostych 
    z listy *)
let skladaj lst sh = 
    let _zloz sh (a, b) = 
        zloz a b sh in
    List.fold_left _zloz sh lst  
    
