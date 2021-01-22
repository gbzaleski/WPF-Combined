(* Kolejka priorytetowa na drzewach lewicowych - 1-4 listopada 2019, Wrocław/Warszawa *)
(* Autor: Grzegorz B. Zaleski *)
(* Recenzent: Juliusz Korab-Karpowicz *)

(* Typ złączalnej kolejki priorytetowej *) 
type 'a queue = 
    Node of
    {
        value: 'a;
        left: 'a queue;
        right: 'a queue;
        dist: int;
    }
    | Null

(* Pusta kolejka priorytetowa *)     
let empty = Null

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *) 
exception Empty

(* Pomocnicza funkcja tworzaca drzewo lewicowe *)
let make_queue value left right dist = Node({value = value; left = left; right = right; dist = dist}) 

(* Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *) 
let is_empty q =
    q = Null  
    
(* Pomocnicza funkcja zwracajaca odleglosc do prawego liscia *)
let get_dist q =
  match q with
    | Null -> 0
    | Node x -> x.dist  

(* Pomocnicza funkcja podczepiająca podrzewa zgodnie z zasada lewicowosci *)    
let rec _join left_tree value right_tree =
    if get_dist right_tree > get_dist left_tree then
        _join right_tree value left_tree
    else 
        make_queue value left_tree right_tree (get_dist right_tree + 1)

(* [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *) 
let rec join q1 q2 = 
    match q1, q2 with 
    | Null, _ -> q2
    | _, Null -> q1
    | Node x, Node y ->
        if x.value > y.value then
            join q2 q1
        else 
            _join x.left x.value (join q2 x.right)
    
(* [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *)
let add e q =
    join (make_queue e Null Null 1) q

(* Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] 
gdzie [e] jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e]. 
Jeśli [q] jest puste podnosi wyjątek [Empty]. *) 
let delete_min q = 
    match q with
    | Null -> raise Empty
    | Node x -> (x.value, join x.left x.right)
    
