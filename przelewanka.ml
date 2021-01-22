(*
    Autor: Grzegorz B. Zaleski
    Review: Bartosz Ruszewszki
    Warszawa, 12-15 stycznia 2020
*)

(* Zbiór wszystkich sprawdzonych stanów grafu *)
let guard = Hashtbl.create 64
(* Kolejka kolejnych stanów do sprawdzenia *)
let bfs = Queue.create ()
(* Odpowiedź do zadania *)
let answer = ref (-1)

(* Pojedynczy krok przeszukania *)
let analyse cur_state t volume final n = 
    if !answer <> -1 then ();    
    try ignore (Hashtbl.find guard cur_state)
    with Not_found -> 
    begin
        Hashtbl.add guard cur_state (Hashtbl.length guard);    
        
        (* Sprawdzenie czy aktualny stan to wynik *)
        let fine = ref true in
        for i = 0 to n-1 do
            if cur_state.(i) <> final.(i) then
                fine := false
        done;    
        if !fine == true then 
        begin
            answer := t;
            ()                
        end;
        
        let time = t + 1 in
        let backup = Array.copy cur_state in
        for i = 0 to n-1 do 
            if cur_state.(i) <> 0 then (* Wylanie wody z i-tej szklanki *)
            begin
                cur_state.(i) <- 0;
                Queue.add (Array.copy cur_state, time) bfs;
                cur_state.(i) <- backup.(i)
            end;
            
            if cur_state.(i) <> volume.(i) then (* Napełnienie i-tej szklanki *)
            begin
                cur_state.(i) <- volume.(i);
                Queue.add (Array.copy cur_state, time) bfs;
                cur_state.(i) <- backup.(i)
            end;
            
            if backup.(i) <> 0 then
                for j = 0 to n-1 do (* Przelanie z i-tej szklanki do j-tej *)
                    if i <> j && cur_state.(j) < volume.(j) then
                    begin
                        if cur_state.(i) + cur_state.(j) <= volume.(j) then
                        begin
                            cur_state.(j) <- cur_state.(j) + cur_state.(i);
                            cur_state.(i) <- 0
                        end
                        else
                        begin
                            cur_state.(i) <- cur_state.(i) + cur_state.(j) - volume.(j);
                            cur_state.(j) <- volume.(j)
                        end;                        
                        Queue.add (Array.copy cur_state, time) bfs;
                        cur_state.(i) <- backup.(i);
                        cur_state.(j) <- backup.(j);
                    end;
                done;
        done;
    end;
    ()

(* Funkcja licząca NWD dwóch liczb *)
let rec gcd a b =
    if a = 0 then b
    else gcd (b mod a) a

(* Sprawdzanie warunku koniecznego - jedna szklanka na koncu musi byc pusta lub pełna *)
let one_possible (vol, fin) = 
    fin = 0 || vol = fin
    
(* Procedura która wyznacza minimalną liczbę czynności potrzebnych do uzyskania opisanej 
sytuacji. Jeżeli jej uzyskanie nie jest możliwe, to wynikiem jest -1. *)
let _przelewanka tab = 
    let n = Array.length tab
    and volume = Array.map fst tab
    and final = Array.map snd tab in
    let div = Array.fold_left gcd 0 volume in 
    if div <> 0 then 
        for i = 0 to n-1 do
            if final.(i) mod div <> 0 then answer := (-2)
        done
    else answer := 0;    
       
    if Array.exists one_possible tab then
    begin
        Queue.add (Array.make n 0, 0) bfs;
        while not (Queue.is_empty bfs) && !answer = (-1) do
            let front = Queue.take bfs in
            analyse (fst front) (snd front) volume final n
        done;
    end;
    !answer 

(* Funkcja pomocnicza analizująca przypadek skrajny (corner case) i zwalniająca pamięć *)
let przelewanka tab = 
    match tab with 
    | [||] -> 0
    | _ ->  
        let ans = _przelewanka tab in
        answer := (-1);
        Queue.clear bfs;
        Hashtbl.reset guard;
        max ans (-1)
      
