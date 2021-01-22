(* 
    Sortowanie Topologiczne
    Autor: Grzegorz B. Zaleski 
    Recenzja: Hubert Badocha 
*)

(** Wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(**  Pomocnicza funkcja przetwarzająca liste wierzchołków dowolnego typu (z możliwymi 
    powtórzeniami) na hashtablice (mapę) <wierzchołek, int> oraz tablice róznych wierzchołków grafu 
    Graf 'a zostanie przetworzony na graf 0, 1, ..., (n-1) *)
let scale_nodes nodes =
    let n = List.length nodes in
    let map = Hashtbl.create n in
    let i = ref 0 in
    let res = ref [] in
    let add v =
        try ignore (Hashtbl.find map v)
        with Not_found ->
            Hashtbl.add map v !i;
            res := v::(!res);
            i := !i + 1
    in
    List.iter add nodes;
    (Array.of_list (List.rev !res), map)

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lst = 
    let (_nodes, nodes_list) = List.split lst in
    let nodes_list_flat = List.flatten nodes_list in
    let (nodes, map) = scale_nodes (_nodes @ nodes_list_flat) in
    let res = ref [] in 
    let n = Array.length nodes in 
    let graph = Array.make n [] in
    (* Funkcja tworząca graf w strukturze array *)
    let rec add_adjs l = 
        match l with 
        | [] -> ()
        | (v, adjs)::t ->
            let hashed = Hashtbl.find map v in 
            graph.(hashed) <- adjs @ graph.(hashed);
            add_adjs t
    in
    add_adjs lst;
    let deg = Array.make n 0 in
    (* Funkcja zliczająca stopnie wchodzące każdego wierchołka *)
    let add_edge v = 
        let hashed = Hashtbl.find map v in
        deg.(hashed) <- deg.(hashed) + 1
    in
    List.iter add_edge nodes_list_flat;
    let queue = Queue.create () in
    for i = 0 to n-1 
    do
        if deg.(i) = 0 then
            Queue.add nodes.(i) queue
    done;
    (* Funkcja obniżająca stopień wszystkim sąsiadom przerobionego wierzchołka *)
    let rec proceed l =
        match l with
        | [] -> ()
        | v::t ->
        let hashed = Hashtbl.find map v in
        proceed t;
        deg.(hashed) <- deg.(hashed) - 1;
        if deg.(hashed) = 0 then 
            Queue.add v queue
    in
    while Queue.is_empty queue = false 
    do
        let v = Queue.take queue in
        let hashed = Hashtbl.find map v in 
        res := v::(!res);
        proceed (graph.(hashed))
    done; 
    if List.length !res = n then 
        List.rev !res
    else
        raise Cykliczne    
    
    
