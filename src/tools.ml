open Graph

let clone_nodes gr = n_fold gr new_node empty_graph ;;



let gmap gr f = 
  let f_1 graphe id1 id2 label = 
    new_arc graphe id1 id2 (f label) in
  e_fold gr f_1 (clone_nodes gr) ;;


let add_arc gr id1 id2 n = match find_arc gr id1 id2 with
  | Some x ->  new_arc gr id1 id2 (x + n) 
  | None -> new_arc gr id1 id2 n;;
