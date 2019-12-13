open Graph

type label_arc = { flow: int; capacity: int };;
type graph_ff = label_arc graph ;; 
type path = id list ;; 

val get_flow : int graph -> id -> id -> int

val string_of_label : label_arc -> string  

val find_path : int graph -> id list -> id -> id -> path option

val print_path: int -> path option -> unit

val find_flow: int graph -> id -> id list option -> int option

val increment: int graph -> int -> id -> id list option -> int graph

val ford_fulkerson : int graph -> string graph -> id list option -> id -> id ->  label_arc graph

val display_solution : label_arc graph -> id -> int