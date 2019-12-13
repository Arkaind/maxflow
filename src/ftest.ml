open Gfile
open Tools
open Fordfulkerson 
open Printf

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let _infile = Sys.argv.(1)
  and _outfile = Sys.argv.(4)

  (* Definition of source and target *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in
  (* Open file *)
  let graph = from_file _infile in

  (*Map all arcs of the graph*)
  let _int_graph = gmap graph int_of_string  in 

  (* START of the algorithm*)
  let path = (find_path _int_graph [] _source _sink) in

  printf "\n Settings: \n";
  printf "  Graph : %s\n" _infile;
  printf "  Source : %d\n" _source;
  printf "  Target : %d\n \n" _sink;

  printf "--------Start Maximum Flow Problem---------\n";

  let graph_fulkerson = ford_fulkerson _int_graph graph path _source _sink in
  let ford_fulkerson_solution = gmap graph_fulkerson string_of_label in

  printf "--------End Maximum Flow Problem---------\n\n";
  (* END of algorithm*)

  (* Write and Export file*)
  let () = write_file _outfile ford_fulkerson_solution in
  let() = export (_outfile^".dot") ford_fulkerson_solution in
  ()
