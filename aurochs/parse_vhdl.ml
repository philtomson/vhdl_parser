open Aurochs_pack;;
open Peg;;
open Vhdl;;

let fp = Printf.fprintf ;;
let pf = Printf.printf ;;
(*** print_indent *)
let print_indent ?(oc=stdout) d =
  for i = 1 to d do
    fp oc "  "
  done


type interface = { signame: string; mode: string; sigtype: string } ;;
type generic   = { mutable name: string; mutable g_type: string; mutable value:
  string option } ;;

let print_interface i = 
  (Printf.printf "signame: %s, mode: %s, type: %s" i.signame i.mode i.sigtype)
;;

(*
let rec extract_port_decl p = 
  
  let rec aux_port_decl pn  = match pn with
  | Node(N_Root, _, [x]) ->  Printf.printf "Root\n"; aux_port_decl x 
  | Node(N_ISigDecl, [(A_signames,sigs);(A_direction,dir);(A_type,stype)],_) ->
    (* List.iter (fun io -> (print_interface { signame=io; * mode=dir;sigtype=stype}) ) sigs *)
      Printf.printf "sigs is: %s\n" sigs
  | Node(N_Port_decl, _, xs) ->   (* xs is a list of interfaces *)
      Printf.printf "Port_decl:\n"; 
      List.iter ( fun i -> aux_port_decl i  ) xs
                                
  | Node(N_Port_decl, _, [])    -> Printf.printf "End Port_decl:\n"; 
  | _ -> Printf.printf "What's this?"  in

  aux_port_decl p  ;;
*)
    
let split rexp = Str.split (Str.regexp_string rexp) ;;
(*
let rec handle_generic gns::al = 
    List.iter ( fun (attrib,value)  -> 
                print_attribute_name stdout attrib;
                match attrib with
                  | A_gennames -> (
                    let generic_list = split "," value in
                    List.iter ( fun g -> Printf.printf " =>   %s\n" g ) generic_list
                  )
                  | A_type     -> Printf.printf " => %s\n" value 
                  | A_generic_value -> Printf.printf " => %s\n" value
                  | _ -> failwith "ILLEGAL ATTRIBUTE!\n"
              ) al; Printf.printf "</GenericDecl>\n"
*)

let rec eval = function
  | Node(N_Root, _, [x]) ->  Printf.fprintf stdout "<Root>\n"; eval x; Printf.printf "</Root>\n";
  | Node(N_Root, _, (_::_ as lst)) ->  Printf.fprintf stdout "<Root>\n"; 
    List.iter ( fun node -> eval node ) lst;
    Printf.fprintf stdout "</Root>\n"
  | Node(N_Entity_decl, [A_entity_name,ename], (_::_ as elist)) ->
      Printf.printf "<Entity: %s>\n" ename;
      List.iter ( fun node -> eval node ) elist;
      Printf.printf "</Entity: %s>\n" ename
  | Node(N_Dir_spec,  [_,z], [x]) -> (Printf.printf "Direction is: %s" z); eval x
  | Node(N_Dir_spec,  _, [x]) -> (Printf.printf "Direction is: " )
  | Node(N_GenericClause, _, (_::_ as lst)) ->
     pf "<GenericClause>\n";
     List.iter ( fun node -> eval node ) lst ;
     pf "</GenericClause>\n"
  | Node(N_GenericDecl,
         [(A_gennames,names);(A_type,gentype);(A_generic_value,value)], _) -> 
         let namelist = split "," names in
         List.iter ( fun s -> (pf " GenName: %s => Type: %s => Value: %s \n" s gentype value) ) namelist
  | Node(N_GenericDecl,
         [(A_gennames,names);(A_type,gentype)], _) -> 
         let namelist = split "," names in
         List.iter ( fun s -> (pf " GenName: %s => Type: %s => Value: ?? \n" s gentype ) ) namelist
  (* May have to bring this back later: 
  | Node(N_GenericDecl, al, _) -> Printf.printf "<GenericDecl>\n";
    List.iter ( fun (attrib,value)  -> 
                print_attribute_name stdout attrib;
                match attrib with
                  | A_gennames -> (
                    let generic_list = split "," value in
                    List.iter ( fun g -> Printf.printf " =>   %s\n" g ) generic_list
                  )
                  | A_type     -> Printf.printf " => %s\n" value 
                  | A_generic_value -> Printf.printf " => %s\n" value
                  | _ -> failwith "ILLEGAL ATTRIBUTE!\n"
              ) al; Printf.printf "</GenericDecl>\n"
  *)
  (*
  | Node(N_Port_sig, [(A_name,z);(A_direction,dir)], x::xs) -> 
      Printf.printf "name: %s, dir: %s\n" z dir;
      eval x; eval (Node(N_Port_sig, [(A_name,z);(A_direction,dir)], xs))
  | Node(N_Port_sig, [(A_name,z);(A_direction,y)], _ ) -> Printf.printf "Name: %s, Dir: %s\n" z y
  | Node(N_Port_sig, _, _ ) -> Printf.printf "port_sig\n"
  *)
  | Node(N_ISigDecl, [(A_signames,z);(A_direction,y);(A_type,tt)], _ ) ->
      let siglist = split "," z in
      List.iter ( fun s -> Printf.printf " SigName: %s => Mode: %s => Type: %s \n" s y tt ) siglist
  | Node(N_ISigDecl, al, _ ) -> Printf.printf "ISigDecl : no specified fields\n"
  (*| Node(N_Port_decl,_, []) -> Printf.printf "Port_decl end\n"*)
  | Node(N_Port_decl, al, (_::_ as lst) ) -> Printf.printf "<Port_decl> \n";
     (* Printf.printf " => %s\n"  value;  )  al;*)
     List.iter ( fun node -> eval node ) lst ;
     Printf.printf "</Port_decl>\n";

  |Node( (_ as unode), _, _) ->  Printf.printf "What node is this? "; print_node_name stdout unode
  | _ -> Printf.printf "What's this?\n";;

let _ =
  let u = "--comment\nentity Blah is generic( genval,gen2: integer := 0; genv3: integer :=
    12; genv4: string ); port ( foo,bar: in bit; baz: out bit );" in
    let t = Aurochs.read ~grammar:(`Program Vhdl.program) ~text:(`String u) in
    eval t;;
    (*Printf.printf ">>> %d\n%!" x;*)

