open Aurochs_pack;;
open Peg;;
open Vhdl;;

let join lst sep = 
    let rec join' lst acc = match lst with
      | []          -> acc
      | [x]         -> (acc ^ x)
      | x::xs       -> join' xs (acc ^ x ^ sep) in
    join' lst "" ;;
let fp = Printf.fprintf ;;
let pf = Printf.printf ;;
let sp = Printf.sprintf ;;
(*** print_indent *)
let print_indent ?(oc=stdout) d str =
  fp oc "%s" ((String.make (d*2) ' ' ) ^ str) ;;


type namespace = { package: string option; entity: string option } ;;
type interface = { signame: string; mode: string; sigtype: string } ;;
type generic   = { name: string; g_type: string; mutable value:
  string option } ;;

let print_interface i = 
  (Printf.printf "signame: %s, mode: %s, type: %s" i.signame i.mode i.sigtype)
;;

(*
let rec extract_port_decl p = 
  
  let rec aux_port_decl pn  = match pn with
  | Node(N_Root, _, [x]) ->  Printf.printf "Root\n"; aux_port_decl x 
  | Node(N_ISigDecl, [(A_signames,sigs);(A_mode,dir);(A_type,stype)],_) ->
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

let rec eval depth tree path symtbl  = 
  
 let rec reduce n  = match n with
  | Node(N_Simple_term, _, [x]) -> reduce x
  | Node(N_Add, _, [x;y]) ->  ((reduce x ) + (reduce y ))
  | Node(N_Sub, _, [x;y]) ->  ((reduce x ) - (reduce y ))
  | Node(N_Mul, _, [x;y]) ->  ((reduce x ) * (reduce y ))
  | Node(N_Neg, _, [x]) -> ( - (reduce x ))
  | Node(N_Number, [A_value,x],[]) -> int_of_string x 
  | Node(N_Var, [A_varname,nm],[]) -> (* lookup variable value *)
      (
         let ent_symtab = Hashtbl.find symtbl path in
         match (try Hashtbl.find ent_symtab nm with Not_found -> None) with
         | None -> failwith  (Printf.sprintf "Undefined variable %s\n" nm)
         | Some x -> (int_of_string x ) 
      ) 
  |Node( (_ as unode), _, _) ->  Printf.printf "What node is this? "; print_node_name stdout unode; 
                                 failwith "\n";
  | _ -> failwith "ILLEGAL NODE TYPE FOR ARITH EXPRESSION" in

  
  match tree with

    Node(N_Root, _, [x]) ->  Printf.fprintf stdout "<Root>\n"; eval (depth-1) x path symtbl; 
      print_indent depth "}\n";
  | Node(N_Root, _, (_::_ as lst)) ->  
      print_indent depth "<Design>\n" ; 
      List.iter ( fun node -> eval (depth+1) node path symtbl ) lst;
      print_indent depth "</Design>\n"
  | Node(N_Entity_decl, [A_entity_name,ename], (_::_ as elist)) ->
      let newpath = {package=path.package; entity=(Some ename)} in
      Hashtbl.add symtbl newpath (Hashtbl.create 7);
      print_indent depth  (Printf.sprintf "<Entity name=\"%s\">\n" ename );
      List.iter ( fun node -> eval (depth+1) node newpath symtbl ) elist;
      print_indent depth "</Entity>\n" 
  | Node(N_Dir_spec,  [_,z], [x]) -> (Printf.printf "Direction is: %s" z); eval (depth+1) x path symtbl
  | Node(N_GenericClause, _, (_::_ as lst)) ->
      (*print_indent depth "<GenericClause>\n";*)
      print_indent depth "<Generics>\n";
      List.iter ( fun node -> eval (depth+1) node path symtbl) lst ;
      (*pf "</GenericClause>\n"*)
      print_indent depth "</Generics>\n";
  | Node(N_GenericDecl, [(A_gennames,names);(A_itype,gentype);(A_generic_value,value)], _) -> 
         let ent_symtab = Hashtbl.find symtbl path in
         let namelist = split "," names in
         List.iter ( fun s -> (
           (print_indent depth (
             Printf.sprintf "<Generic> name=\"%s\", type=\"%s\", value=\"%s\" </Generic>\n" s gentype value)
           )); 
           Hashtbl.add ent_symtab s (Some value)
         ) namelist
  | Node(N_GenericDecl, [(A_gennames,names);(A_itype,gentype)], _) -> 
         let ent_symtab = Hashtbl.find symtbl path in
         let namelist = split "," names in
         List.iter ( fun s -> (
           print_indent depth ( Printf.sprintf "<Generic> name=\"%s\", type=\"%s\", value=\"??\" </Generic>\n" s gentype ); 
           Hashtbl.add ent_symtab s None
         )) namelist
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

  | Node(N_SliceName, _, [x]) -> Printf.printf "( "; eval depth x path symtbl;  Printf.printf " )"
  | Node(N_Range, [A_dir,d], [x;y]) -> eval depth x path symtbl;Printf.printf " %s " d; 
                                       eval depth y path symtbl
  | Node(N_ISigDecl, [(A_signames,z);(A_mode,y)], [x] ) ->
      let siglist = split "," z in
      List.iter ( fun s -> 
        print_indent depth (Printf.sprintf "{\"name\": %s, \"mode\": %s => TypE: ? }\n" s y) ; 
                           eval (depth+1) x path symtbl
                ) siglist
  | Node(N_ISigDecl, [(A_signames,z);(A_mode,y);(A_itype,tt)], x::xs ) ->
      let siglist = split "," z in
      List.iter ( fun s -> 
        print_indent depth (Printf.sprintf "<Port_signal> name=\"%s\" mode=\"%s\" type: \"%s" s y tt); 
                           eval (depth+1) x path symtbl; Printf.printf "\" </Port_signal>\n";
                ) siglist

  | Node(N_ISigDecl, [(A_signames,z);(A_mode,y);(A_itype,tt)], [] ) ->
      let siglist = split "," z in
      List.iter ( fun s -> 
        print_indent depth (Printf.sprintf "<Port_signal> name=\"%s\" mode=\"%s\" type=\"%s\" </Port_signal>\n" s y tt )
                ) siglist

  | Node(N_ISigDecl, al, _ ) -> Printf.printf "ISigDecl : no specified fields\n"
  | Node(N_Port_decl, al, (_::_ as lst) ) -> print_indent depth "<Ports>\n";
     List.iter ( fun node -> eval (depth+1) node path symtbl) lst ;
     print_indent depth "</Ports>\n";

  | Node(N_Add, _, _) | Node(N_Sub, _, _) | Node(N_Mul, _, _) 
  | Node(N_Neg, _, _) | Node(N_Number,_,_)| Node(N_Simple_term,_,_)  ->
      Printf.printf " %d " (reduce tree)

  | Node(N_Comment, _,_) -> ()
  |Node( (_ as unode), _, _) ->  Printf.printf "What node is this? "; print_node_name stdout unode; 
                                 Printf.printf "\n";
  | _ -> Printf.printf "What's this?\n"

;;

let _ =
  let sym_tbl = Hashtbl.create 17 in
  let path = { package=(Some "__DEFAULT__"); entity=None} in
  (*let u = "--comment\nentity Blah is generic( genval,gen2: integer := 0; genv3: integer :=
    12; genv4: string ); port ( foo,bar: in bit; baz: out bit
);\n--comment2\nentity BAZ is port(x:in bit; y:out bit);" in *)
  let u = "--comment\nentity Blah is generic( genval,gen2: integer := 2; genv3: integer :=
    12; genv4: string ); port ( foo,bar: in bit; baz: out bitvector((gen2+1)*2 downto 3*(0-1))
);\n--comment2\nentity BAZ is port(x:in bit; y:out bit);" in
    let t = Aurochs.read ~grammar:(`Program Vhdl.program) ~text:(`String u) in
    Hashtbl.add sym_tbl path (Hashtbl.create 17); (* path => Hash *)
    eval 0 t path sym_tbl;;

