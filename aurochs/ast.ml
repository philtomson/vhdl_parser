(* ast.ml - abstract syntax for port parser *)
type name  = string;;
type mode  = In | Out | InOut;;
type 'a range = Full | Range of ('a * 'a);;
(* aType probably needs to be expanded quite a lot *)
type aTypeVal = Int of (int * int range) 
              | Float of (float * float range) 
              | Vector of ( bool array * int range)
              | Str of string  ;;
             (* | UserDef of 'a ;;  UserDef probably won't work
                                    in practice *)

type desLst = desUnit list
and desUnit = DesUnit of (interfaceLst * (genericLst option)) 
and genericLst = GenLst of generic list
and generic = Generic of (name * aTypeVal) 
and interfaceLst = InterfaceLst of interfaceSig list
and interfaceSig = InterfaceSig of (name * mode * aTypeVal) ;;

(* Example: 
let des = [ DesUnit( 
               InterfaceLst [ 
                 InterfaceSig ("A",In,Int(0, Range(0,255)))
               ],
               Some(GenLst [
                      Generic("FOO",Float(0.0,Full))
                    ]))
          ] ;;  
*)
