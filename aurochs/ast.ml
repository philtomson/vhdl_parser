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

type desSeq = desUnit list
and desUnit = interfaceSeq * (genericSeq option) 
and genericSeq = generic list
and generic = (name * aTypeVal) 
and interfaceSeq = interfaceSig list
and interfaceSig = (name * mode * aTypeVal) ;;

  
