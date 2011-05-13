--comment
entity Blah is
  Generic( GENVAL,gv1 : integer := 8; gv2 : integer := 12  );
  Port ( foo , bar : in bitvector(gv2+0 to 8-1); 
       baz: out bit 
     );
--comment
