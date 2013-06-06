USING: io kernel hashtables assocs math sequences ;
IN: dna

CONSTANT: counts H{ { CHAR: A 0 } 
                    { CHAR: C 0 }
                    { CHAR: G 0 }
                    { CHAR: T 0 } } 

: assoc-at ( ..a assoc key quot: ( ..a value -- ..b newvalue ) -- ..b assoc )


: foo ( string -- assoc )
counts [ compose [ swap ] [ [ 1 + ] assoc-at ] ] reduce ;

: formatted-count ( string -- string )
drop "todo" ;

: demo ( -- assoc )
"ACGCATGAAT" foo ;

MAIN: demo drop