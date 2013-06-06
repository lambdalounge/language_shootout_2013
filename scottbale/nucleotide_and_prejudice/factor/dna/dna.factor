USING: io kernel hashtables assocs math sequences ;
IN: dna

CONSTANT: counts H{ { CHAR: A 0 } 
                    { CHAR: C 0 }
                    { CHAR: G 0 }
                    { CHAR: T 0 } } 

: count-dna ( string -- counts-assoc )
counts [ over [ 1 + ] change-at ] reduce ;

: formatted-count ( string -- string )
drop "todo" ;

: demo ( -- assoc )
"ACGCATGAAT" count-dna ;

