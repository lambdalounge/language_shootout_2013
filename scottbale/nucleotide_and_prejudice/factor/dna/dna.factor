USING: io kernel hashtables assocs math sequences strings prettyprint ;
IN: dna

CONSTANT: count-inits H{ { CHAR: A 0 } 
                         { CHAR: C 0 }
                         { CHAR: G 0 }
                         { CHAR: T 0 } } 

: count-dna ( string -- counts-assoc )
count-inits { } assoc-clone-like [ over [ 1 + ] change-at ] reduce ;

: formatted-count ( string -- string )
drop "todo" ;

: demo ( -- )
"ACGCATGAAT" count-dna values pprint ;

MAIN: demo