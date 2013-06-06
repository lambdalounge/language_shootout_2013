USING: io kernel hashtables assoc;
IN: dna

CONSTANT: counts H{ { CHAR: A 0 } 
                    { CHAR: C 0 }
                    { CHAR: G 0 }
                    { CHAR: T 0 } } 

: foo ( char assoc -- assoc )
"ABCAB" H{ { CHAR: A 0} } [ swap [ 1 + ] change-at ] reduce

: formatted-count ( string -- string )
drop "todo" ;

: demo ( -- )
"ACGCATGAAT" formatted-count print ;

MAIN: CHAR: A foo print ;