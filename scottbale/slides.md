!SLIDE bullets

# [Factor](http://factorcode.org)
## [@scottbale](http://twitter.com/scottbale), Revelytix

!SLIDE transition=scrollUp

# Clojure

    @@@ clojure
    (defn count-nucleotides [word]
      (reduce (fn [m symb]
                (let [[k count] (find m symb)]
                  (apply assoc m 
                        [k (inc count)])))
              {\A 0 \C 0 \G 0 \T 0}
              word))

    (comment
      (count-nucleotides "ACGCATGAAT")
      ;; "4 2 2 2"
      )

!SLIDE transition=scrollUp

# Factor

    @@@ factor
    : count-dna ( string -- counts-assoc )
    count-inits { } 
                assoc-clone-like 
                [ over [ 1 + ] change-at ] 
                reduce ;
    
    : demo ( -- )
    "ACGCATGAAT" count-dna values pprint ;
    #! { 4 2 2 2 }

!SLIDE bullets transition=scrollUp

* f g

!SLIDE code transition=scrollUp

# function application

    f g => (f g)

# function composition

    f g => (f . g) => 
    λf. λg. λx. f (g x)
    
!SLIDE code transition=scrollUp

# pointfree 

    f (x, y) = x + y

    f = (+)

!SLIDE transition=scrollUp

# Clojure

    @@@ clojure
    (defn count-nucleotides [word]
      (reduce (fn [m symb]
                (let [[k count] (find m symb)]
                  (apply assoc m 
                        [k (inc count)])))
              {\A 0 \C 0 \G 0 \T 0}
              word))

    (comment
      (count-nucleotides "ACGCATGAAT")
      ;; "4 2 2 2"
      )

!SLIDE transition=scrollUp

# Factor

    @@@ factor
    : count-dna ( string -- counts-assoc )
    count-inits { } 
                assoc-clone-like 
                [ over [ 1 + ] change-at ] 
                reduce ;
    
    : demo ( -- )
    "ACGCATGAAT" count-dna values pprint ;
    #! { 4 2 2 2 }

!SLIDE

    @@@
    (defn count-nucleotides [word]
      (reduce (fn [m symb]
                (let [[k count] (find m symb)]
                  (apply assoc m 
                        [k (inc count)])))
              {\A 0 \C 0 \G 0 \T 0}
              word))

    : count-dna ( string -- counts-assoc )
    count-inits { } 
                assoc-clone-like 
                [ over [ 1 + ] change-at ] 
                reduce ;


