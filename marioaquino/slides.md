!SLIDE bullets

# [Lua Nucleotide Counter](http://lua.org)
## (functional style)
## [@marioaquino](http://twitter.com/marioaquino)

!SLIDE transition=scrollUp smaller-code

    @@@ javascript
    function countNucleotides(input)
      return trim(
               reduce(
                 map(nucleotides(), function(nucleotide)
                   return substringCounter(input, nucleotide)
                 end),
                 "",
                 function(memo, val)
                   return memo.." "..val
                 end)
             )
    end
