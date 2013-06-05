require 'aqueductron'

input = "AATTGGGGAGCAN"

# create a piece of a duct that counts what goes in
#   --\
#***   count
#   --/
count_letter = ->(unused) { Aqueductron::Duct.new.count }
identity = ->(e) { e }

# a dynamically-splitting pipeline that categorizes
#                 < ***
# ---            /
# >   identity <  - < ***
# ---            \
#                 < ***
pipe = Aqueductron::Duct.new.partition(identity, count_letter)

# send the letters through.
result = duct.flow(input.chars)

bases = result.keys
puts bases.join(" ")
puts bases.map { |atgc| result.value(atgc)}.join(" ")
