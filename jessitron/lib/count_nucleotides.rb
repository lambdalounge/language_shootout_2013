require 'aqueductron'


class CountNucleotides
  def initialize
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
    @duct = Aqueductron::Duct.new.partition(identity, count_letter)

  end

  def count(sequence)
    # send the letters through.
    result = @duct.flow(sequence.chars)
    result_to_hash(result)
  end

  # for demonstration purposes
  def for_example
    input = "GACCACTGGTCA"
    puts "input is #{input}"
    result = @duct.flow(input.chars)
    bases = result.keys
    puts bases.join(" ")
    puts bases.map { |atgc| result.value(atgc)}.join(" ")
  end

  private
    def result_to_hash(result)
      keys_to_values = result.keys.map { |k| [k, result.value(k)]}
      Hash[*keys_to_values.flatten]
    end
end
