require 'aqueductron'

integers = -> { (1..Float::INFINITY).lazy }

# a custom function that returns a new custom function. This remembers
# the previous two numbers, so it know which to return next.
fib_function = ->(prev_number, prev_prev_number, k = 1) do
  # aqueductron functions accept the current piece and a message.
  # they return the result of passing on both the message and the next
  # action.
  -> (piece,msg) do
    current_val = prev_number + ( prev_prev_number * k )
    piece.pass_on(current_val, fib_function.call(current_val, prev_number, k), "..#{prev_number}, #{current_val}..")
  end
end

#
# To play with a duct in irb, initialize this:
# f = Duct.new.custom(fib_function.call(1, 0, 1))).last
#
# f = f.drip(:whatever)
#
# repeat that for a while.
# Whatever you send to it, it'll track fibonacci numbers.

fib = ->(n, k=1) do
  if n == 1 then
    1
  else
    # create a duct that generates fib numbers, stops after so many,
    # and returns the last one generated.
    duct = Aqueductron::Duct.new.custom(fib_function.call(1, 0, k)).take(n-1).last
    # send all the integers in the world - it won't take them all.
    duct.flow(integers.call).value
  end
end

puts "fib from 1 to 10: "
puts (1..10).map{|n| fib.call(n)}.join(" ")

puts "for n=5, k=3 we have #{fib.call(5,3)}"
