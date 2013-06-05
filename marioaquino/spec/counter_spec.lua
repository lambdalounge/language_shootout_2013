require 'counter'

describe('nucleotide counter', function()
  it('returns a string with zeros if an empty string is passed', function()
    assert.are.equal('0 0 0 0', count_nucleotides(''))
  end)

  it('does not count non-nucleotide characters in the string', function()
    assert.are.equal('0 0 0 0', count_nucleotides('POLIUKJMNHYBVFREDXSWQZ'))
  end)

  it('ignores whitespace', function()
    assert.are.equal('0 0 0 0', count_nucleotides('       '))
  end)

  it('does not count non-string parameters', function()
    assert.are.equal('0 0 0 0', count_nucleotides(123456789))
  end)
end)

describe('a string iterator', function()
  it('splits a string into a table of individual characters', function()
    assert.are.same('a', string_iterator('abc')())
  end)
end)

describe('a string trimmer', function()
  it('removes whitespace on either side of a string', function()
    assert.are.same('a b c', trim(' a b c  '))
  end)
end)

describe('a value accumulator', function()
  it('inserts a value at the end of the table', function()
    local val = {'a', 'b', 'c'}
    local collector = {}
    append_from_table(val, function(val) return val end, collector)
    assert.are.same({'a', 'b', 'c'}, collector)
  end)
end)

describe('a map function', function()
  it('applies a function to each item in a structure and returns a structure with the results', function()
    local data = {'a', 'b', 'c'}
    assert.are.same({'A', 'B', 'C'}, map(data, string.upper))
  end)
end)

describe('a reduce function', function()
  it('accumulates the result of all function calls over a data structure', function()
    assert.are.equal(10, reduce({1,2,3,4}, 0, function(memo, val) return memo+val end))

    assert.are.equal(24, reduce({1,2,3,4}, 1, function(memo, val) return memo*val end))
  end)
end)

describe('a length function', function()
  it('counts the length of a string', function()
    assert.are.equal(3, length('abc'))
  end)

  it('counts the length of a table', function()
    assert.are.equal(4, length({1, 2, 3, 4}))
  end)
end)

describe('using a string iterator in a map', function()
  it('collects all tokens from the string in a table', function()
    assert.are.same({'a', 'b', 'c'}, map(string_iterator('abc'), function(val) return val end))
  end)
end)
