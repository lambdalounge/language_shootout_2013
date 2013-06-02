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

  it('accepts non-string parameters', function()
    assert.are.equal('0 0 0 0', count_nucleotides(123456789))
  end)
end)

describe('a string splitter', function()
  it('splits a string into a table of individual characters', function()
    assert.are.same('a', string_iterator('a')())
  end)
end)

describe('a value accumulator', function()
  it('inserts a value at the end of the table', function()
    local val = {'a', 'b', 'c'}
    local collector = {}
    append_to(val, function(val) return val end, collector)
    assert.are.same({'a', 'b', 'c'}, collector)
  end)
end)

describe('a map function', function()
  it('applies a function to each item in a structure and returns a structure with the results', function()
    local data = {'a', 'b', 'c'}
    assert.are.same({'A', 'B', 'C'}, map(data, string.upper))
  end)
end)
