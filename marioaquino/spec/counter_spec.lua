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
  it("splits a string into a table of individual characters", function()
    assert.are.same('a', string_iterator('a')())
  end)
end)

describe('a value accumulator', function()

end)
