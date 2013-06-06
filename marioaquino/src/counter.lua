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

function nucleotides()
  return {'A', 'C', 'G', 'T'}
end

function appendFromTable(data, func, collector)
  for i,val in ipairs(data) do
    table.insert(collector, func(val))
  end
end

function trim(str)
  return str:match "^%s*(.-)%s*$"
end

function length(input)
  return #input
end

function substringCounter(str, substring)
  return length(prune(str, substring))
end

function prune(str, keep)
  val = string.gsub(str, "[^"..keep.."]", "")
  return val
end

function map(data, func)
  local collector = {}
  appendFromTable(data, func, collector)
  return collector
end

function reduce(data, memo, func)
  for i,val in ipairs(data) do
    memo = func(memo, val)
  end
  return memo
end
