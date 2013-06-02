function count_nucleotides(input)
  return '0 0 0 0'
end

function append_to(func, collector)
  for val in func do
    table.insert(collector, val)
  end
end

function string_iterator(str)
  return string.gmatch(str, "%a")
end
