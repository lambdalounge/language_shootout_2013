function count_nucleotides(input)
  return '0 0 0 0'
end

function append_to(func, collector)
  for val in func do
    collector[#collector + 1] = val
  end
end

function string_iterator(str)
  return string.gmatch(str, "%a")
end
