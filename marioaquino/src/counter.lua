function count_nucleotides(input)
  return '0 0 0 0'
end

function append_to(data, func, collector)
  for i,val in ipairs(data) do
    table.insert(collector, func(val))
  end
end

function string_iterator(str)
  return string.gmatch(str, "%a")
end

function map(data, func)
  local collector = {}
  append_to(data, func, collector)
  return collector
end
