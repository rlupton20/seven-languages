array = (1..16).to_a


class Array
  def each_4
    i = 0
    while i < self.length
      yield self.slice(i,4)
      i = i+4
    end
  end
end


def block_print(block)
  block.each do | i |
    print i
    print " "
  end
end

puts "Manual method"

array.each_4 do | block |
  block_print(block)
  puts ""
end


puts ""
puts "each_slice method"

array.each_slice(4) do | block |
  block_print(block)
  puts ""
end
