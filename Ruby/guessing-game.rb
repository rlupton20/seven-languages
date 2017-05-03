def high_or_low(i, j)
  if i < j
    "high"
  else
    "low"
  end
end


def make_a_guess
  print "Make a guess: "
  gets.to_i
end


def main
  i = rand(10)
  while i != (j = make_a_guess)
    puts "Your guess was too #{high_or_low(i,j)}"
  end
  puts "You guessed correct"
end


if __FILE__ == $0
  main
end
