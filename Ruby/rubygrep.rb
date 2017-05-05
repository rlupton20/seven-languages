class File
  def numbered_lines(&block)
    i = 0
    while line = self.gets
      yield i, line
      i = i + 1
    end
  end
end


def rubygrep(term, file)
  regex = Regexp.new(term)
  i = 0
  File.open(file, 'r') do |f|
    f.numbered_lines do |i, line|
      if regex.match(line)
        print i, ": ", line
      end
    end
  end
end
 

term = ARGV[0]
file = ARGV[1]
rubygrep(term,file)
