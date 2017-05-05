# Our CSV row object
class CSVRow
  attr_accessor :data

  def initialize(headings, row)
    @data = row
    @headings = headings
  end

  def method_missing name, *args
    column = name.to_s
    i = @headings.find_index(column)
    data[i]
  end
end
    

# Act as CSV class (from book)
module ActsAsCsv
  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end

  module InstanceMethods
    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.csv'
      file = File.new(filename)
      @headers = file.gets.chomp.split(', ')

      file.each do |row|
        @csv_contents << row.chomp.split(', ')
      end
    end

    attr_accessor :headers, :csv_contents
    def initialize
      read
    end

    # Our each method
    def each
      @csv_contents.each do |row|
        yield CSVRow.new(@headers, row)
      end
    end
  end
end


class RubyCsv
  include ActsAsCsv
  acts_as_csv
end


if __FILE__ == $0
  test = RubyCsv.new
  test.each do |row|
    puts row.Description
  end
end
