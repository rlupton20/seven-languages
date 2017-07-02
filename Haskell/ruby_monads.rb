# I'll have a go at making something monadic in Ruby also
# There is no type system to enforce bind at compile time,
# and we can't talk about monads in the abstract.
#
# Something like an abstract base class would be one
# way to capture a monad. We fake that using a mixin
# that needs overiding. This can be tightened up.

module Monad
  def return(x)
    raise "Not Implemented"
  end

  def bind(f)
    raise "Not Implemented"
  end
end

# By way of example we encode the maybe monad
# We don't allow access to the wrapped value, save
# via the maybe function
class Maybe
  include Monad

  def initialize(v,n)
    @value = v
    @nothing = n
  end

  def self.nothing()
    return Maybe.new(nil,true)
  end

  def self.return(v)
    return Maybe.new(v,false)
  end

  def bind
    if @nothing
      return Maybe.nothing()
    else
      return yield @value
    end
  end

  def maybe(default)
    if @nothing
      return default
    else
      return yield @value
    end
  end

  def render
    if @nothing
      return "Nothing"
    else
      return "Just #{@value}"
    end
  end

end

# Test the monadic operators
test1 = Maybe.return(1).bind {|v| Maybe.return(v+4)}
puts(test1.render())
test2 = Maybe.nothing().bind {|v| Maybe.new(5)}
puts(test2.render())

# Test the maybe unwrapper
puts "maybe tests"
test1.maybe(0) {|v| puts v}
puts test2.maybe("Default") {|v| return v}
