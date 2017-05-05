# Verbatim tree from book
class Tree
  attr_accessor :children, :node_name

  def initialize(name, children=[])
    @children = children
    @node_name = name
  end

  def visit_all(&block)
    visit &block
    children.each {|c| c.visit_all &block}
  end

  def visit(&block)
    block.call self
  end
end

# Better initializer

class BadTreeInitializer < RuntimeError
end

class Hash
  def to_tree
    if self.length == 1
      node = self.flatten[0]
      child_hashes = self.flatten[1]
      Tree.new(node, child_hashes.map{ |k,v| {k => v}.to_tree })
    else
      raise BadTreeInitializer
    end
  end
end

t = { 'a' => {'b' => {}, 'c' => {}} }.to_tree

t.visit_all do | node |
  puts node.node_name
end

{}.to_tree
