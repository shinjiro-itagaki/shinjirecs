class Object
  def available?(&block)
    if block then
      (block.call self) ? self : nil
    else
      self
    end
  end
end
