class String
  def non_empty
    (self.empty?) ? nil : self
  end

  def self.get_max_common_words(xs_org,ys_org,step=0,words=[])
    if xs_org.length < 1 || ys_org.length < 1 then
      return words
    end

    nils = Array.new(xs_org.length-1){nil}
    ys = nils + ys_org
    ys = ys.drop(step)
    if ys.empty? then
      return words
    end
    xs = [] + xs_org
    detected = ""
    
    while x = xs.shift
      if x == ys.shift then
        detected << x
      else
        if not detected.empty? then
          words << detected
          detected = ""
        end
      end
    end

    words << detected if not detected.empty?
    
    self.get_max_common_words(xs_org,ys_org,step+1,words.uniq)
  end
  
  def &(str)
    xs = self.split(//)
    ys = str.split(//)
    self.class.get_max_common_words(xs,ys).reverse.sort_by(&:length).last
  end
end
