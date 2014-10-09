def cordovaEnum(str)
  str.gsub(/([a-z])([A-Z])([A-Z]|$)/) do |md|
    $1 + '_' + $2 + $3
  end.upcase
end

class Tag
  def initialize(nameHs, exprJs = cordovaEnum(nameHs))
    @nameHs = nameHs
    @exprJs = exprJs
  end
  attr_reader :nameHs, :exprJs
end

def makeEnum(name, tags, exprPrefix = '')
  out = ''
  tags = tags.map do |t|
    t.is_a?(String) ? Tag.new(t) : t
  end
  class << out
    def line(ln)
      self << ln << "\n"
    end
  end
  out.line "data #{name} = #{tags.map(&:nameHs).join(' | ')} deriving (Eq, Ord, Show, Read, Enum, Bounded)"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    out.line "foreign import javascript unsafe #{(exprPrefix + tag.exprJs).inspect} #{importName} :: JSRef #{name}"
  end
  out.line "instance ToJSRef #{name} where"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    out.line "  toJSRef #{tag.nameHs} = return #{importName}"
  end
  out
end
