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
  lines = []
  tags = tags.map do |t|
    t.is_a?(String) ? Tag.new(t) : t
  end
  lines << "data #{name} = #{tags.map(&:nameHs).join(' | ')} deriving (Eq, Ord, Show, Read, Enum, Bounded)"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    lines << "foreign import javascript unsafe #{(exprPrefix + tag.exprJs).inspect} #{importName} :: JSRef #{name}"
  end
  lines << "instance ToJSRef #{name} where"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    lines << "  toJSRef #{tag.nameHs} = return #{importName}"
  end
  lines.join("\n")
end
