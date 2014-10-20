class Tag
  def initialize(nameHs, exprJs = nameHs.upcase)
    @nameHs = nameHs
    @exprJs = exprJs
  end
  attr_reader :nameHs, :exprJs
end

def erbFileToModule(erb_file)
  parts = erb_file.split('/')
  parts.shift until parts[0] == 'lib'
  parts.shift
  parts[-1] = parts[-1].match(/\.erb$/) { |md| md.pre_match }
  parts.join('.')
end

def makeEnumModule(erb_file, name, tags, exprPrefix = '')
  tags = tags.map do |t|
    t.is_a?(String) ? Tag.new(t) : t
  end
  lines = []

  lines << "module #{erbFileToModule(erb_file)} where"

  lines << "import GHCJS.Types"
  lines << "import GHCJS.Marshal"
  lines << "import System.Cordova.Internal"

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

  lines << "instance FromJSRef #{name} where"
  lines << "  fromJSRef = js_fromEnum"

  lines.join("\n")
end
