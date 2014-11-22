class Field
  def initialize(type, nameHs, nameJs = nameHs)
    @type = type
    @nameHs = nameHs
    @nameJs = nameJs
  end
  attr_reader :type, :nameHs, :nameJs
end

def makeRecordModule(imports, name, fields, default = true)
  fieldDefs = fields.map do |field|
    "#{field.nameHs} :: #{field.type}"
  end
  lines = []

  lines << "module #{ENV['ERB_HS_MODULE']} where"

  lines << "import GHCJS.Foreign"
  lines << "import GHCJS.Marshal"
  if default
    lines << "import Data.Default"
  end
  lines << "import Control.Applicative"
  lines << "import System.Cordova.Internal"
  lines += imports

  lines << "data #{name} = #{name} { #{fieldDefs.join(', ')} } deriving (Eq, Ord, Show, Read)"

  defaultExprs = [name] + Array.new(fields.length, 'def')
  if default
    lines << "instance Default #{name} where def = #{defaultExprs.join(' ')}"
  end

  lines << "instance ToJSRef #{name} where"
  lines << "  toJSRef opts = do"
  lines << "    obj <- newObj"
  lines << "    let _setJust s f = case f opts of"
  lines << "          Nothing -> return ()"
  lines << "          Just x -> toJSRef x >>= \\ref -> setProp s ref obj"
  lines << "        _set s f = toJSRef (f opts) >>= \\ref -> setProp s ref obj"
  fields.each do |field|
    if field.type.start_with? 'Maybe'
      lines << "    _setJust #{field.nameJs.inspect} #{field.nameHs}"
    else
      lines << "    _set #{field.nameJs.inspect} #{field.nameHs}"
    end
  end
  lines << "    return obj"

  lines << "instance FromJSRef #{name} where"
  lines << "  fromJSRef obj = do"
  bound = []
  fields.each_with_index do |field, i|
    thisBound = "_x#{i}"
    bound << thisBound
    lines << "    #{thisBound} <- fromProp #{field.nameJs.inspect} obj"
  end
  lines << "    return $ #{name} <$> #{bound.join(' <*> ')}"

  lines.join("\n")
end
