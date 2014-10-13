class Field
  def initialize(type, nameHs, nameJs = nameHs)
    @type = type
    @nameHs = nameHs
    @nameJs = nameJs
  end
  attr_reader :type, :nameHs, :nameJs
end

def makeRecord(name, fields)
  lines = []
  fieldDefs = fields.map do |field|
    "#{field.nameHs} :: #{field.type}"
  end
  lines << "data #{name} = #{name} { #{fieldDefs.join(', ')} } deriving (Eq, Ord, Show, Read)"
  defaultExprs = [name] + Array.new(fields.length, 'def')
  lines << "instance Default #{name} where def = #{defaultExprs.join(' ')}"
  lines << "instance ToJSRef #{name} where"
  lines << "  toJSRef opts = do"
  lines << "    obj <- newObj"
  lines << "    let setJust s f = case f opts of"
  lines << "          Nothing -> return ()"
  lines << "          Just x -> toJSRef x >>= \\ref -> setProp s ref obj"
  fields.each do |field|
    lines << "    setJust #{field.nameJs.inspect} #{field.nameHs}"
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
