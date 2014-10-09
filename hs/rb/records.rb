class Field
  def initialize(type, nameHs, nameJs = nameHs)
    @type = type
    @nameHs = nameHs
    @nameJs = nameJs
  end
  attr_reader :type, :nameHs, :nameJs
end

def makeRecord(name, fields)
  out = ''
  class << out
    def line(ln)
      self << ln << "\n"
    end
  end
  fieldDefs = fields.map do |field|
    "#{field.nameHs} :: Maybe #{field.type}"
  end
  out.line "data #{name} = #{name} { #{fieldDefs.join(', ')} } deriving (Eq, Ord, Show, Read)"
  defaultExprs = [name] + Array.new(fields.length, 'Nothing')
  out.line "instance Default #{name} where defaultValue = #{defaultExprs.join(' ')}"
  out << """
instance ToJSRef #{name} where
  toJSRef opts = do
    obj <- newObj
    let setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \\ref -> setProp s ref obj
"""
  fields.each do |field|
    out.line "    setJust #{field.nameJs.inspect} #{field.nameHs}"
  end
  out.line "    return obj"
  out
end
