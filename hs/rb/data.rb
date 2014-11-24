def hsImports
<<-EOS
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp
import qualified System.IO.Unsafe as RUnsafe
EOS
end

class Tag
  def initialize(nameHs, exprJs = nameHs.upcase)
    @nameHs = nameHs
    @exprJs = exprJs
  end
  attr_reader :nameHs, :exprJs
end

def makeEnum(name, tags, exprPrefix: '', instanceFrom: true)
  tags = tags.map do |t|
    t.is_a?(String) ? Tag.new(t) : t
  end
  lines = []

  lines << "data #{name} = #{tags.map(&:nameHs).join(' | ')} deriving (Eq, Ord, Show, Read, Enum, Bounded)"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    lines << "foreign import javascript unsafe #{(exprPrefix + tag.exprJs).inspect} #{importName} :: RTypes.JSRef #{name}"
  end

  lines << "instance RMarshal.ToJSRef #{name} where"
  tags.each do |tag|
    importName = "_#{name}_#{tag.nameHs}"
    lines << "  toJSRef #{tag.nameHs} = return #{importName}"
  end

  if instanceFrom
    lines << "instance RMarshal.FromJSRef #{name} where"
    lines << "  fromJSRef = RInternal.js_fromEnum"
  end

  lines.join("\n")
end

class Field
  def initialize(type, nameHs, nameJs = nameHs)
    @type = type
    @nameHs = nameHs
    @nameJs = nameJs
  end
  attr_reader :type, :nameHs, :nameJs
end

def makeRecord(name, fields, instanceDefault: true)
  fieldDefs = fields.map do |field|
    "#{field.nameHs} :: #{field.type}"
  end
  lines = []

  lines << "data #{name} = #{name} { #{fieldDefs.join(', ')} } deriving (Eq, Ord, Show, Read)"

  defaultExprs = [name] + Array.new(fields.length, 'RDefault.def')
  if instanceDefault
    lines << "instance RDefault.Default #{name} where def = #{defaultExprs.join(' ')}"
  end

  lines << "instance RMarshal.ToJSRef #{name} where"
  lines << "  toJSRef opts = do"
  lines << "    obj <- RForeign.newObj"
  lines << "    let _setJust s f = case f opts of"
  lines << "          Nothing -> return ()"
  lines << "          Just x -> RMarshal.toJSRef x >>= \\ref -> RForeign.setProp s ref obj"
  lines << "        _set s f = RMarshal.toJSRef (f opts) >>= \\ref -> RForeign.setProp s ref obj"
  fields.each do |field|
    if field.type.start_with? 'Maybe'
      lines << "    _setJust #{field.nameJs.inspect} #{field.nameHs}"
    else
      lines << "    _set #{field.nameJs.inspect} #{field.nameHs}"
    end
  end
  lines << "    return obj"

  lines << "instance RMarshal.FromJSRef #{name} where"
  lines << "  fromJSRef obj = do"
  bound = []
  fields.each_with_index do |field, i|
    thisBound = "_x#{i}"
    bound << thisBound
    lines << "    #{thisBound} <- RInternal.fromProp #{field.nameJs.inspect} obj"
  end
  lines << "    return $ #{name} RApp.<$> #{bound.join(' RApp.<*> ')}"

  lines.join("\n")
end

def jsImport(hsName, jsExpr, args, result, isIO: true)
  isEither   = result =~ /\bEither\b/
  isCallback = jsExpr.include? '$c'
  unless hsName
    idents = jsExpr.scan /[A-Za-z_][A-Za-z0-9_]*/
    idents.reject! { |id| %w{hs_good hs_error c}.include? id }
    hsName = idents[-1]
  end
  argsJs = args.map { |arg| "RTypes.JSRef (#{arg}) ->" }.join(' ')
  argsHs = args.map { |arg| "#{arg} ->"         }.join(' ')
  vals = (0 ... args.length).map { |i| "arg#{i}" }
  lines = []

  lines << "foreign import javascript #{isCallback ? 'interruptible' : 'unsafe'}"
  lines << "  #{jsExpr.inspect}"
  if isEither
    resultJs = result.gsub(/\bEither\b/, 'RInternal.JSEitherRef')
  else
    resultJs = "RTypes.JSRef (#{result})"
  end
  lines << "  js_#{hsName} :: #{argsJs} IO (#{resultJs})"

  lines << "#{hsName} :: #{argsHs} #{isIO ? 'IO' : ''} (#{result})"
  lines << "#{hsName} #{vals.join(' ')} = #{isIO ? '' : 'RUnsafe.unsafePerformIO $'} do"
  vals.each do |val|
    lines << "  #{val}' <- RMarshal.toJSRef #{val}"
  end
  lines << "  res <- js_#{hsName} #{vals.map { |v| "#{v}'" }.join(' ')}"
  if isEither
    lines << "  RInternal.fromJSEitherRef res"
  else
    lines << "  RInternal.fromJSRef' res"
  end

  lines.join("\n")
end
