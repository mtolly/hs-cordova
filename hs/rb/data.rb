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
  def initialize(hsName, jsExpr: hsName.upcase, hsDoc: nil)
    @hsName = hsName
    @jsExpr = jsExpr
    @hsDoc = hsDoc
  end
  attr_reader :hsName, :jsExpr, :hsDoc
end

def makeEnum(name, tags, exprPrefix: '', instanceFrom: true, hsDoc: nil)
  tags = tags.map do |t|
    t.is_a?(String) ? Tag.new(t) : t
  end
  lines = []

  lines << "-- | #{hsDoc}" if hsDoc
  lines << "data #{name}"
  tags.each_with_index do |tag, i|
    lines << "  #{i == 0 ? '=' : '|'} #{tag.hsName}"
    lines << "  -- ^ #{tag.hsDoc}" if tag.hsDoc
  end
  lines << "  deriving (Eq, Ord, Show, Read, Enum, Bounded)"

  tags.each do |tag|
    importName = "_#{name}_#{tag.hsName}"
    lines << "foreign import javascript unsafe #{(exprPrefix + tag.jsExpr).inspect} #{importName} :: RTypes.JSRef #{name}"
  end

  lines << "instance RMarshal.ToJSRef #{name} where"
  tags.each do |tag|
    importName = "_#{name}_#{tag.hsName}"
    lines << "  toJSRef #{tag.hsName} = return #{importName}"
  end

  if instanceFrom
    lines << "instance RMarshal.FromJSRef #{name} where"
    lines << "  fromJSRef = RInternal.js_fromEnum"
  end

  lines.join("\n")
end

class Field
  def initialize(type, hsName, jsName: hsName, hsDoc: nil)
    @type = type
    @hsName = hsName
    @jsName = jsName
    @hsDoc = hsDoc
  end
  attr_reader :type, :hsName, :jsName, :hsDoc
end

def makeRecord(name, fields, instanceTo: true, instanceDefault: true, hsDoc: nil)
  lines = []

  words = name.split(/\s+/)
  constructor = words[0]
  vars = words.select { |word| word.match(/^[a-z]/) }

  def context(tclass, vars)
    if vars.empty?
      ""
    else
      "(#{ vars.map { |v| tclass + ' ' + v }.join(', ') }) =>"
    end
  end

  lines << "-- | #{hsDoc}" if hsDoc
  lines << "#{fields.length == 1 ? 'newtype' : 'data'} #{name} = #{constructor}"
  fields.each_with_index do |field, i|
    lines << "  #{i == 0 ? '{' : ','} #{field.hsName} :: #{field.type}"
    lines << "  -- ^ #{field.hsDoc}" if field.hsDoc
  end
  lines << "  } deriving (Eq, Ord, Show, Read)"

  defaultExprs = [constructor] + Array.new(fields.length, 'RDefault.def')
  if instanceDefault
    lines << "instance #{context('RDefault.Default', vars)} RDefault.Default (#{name}) where def = #{defaultExprs.join(' ')}"
  end

  if instanceTo
    lines << "instance #{context('RMarshal.ToJSRef', vars)} RMarshal.ToJSRef (#{name}) where"
    lines << "  toJSRef opts = do"
    lines << "    obj <- RForeign.newObj"
    lines << "    let _setJust s f = case f opts of"
    lines << "          Nothing -> return ()"
    lines << "          Just x -> RMarshal.toJSRef x >>= \\ref -> RForeign.setProp s ref obj"
    lines << "        _set s f = RMarshal.toJSRef (f opts) >>= \\ref -> RForeign.setProp s ref obj"
    fields.each do |field|
      if field.type.start_with? 'Maybe'
        lines << "    _setJust #{field.jsName.inspect} #{field.hsName}"
      else
        lines << "    _set #{field.jsName.inspect} #{field.hsName}"
      end
    end
    lines << "    return obj"
  end

  lines << "instance #{context('RMarshal.FromJSRef', vars)} RMarshal.FromJSRef (#{name}) where"
  lines << "  fromJSRef obj = do"
  bound = []
  fields.each_with_index do |field, i|
    thisBound = "_x#{i}"
    bound << thisBound
    lines << "    #{thisBound} <- RInternal.fromProp #{field.jsName.inspect} obj"
  end
  lines << "    return $ #{constructor} RApp.<$> #{bound.join(' RApp.<*> ')}"

  lines.join("\n")
end

def jsImport(jsExpr, args, result, hsName: nil, isIO: true, hsDoc: nil)
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

  lines << "-- | #{hsDoc}" if hsDoc
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
