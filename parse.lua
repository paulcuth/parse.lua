local bit = require('bit32')

local Parser = {
  version = '0,.2.1'
}


local input, options, length

-- Options can be set either globally on the parser object through
-- defaultOptions, or during the parse call.
local defaultOptions = {
  -- Explicitly tell the parser when the input ends.
  wait = false,

  -- Store comments as an array in the chunk object.
  comments = true,

  -- Track identifier scopes by adding an isLocal attribute to each
  -- identifier-node.
  scope = false,

  -- Store location information on each syntax node as
  -- `loc = { start = { line, column }, end = { line, column } }`.
  locations = false,

  -- Store the start and end character locations on each syntax node as
  -- `range = [start, end]`.
  ranges = false,

  -- A callback which will be invoked when a syntax node has been completed.
  -- The node which has been created will be passed as the only parameter.
  onCreateNode = null,

  -- A callback which will be invoked when a new scope is created.
  onCreateScope = null,

  -- A callback which will be invoked when the current scope is destroyed.
  onDestroyScope = null,
}

-- The available tokens expressed as enum flags so they can be checked with
-- bitwise operations.

local EOF = 1
local StringLiteral = 2
local Keyword = 4
local Identifier = 8
local NumericLiteral = 16
local Punctuator = 32
local BooleanLiteral = 64
local NilLiteral = 128
local VarargLiteral = 256


Parser.tokenTypes = { 
  EOF = EOF, 
  StringLiteral = StringLiteral,
  Keyword = Keyword,
  Identifier = Identifier,
  NumericLiteral = NumericLiteral,
  Punctuator = Punctuator,
  BooleanLiteral = BooleanLiteral,
  NilLiteral = NilLiteral,
  VarargLiteral = VarargLiteral,
}

-- As this parser is a bit different from luas own, the error messages
-- will be different in some situations.

local errors = {
  unexpected = 'unexpected, %s \'%s\' near \'%s\'',
  expected = ',\'%s\' expected near \'%s\'',
  expectedToken = ',%s expected near \'%s\'',
  unfinishedString = 'unfinished, string near \'%s\'',
  malformedNumber = 'malformed, number near \'%s\'',
  invalidVar = 'invalid, left-hand side of assignment near \'%s\'',
}


-- ### Abstract Syntax Tree
-- The default AST structure is inspired by the Mozilla Parser API but can
-- easily be customized by overriding these functions.

local ast = {

  labelStatement = function(label)
    return {
      type = 'LabelStatement',
      label = label,
    }
  end,

  breakStatement = function()
    return {
      type = 'BreakStatement',
    }
  end,

  gotoStatement = function(label)
    return {
      type = 'GotoStatement',
      label = label,
    }
  end,

  returnStatement = function(args)
    return {
      type = 'ReturnStatement',
      arguments = args,
    }
  end,

  ifStatement = function(clauses)
    return {
      type = 'IfStatement',
      clauses = clauses,
    }
  end,

  ifClause = function(condition, body)
    return {
      type = 'IfClause',
      condition = condition,
      body = body,
    }
  end,
  
  elseifClause = function(condition, body)
    return {
      type = 'ElseifClause',
      condition = condition,
      body = body,
    }
  end,

  elseClause = function(body)
    return {
      type = 'ElseClause',
      body = body,
    }
  end,
  
  whileStatement = function(condition, body)
    return {
      type = 'WhileStatement',
      condition = condition,
      body = body,
    }
  end,

  doStatement = function(body)
    return {
      type = 'DoStatement',
      body = body,
    }
  end,

  repeatStatement = function(condition, body)
    return {
      type = 'RepeatStatement',
      condition = condition,
      body = body,
    }
  end,

  localStatement = function(variables, init)
    return {
      type = 'LocalStatement',
      variables = variables,
      init = init,
    }
  end,

  assignmentStatement = function(variables, init)
    return {
      type = 'AssignmentStatement',
      variables = variables,
      init = init,
    }
  end,

  callStatement = function(expression)
    return {
      type = 'CallStatement',
      expression = expression,
    }
  end,

  functionStatement = function(identifier, parameters, isLocal, body)
    return {
      type = 'FunctionDeclaration',
      identifier = identifier,
      isLocal = isLocal,
      parameters = parameters,
      body = body,
    }
  end,

  forNumericStatement = function(variable, start, _end, step, body)
    return {
      type = 'ForNumericStatement',
      variable = variable,
      start = start,
      ['end'] = _end,
      step = step,
      body = body,
    }
  end,

  forGenericStatement = function(variables, iterators, body)
    return {
      type = 'ForGenericStatement',
      variables = variables,
      iterators = iterators,
      body = body,
    }
  end,

  chunk = function(body)
    return {
      type = 'Chunk',
      body = body,
    }
  end,

  identifier = function(name)
    return {
      type = 'Identifier',
      name = name,
    }
  end,

  literal = function(type, value, raw)
    if type == StringLiteral then
      type = 'StringLiteral'
    elseif type == NumericLiteral then
      type = 'NumericLiteral'
    elseif type == BooleanLiteral then
      type = 'BooleanLiteral'
    elseif type == NilLiteral then
      type = 'NilLiteral'
    else
      type = 'VarargLiteral'
    end

    return {
      type = type,
      value = value,
      raw = raw,
    }
  end,

  tableKey = function(key, value)
    return {
      type = 'TableKey',
      key = key,
      value = value,
    }
  end,

  tableKeyString = function(key, value)
    return {
      type = 'TableKeyString',
      key = key,
      value = value,
    }
  end,

  tableValue = function(value)
    return {
      type = 'TableValue',
      value = value,
    }
  end,

  tableConstructorExpression = function(fields)
    return {
      type = 'TableConstructorExpression',
      fields = fields,
    }
  end,

  binaryExpression = function(operator, left, right)
    local type

    if 'and' == operator or 'or' == operator then
      type = 'LogicalExpression'
    else
      type = 'BinaryExpression'
    end

    return {
      type = type,
      operator = operator,
      left = left,
      right = right,
    }
  end,

  unaryExpression = function(operator, argument)
    return {
      type = 'UnaryExpression',
      operator = operator,
      argument = argument,
    }
  end,

  memberExpression = function(base, indexer, identifier)
    return {
      type = 'MemberExpression',
      indexer = indexer,
      identifier = identifier,
      base = base,
    }
  end,

  indexExpression = function(base, index)
    return {
      type = 'IndexExpression',
      base = base,
      index = index,
    }
  end,

  callExpression = function(base, args)
    return {
      type = 'CallExpression',
      base = base,
      arguments = args,
    }
  end,

  tableCallExpression = function(base, args)
    return {
      type = 'TableCallExpression',
      base = base,
      arguments = args,
    }
  end,

  stringCallExpression = function(base, argument)
    return {
      type = 'StringCallExpression',
      base = base,
      argument = argument,
    }
  end,

  comment = function(value, raw)
    return {
      type = 'Comment',
      value = value,
      raw = raw,
    }
  end,

}


local index
local token
local previousToken
local lookahead
local comments
local tokenStart
local line
local lineStart



function finishNode(node)
  -- Pop a `Marker` off the location-array and attach its location data.
  if trackLocations then
    local location = table.remove(locations)
    location.complete()
    
    if options.locations then 
      node.loc = location.loc
    end

    if options.ranges then 
      node.range = location.range
    end
  end

  if options.onCreateNode then
    options.onCreateNode(node)
  end

  return node
end


local lookahead


-- Helpers
-- -------

function indexOf(array, element)
  for i = 1, #array do
    if array[i] == element then 
      return i
    end
  end

  return 0
end

-- Iterate through an array of objects and return the index of an object
-- with a matching property.

function indexOfObject(array, property, element)
  for i = 1, #array do
    if array[i][property] == element then
      return i
    end
  end

  return 0
end

-- Returns a new object with the properties from all objectes passed as
-- arguments. Last argument takes precedence.
--
-- Example:
--
--     this.options = extend(options, { output: false });

function extend(...)
  local dest = {}

  for _, src in ipairs({...}) do
    for prop, val in pairs(src) do
      dest[prop] = val
    end
  end

  return dest
end





-- ### Error functions

-- #### Raise an exception.
--
-- Raise an exception by passing a token, a string format and its paramters.
--
-- The passed tokens location will automatically be added to the error
-- message if it exists, if not it will default to the lexers current
-- position.
--
-- Example:
--
--     -- [1:0] expected [ near (
--     raise(token, "expected %1 near %2", '[', token.value);

function raise(token, ...)
  local message = string.format(...)
  local col

  if 'nil' ~= type(token.line) then  
    col = token.range[1] - token.lineStart;
    error(string.format('[%s:%s] %s', token.line, col, message))
  else
    col = index - lineStart + 1;
    error(string.format('[%s:%s] %s', line, col, message))
  end
end

-- #### Raise an unexpected token error.
--
-- Example:
--
--     -- expected <name> near '0'
--     raiseUnexpectedToken('<name>', token);

function raiseUnexpectedToken(type, token) 
  raise(token, errors.expectedToken, type, token.value)
end

-- #### Raise a general unexpected error
--
-- Usage should pass either a token object or a symbol string which was
-- expected. We can also specify a nearby token such as <eof>, this will
-- default to the currently active token.
--
-- Example:
--
--     -- Unexpected symbol 'end' near '<eof>'
--     unexpected(token);
--
-- If there's no token in the buffer it means we have reached <eof>.

function unexpected(found, near)
  if 'nil' == type(near) then
    near = lookahead.value
  end

  if 'nil' ~= type(found.type) then
    local type
    if found.type == StringLiteral then
      type = 'string'
    elseif found.type == Keyword then
      type = 'keyword'
    elseif found.type == Identifier then
      type = 'identifier'
    elseif found.type == NumericLiteral then
      type = 'number'
    elseif found.type == Punctuator then
      type = 'symbol'
    elseif found.type == BooleanLiteral then
      type = 'boolean'
    elseif found.type == NilLiteral then
      return raise(found, errors.unexpected, 'symbol', 'nil', near)
    end

    return raise(found, errors.unexpected, type, found.value, near)
  end

  return raise(found, errors.unexpected, 'symbol', found, near)
end



-- Lexer
-- -----
--
-- The lexer, or the tokenizer reads the input string character by character
-- and derives a token left-right. To be as efficient as possible the lexer
-- prioritizes the common cases such as identifiers. It also works with
-- character codes instead of characters as string comparisons was the
-- biggest bottleneck of the parser.
--
-- If `options.comments` is enabled, all comments encountered will be stored
-- in an array which later will be appended to the chunk object. If disabled,
-- they will simply be disregarded.
--
-- When the lexer has derived a valid token, it will be returned as an object
-- containing its value and as well as its position in the input string (this
-- is always enabled to provide proper debug messages).
--
-- `lex()` starts lexing and returns the following token in the stream.

function charAt(str, index)
  return string.sub(str, index, index)
end

function charCodeAt(...)
  return string.byte(charAt(...))
end

function currentChar() 
  return charAt(input, index)
end

function nextChar() 
  return charAt(input, index + 1)
end

function currentCharCode() 
  return string.byte(currentChar())
end

function nextCharCode() 
  return string.byte(nextChar())
end

function moveNext() 
  index = index + 1
end

function move(number) 
  index = index + number
end



function lex()
  skipWhiteSpace()

  -- Skip comments beginning with --
  while 
    45 == currentCharCode() 
    and 45 == nextCharCode()
  do
    scanComment()
    skipWhiteSpace()
  end

  if index > length then
    return {
      type = EOF,
      value = '<eof>',
      line = line,
      lineStart = lineStart,
      range = { index, index },
    }
  end

  local charCode = currentCharCode()
  local next = nextCharCode()

  -- Memorize the range index where the token begins.
  tokenStart = index
  if isIdentifierStart(charCode) then
    local x = scanIdentifierOrKeyword()
    return x
  end

  if charCode == 39 or charCode == 34 then
    -- '"
    return scanStringLiteral()


  elseif charCode >= 48 and charCode <= 57 then
    -- 0-9
    return scanNumericLiteral()


  elseif charCode == 46 then
    -- .
    -- If the dot is followed by a digit it's a float.
    if isDecDigit(next) then
      return scanNumericLiteral()
    end

    if 46 == next then
      if 46 == charCodeAt(input, index + 2) then 
        return scanVarargLiteral()
      else
        return scanPunctuator('..')
      end
    else
      return scanPunctuator('.')
    end


  elseif charCode == 61 then
    -- =
    if 61 == next then
      return scanPunctuator('==')
    else
      return scanPunctuator('=')
    end


  elseif charCode == 62 then
    -- >
    if 61 == next then 
      return scanPunctuator('>=')
    elseif 62 == next then
      return scanPunctuator('>>')
    else 
      return scanPunctuator('>')
    end


  elseif charCode == 60 then
    -- <
    if 60 == next then 
      return scanPunctuator('<<')
    elseif 61 == next then
      return scanPunctuator('<=')
    else 
      return scanPunctuator('<')
    end


  elseif charCode == 126 then
    -- ~
    if 61 == next then
      return scanPunctuator('~=')
    else
      return scanPunctuator('~')
    end


  elseif charCode == 58 then
    -- :
    if 58 == next then
      return scanPunctuator('::')
    else
      return scanPunctuator(':')
    end


  elseif charCode == 91 then
    -- [
    -- Check for a multiline string, they begin with [= or [[
    if 91 == next or 61 == next then 
      return scanLongStringLiteral()
    else
      return scanPunctuator('[')
    end


  elseif charCode == 47 then
    -- /
    -- Check for integer division op (--)
    if 47 == next then 
      return scanPunctuator('--')
    else
      return scanPunctuator('/')
    end


  elseif
    charCode == 35
    or charCode == 37
    or charCode == 38
    or (charCode >= 40 and charCode <= 45) 
    or charCode == 59 
    or charCode == 93 
    or charCode == 94 
    or (charCode >= 123 and charCode <= 125)
  then
    -- * ^ % , { } ] ( ) ; & # - + |
    return scanPunctuator(currentChar())


  else
    return unexpected(currentChar())
  end
end




-- Whitespace has no semantic meaning in lua so simply skip ahead while
-- tracking the encounted newlines. Any kind of eol sequence is counted as a
-- single line.

function consumeEOL()
  local charCode = currentCharCode()
  local peekCharCode = nextCharCode()

  if isLineTerminator(charCode) then
    -- Count \n\r and \r\n as one newline.
    if 
      (10 == charCode and 13 == peekCharCode)
      or (13 == charCode and 10 == peekCharCode) 
    then
      moveNext()
    end

    line = line + 1
    moveNext()
    lineStart = index
 
    return true;

  else
    return false;
  end
end


function skipWhiteSpace() 
  while index <= length do
    local charCode = currentCharCode()

    if isWhiteSpace(charCode) then
      index = index + 1
    elseif not consumeEOL() then
      break
    end
  end
end


-- Identifiers, keywords, booleans and nil all look the same syntax wise. We
-- simply go through them one by one and defaulting to an identifier if no
-- previous case matched.

function scanIdentifierOrKeyword()
  local value, _type

  -- Slicing the input string is prefered before string concatenation in a
  -- loop for performance reasons.
  moveNext()
  while isIdentifierPart(currentCharCode()) do
    moveNext()
  end

  value = string.sub(input, tokenStart, index - 1)

  -- Decide on the token type and possibly cast the value.
  if isKeyword(value) then
    _type = Keyword

  elseif 'true' == value or 'false' == value then
    _type = BooleanLiteral
    value = ('true' == value)

  elseif 'nil' == value then
    _type = NilLiteral
    value = null

  else
    _type = Identifier
  end

  return {
    type = _type,
    value = value,
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  };
end

-- Once a punctuator reaches this function it should already have been
-- validated so we simply return it as a token.

function scanPunctuator(value)
  index = index + #value
  return {
    type = Punctuator,
    value = value,
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  }
end

-- A vararg literal consists of three dots.

function scanVarargLiteral()
  index = index + 3
  return {
    type = VarargLiteral,
    value = '...',
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  }
end

-- Find the string literal by matching the delimiter marks used.

function scanStringLiteral()
  local delimiter = charCodeAt(input, index)
  moveNext()
  local stringStart = index
  local string = ''

  while index <= length do
    local charCode = charCodeAt(input, index)
    moveNext()

    if delimiter == charCode then
      break
    end

    if 92 == charCode then
      -- \
      string = string..string.sub(input, stringStart, index - 2)..readEscapeSequence()
      stringStart = index
    -- EOF or `\n` terminates a string literal. If we haven't found the
    -- ending delimiter by now, raise an exception.
    elseif index > length or isLineTerminator(charCode) then    
      string = string..string.sub(input, stringStart, index - 2)
      raise({}, errors.unfinishedString, string..string.char(charCode))
    end
  end

  string = string..string.sub(input, stringStart, index - 2)

  return {
    type = StringLiteral,
    value = string,
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  };
end


-- Expect a multiline string literal and return it as a regular string
-- literal, if it doesn't validate into a valid multiline string, throw an
-- exception.

function scanLongStringLiteral()
  local str = readLongString()
  -- Fail if it's not a multiline literal.
  if false == str then
    raise(token, errors.expected, '[', token.value)
  end

  return {
    type = StringLiteral,
    value = str,
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  }
end

-- Numeric literals will be returned as floating-point numbers instead of
-- strings. The raw value should be retrieved from slicing the input string
-- later on in the process.
--
-- If a hexadecimal number is encountered, it will be converted.

function scanNumericLiteral()
  local character = charAt(input, index)
  local next = charAt(input, index + 1)
  local value 
  
  if 
    '0' == character 
    and (next == 'x' or next == 'X')
  then
    value = readHexLiteral()
  else 
    value = readDecLiteral()
  end

  return {
    type = NumericLiteral,
    value = value,
    line = line,
    lineStart = lineStart,
    range = { tokenStart, index },
  }
end

-- Lua hexadecimals have an optional fraction part and an optional binary
-- exoponent part. These are not included in JavaScript so we will compute
-- all three parts separately and then sum them up at the end of the function
-- with the following algorithm.
--
--     Digit := toDec(digit)
--     Fraction := toDec(fraction) / 16 ^ fractionCount
--     BinaryExp := 2 ^ binaryExp
--     Number := ( Digit + Fraction ) * BinaryExp

function readHexLiteral()
  local fraction = 0 -- defaults to 0 as it gets summed
  local binaryExponent = 1 -- defaults to 1 as it gets multiplied
  local binarySign = 1 -- positive
  local digit, fractionStart, exponentStart, digitStart

  move(2) -- Skip 0x part
  digitStart = index

  -- A minimum of one hex digit is required.
  if not isHexDigit(charCodeAt(input, index)) then
    raise({}, errors.malformedNumber, string.sub(input, tokenStart, index))
  end

  while isHexDigit(charCodeAt(input, index)) do 
    moveNext()
  end

  -- Convert the hexadecimal digit to base 10.
  digit = tonumber(string.sub(input, digitStart, index - 1), 16)

  -- Fraction part i optional.
  if '.' == charAt(input, index) then
    moveNext()
    fractionStart = index

    while isHexDigit(charCodeAt(input, inddex)) do
      moveNext()
    end

    fraction = string.sub(input, fractionStart, index - 1)

    -- Empty fraction parts should default to 0, others should be converted
    -- 0.x form so we can use summation at the end.
    if fractionStart == index then
      fraction = 0
    else
      fraction = tonumber(fraction, 16) / math.pow(16, index - fractionStart)
    end
  end

  -- Binary exponents are optional
  local char = charAt(input, index)

  if string.lower(char) == 'p' then
    moveNext()

    -- Sign part is optional and defaults to 1 (positive).
    char = charAt(input, index)
    if char == '+' or char == '-' then
      binarySign = (char == '+' and 1 or -1)
      moveNext()
    end

    exponentStart = index;

    -- The binary exponent sign requires a decimal digit.
    if not isDecDigit(charCodeAt(input, index)) then
      raise({}, errors.malformedNumber, string.sub(input, tokenStart, index))
    end

    while isDecDigit(charCodeAt(input, index)) do
      moveNext()
    end

    binaryExponent = string.sub(input, exponentStart, index - 1)

    -- Calculate the binary exponent of the number.
    binaryExponent = math.pow(2, binaryExponent * binarySign)
  end

  return (digit + fraction) * binaryExponent
end

-- Decimal numbers are exactly the same in Lua and in JavaScript, because of
-- this we check where the token ends and then parse it with native
-- functions.

function readDecLiteral()
  while isDecDigit(currentCharCode()) do
    moveNext()
  end

  -- Fraction part is optional
  if '.' == currentChar() then
    moveNext()

    -- Fraction part defaults to 0
    while isDecDigit(currentCharCode()) do
      moveNext()
    end
  end

  -- Exponent part is optional.
  local char = currentChar()
  if char == 'e' or char == 'E' then
    moveNext()

    -- Sign part is optional.
    char = currectChar()
    if char == '+' or char == '-' then 
      moveNext()
    end

    -- An exponent is required to contain at least one decimal digit.
    local charCode = currentCharCode()
    if not isDecDigit(char) then
      raise({}, errors.malformedNumber, string.sub(input, tokenStart, index - 1))
    end

    while isDecDigit(currentCharCode) do
      moveNext()
    end
  end

  return tonumber(string.sub(input, tokenStart, index - 1))
end


-- Translate escape sequences to the actual characters.

function readEscapeSequence() 
  local sequenceStart = index

  local char = currentChar()
  local result

  -- Lua allow the following escape sequences.
  -- We don't escape the bell sequence.
  if char == 'n' then
    result = '\n'
  elseif char == 'r' then
    result = '\r'
  elseif char == 't' then 
    result = '\t'
  elseif char == 'v' then
    result = '\x0B'
  elseif char == 'b' then 
    result = '\b'
  elseif char == 'f' then 
    result = '\f'
  elseif char == 'z' then 
    -- Skips the following span of white-space.
    skipWhiteSpace() 
    result = ''
  elseif char == 'x' then 
    -- Byte representation should for now be returned as is.
    -- \xXX, where XX is a sequence of exactly two hexadecimal digits
    if 
      isHexDigit(nextCharCode())
      and isHexDigit(charCodeAt(input, index + 2)) 
    then
      move(2)
      -- Return it as is, without translating the byte.
      result = '\\'..string.sub(input, sequenceStart, index - 1)
    end
    result = '\\'..currentChar()
  else
    -- \ddd, where ddd is a sequence of up to three decimal digits.
    if isDecDigit(currentCharCode()) then
      moveNext()
      while isDecDigit(currentCharCode()) do
        moveNext()
      end
      return '\\'..string.sub(input, sequenceStart, index - 1)
    end

    -- Simply return the \ as is, it's not escaping any sequence.
    result = currentChar()
  end

  if result then
    moveNext()
  end

  return result
end

-- Comments begin with -- after which it will be decided if they are
-- multiline comments or not.
--
-- The multiline functionality works the exact same way as with string
-- literals so we reuse the functionality.

function scanComment()
  local tokenStart = index
  move(2)

  local character = currentChar()
  local content = ''
  local isLong = false
  local commentStart = index
  local lineStartComment = lineStart
  local lineComment = line

  if '[' == character then
    content = readLongString()
    -- This wasn't a multiline comment after all.
    if false == content then 
      content = character
    else 
      isLong = true
    end
  end

  -- Scan until next line as long as it's not a multiline comment.
  if not isLong then
    while index <= length do
      if isLineTerminator(currentCharCode()) then 
        break
      end

      moveNext()
    end

    if options.comments then
      content = string.sub(input, commentStart, index - 1)
    end
  end

  if options.comments then
    local node = ast.comment(content, string.sub(input, tokenStart, index - 1))

    -- `Marker`s depend on tokens available in the parser and as comments are
    -- intercepted in the lexer all location data is set manually.
    if options.locations then
      node.loc = {
        start = { 
          line = lineComment, 
          column = tokenStart - lineStartComment 
        },
        ['end'] = {
          line = line, 
          column = index - lineStart
        }
      }
    end

    if options.ranges then
      node.range = { tokenStart, index }
    end

    if options.onCreateNode then 
      options.onCreateNode(node)
    end

    table.insert(comments, node)
  end
end

-- Read a multiline string by calculating the depth of `=` characters and
-- then appending until an equal depth is found.

function readLongString()
  local level = 0
  local content = ''
  local terminator = false
  local character, stringStart

  moveNext()

  -- Calculate the depth of the comment.
  while '=' == charAt(input, index + level) do
    level = level + 1
  end

  -- Exit, this is not a long string afterall.
  if '[' ~= charAt(input, index + level) then 
    return false
  end

  move(level + 1)

  -- If the first character is a newline, ignore it and begin on next line.
  if isLineTerminator(currentCharCode()) then 
    consumeEOL()
  end

  stringStart = index

  while index <= length do
    -- To keep track of line numbers run the `consumeEOL()` which increments
    -- its counter.
    if isLineTerminator(currentCharCode()) then 
      consumeEOL()
    end

    character = currentChar()
    moveNext()

    -- Once the delimiter is found, iterate through the depth count and see
    -- if it matches.
    if ']' == character then
      terminator = true
      
      for i = 0, level - 1 do
        if '=' ~= charAt(input, index + i) then
          terminator = false
        end
      end

      if ']' ~= charAt(input, index + level) then
        terminator = false
      end
    end

    -- We reached the end of the multiline string. Get out now.
    if terminator then 
      break
    end
  end

  content = content string.sub(input, stringStart, index - 2)
  move(level + 1)

  return content
end


-- ## Lex functions and helpers.

-- Read the next token.
--
-- This is actually done by setting the current token to the lookahead and
-- reading in the new lookahead token.

function next() 
  previousToken = token
  token = lookahead
  lookahead = lex()
end

-- Consume a token if its value matches. Once consumed or not, return the
-- success of the operation.

function consume(value)
  if value == token.value then
    next()
    return true
  end

  return false
end


-- Expect the next token value to match. If not, throw an exception.

function expect(value)
  if value == token.value then 
    next()
  else 
    raise(token, errors.expected, value, token.value)
  end
end

-- ### Validation functions

function isWhiteSpace(charCode)
  return 9 == charCode or 32 == charCode or 0xB == charCode or 0xC == charCode
end

function isLineTerminator(charCode)
  return 10 == charCode or 13 == charCode
end

function isDecDigit(charCode)
  return charCode >= 48 and charCode <= 57
end

function isHexDigit(charCode)
  return (charCode >= 48 and charCode <= 57) or (charCode >= 97 and charCode <= 102) or (charCode >= 65 and charCode <= 70)
end

-- From [Lua 5.2](http:--www.lua.org/manual/5.2/manual.html#8.1) onwards
-- identifiers cannot use locale-dependet letters.

function isIdentifierStart(charCode)
  return (charCode >= 65 and charCode <= 90) or (charCode >= 97 and charCode <= 122) or 95 == charCode;
end

function isIdentifierPart(charCode)
  if charCode == nil then 
    return false
  end

  return (charCode >= 65 and charCode <= 90) or (charCode >= 97 and charCode <= 122) or 95 == charCode or (charCode >= 48 and charCode <= 57);
end

-- [3.1 Lexical Conventions](http:--www.lua.org/manual/5.2/manual.html#3.1)
--
-- `true`, `false` and `nil` will not be considered keywords, but literals.

function isKeyword(id)
  local length = #id
  if length == 2 then
    return 'do' == id or 'if' == id or 'in' == id or 'or' == id
  elseif length == 3 then
    return 'and' == id or 'end' == id or 'for' == id or 'not' == id
  elseif length == 4 then
    return 'else' == id or 'goto' == id or 'then' == id
  elseif length == 5 then
    return 'break' == id or 'local' == id or 'until' == id or 'while' == id
  elseif length == 6 then
    return 'elseif' == id or 'repeat' == id or 'return' == id
  elseif length == 8 then
    return 'function' == id
  end
  return false
end

function isUnary(token)
  if Punctuator == token.type then
    local value = token.value
    return value == '#' or value == '-' or value == '~'
  end

  if Keyword == token.type then
    return 'not' == token.value
  end

  return false
end

-- @TODO this needs to be rethought.
function isCallExpression(expression)
  local type = expression.type
  return type == 'CallExpression' or type == 'TableCallExpression' or type == 'StringCallExpression'
end

-- Check if the token syntactically closes a block.

function isBlockFollow(token)
  if EOF == token.type then
    return true
  end

  if Keyword ~= token.type then
    return false
  end

  local value = token.value
  return value == 'else' or value == 'elseif' or value == 'end' or value == 'until'
end

-- Scope
-- -----

-- Store each block scope as a an array of identifier names. Each scope is
-- stored in an FILO-array.
local scopes

-- The current scope index
local scopeDepth

  -- A list of all global identifier nodes.
local globals


-- Create a new scope inheriting all declarations from the previous scope.
function createScope()
  local scope = scopes[scopeDepth]
  scopeDepth = scopeDepth + 1

  table.insert(scopes, scope)
  if options.onCreateScope then 
    options.onCreateScope()
  end
end

-- Exit and remove the current scope.
function destroyScope()
  local scope = table.remove(scopes)
  scopeDepth = scopeDepth - 1

  if options.onDestroyScope then 
    options.onDestroyScope()
  end
end

-- Add identifier name to the current scope if it doesnt already exist.
function scopeIdentifierName(name)
  if 0 ~= indexOf(scopes[scopeDepth], name) then 
    return
  end
  table.insert(scopes[scopeDepth], name)
end

-- Add identifier to the current scope
function scopeIdentifier(node)
  scopeIdentifierName(node.name)
  attachScope(node, true)
end

-- Attach scope information to node. If the node is global, store it in the
-- globals array so we can return the information to the user.
function attachScope(node, isLocal)
  if not isLocal and 0 == indexOfObject(globals, 'name', node.name) then
    table.insert(globals, node)
  end

  node.isLocal = isLocal
end

-- Is the identifier name available in this scope.
function scopeHasName(name)
  return 0 ~= indexOf(scopes[scopeDepth], name)
end

-- Location tracking
-- -----------------
--
-- Locations are stored in FILO-array as a `Marker` object consisting of both
-- `loc` and `range` data. Once a `Marker` is popped off the list an end
-- location is added and the data is attached to a syntax node.

local locations = {}
local trackLocations
local Marker = {}
Marker.__index = Marker

function Marker.new(token)
  local self = setmetatable({}, Marker)

  if (options.locations) then
    self.loc = {
      start = {
        line = token.line,
        column = token.range[0] - token.lineStart,
      },
      ['end'] = {
        line = 0,
        column = 0,
      }
    }
  end

  if options.ranges then 
    this.range = { token.range[0], 0 }
  end

  return self
end


function createLocationMarker()
  return Marker.new(token)
end


-- Complete the location data stored in the `Marker` by adding the location
-- of the *previous token* as an end location.
Marker.complete = function()
  if options.locations then
    self.loc['end'].line = previousToken.line
    self.loc['end'].column = previousToken.range[2] - previousToken.lineStart
  end

  if options.ranges then
    self.range[2] = previousToken.range[2]
  end
end

-- Create a new `Marker` and add it to the FILO-array.
function markLocation()
  if trackLocations then
    table.insert(locations, createLocationMarker())
  end
end

-- Push an arbitrary `Marker` object onto the FILO-array.
function pushLocation(marker)
  if trackLocations then
    table.insert(locations, marker)
  end
end



-- Parse functions
-- ---------------

-- Chunk is the main program object. Syntactically it's the same as a block.
--
--     chunk ::= block

function parseChunk()
  next()
  markLocation()
  
  if options.scope then 
    createScope()
  end

  local body = parseBlock()

  if options.scope then 
    destroyScope()
  end

  if EOF ~= token.type then 
    unexpected(token)
  end

  -- If the body is empty no previousToken exists when finishNode runs.
  if trackLocations and #body == 0 then
    previousToken = token
  end

  return finishNode(ast.chunk(body))
end

-- A block contains a list of statements with an optional return statement
-- as its last statement.
--
--     block ::= {stat} [retstat]

function parseBlock(terminator)
  local block = {}
  local statement

  while not isBlockFollow(token) do
    -- Return has to be the last statement in a block.
    if 'return' == token.value then
      table.insert(block, parseStatement())
      break
    end

    statement = parseStatement()
    
    -- Statements are only added if they are returned, this allows us to
    -- ignore some statements, such as EmptyStatement.
    if statement then
      table.insert(block, statement)
    end
  end

  -- Doesn't really need an ast node
  return block
end


-- There are two types of statements, simple and compound.
--
--     statement ::= break | goto | do | while | repeat | return
--          | if | for | function | local | label | assignment
--          | functioncall | ';'

function parseStatement()
  markLocation()
  if Keyword == token.type then
    local result
    if token.value == 'local' then
      next()
      return parseLocalStatement()

    elseif token.value == 'if' then
      next()
      return parseIfStatement()

    elseif token.value == 'return' then
      next()
      return parseReturnStatement()

    elseif token.value == 'function' then
      next()
      local name = parseFunctionName()
      return parseFunctionDeclaration(name)

    elseif token.value == 'while' then
      next()
      return parseWhileStatement()

    elseif token.value == 'for' then
      next()
      return parseForStatement()

    elseif token.value == 'repeat' then
      next()
      return parseRepeatStatement()

    elseif token.value == 'break' then
      next()
      return parseBreakStatement()

    elseif token.value == 'do' then
      next()
      return parseDoStatement()

    elseif token.value == 'goto' then
      next()
      return parseGotoStatement()
    end
  end

  if Punctuator == token.type then
    if consume('::') then 
      return parseLabelStatement()
    end
  end

  -- Assignments memorizes the location and pushes it manually for wrapper
  -- nodes. Additionally empty `;` statements should not mark a location.
  if trackLocations then
    table.remove(locations)
  end

  -- When a `;` is encounted, simply eat it without storing it.
  if consume(';') then 
    return
  end

  return parseAssignmentOrCallStatement()
end




-- ## Statements

--     label ::= '::' Name '::'

function parseLabelStatement()
  local name = token.value
  local label = parseIdentifier()

  if options.scope then
    scopeIdentifierName('::'..name..'::')
    attachScope(label, true)
  end

  expect('::')
  return finishNode(ast.labelStatement(label))
end

--     break ::= 'break'

function parseBreakStatement()
  return finishNode(ast.breakStatement())
end

--     goto ::= 'goto' Name

function parseGotoStatement()
  local name = token.value
  local label = parseIdentifier()

  return finishNode(ast.gotoStatement(label))
end

--     do ::= 'do' block 'end'

function parseDoStatement()
  if options.scope then
    createScope()
  end

  local body = parseBlock()

  if options.scope then
    destroyScope()
  end

  expect('end')

  return finishNode(ast.doStatement(body))
end

--     while ::= 'while' exp 'do' block 'end'

function parseWhileStatement()
  local condition = parseExpectedExpression()

  expect('do')

  if options.scope then 
    createScope()
  end

  local  body = parseBlock()

  if options.scope then
    destroyScope()
  end

  expect('end')

  return finishNode(ast.whileStatement(condition, body))
end


--     repeat ::= 'repeat' block 'until' exp

function parseRepeatStatement()
  if options.scope then
    createScope()
  end

  local body = parseBlock()
  expect('until')

  local condition = parseExpectedExpression()

  if options.scope then 
    destroyScope()
  end

  return finishNode(ast.repeatStatement(condition, body))
end

--     retstat ::= 'return' [exp {',' exp}] [';']

function parseReturnStatement()
  local expressions = {}

  if 'end' ~= token.value then
    local expression = parseExpression()
    if null ~= expression then
      table.insert(expressions, expression)
    end

    while consume(',') do
      expression = parseExpectedExpression()
      table.insert(expressions, expression)
    end
    consume(';') -- grammar tells us ; is optional here.
  end
  return finishNode(ast.returnStatement(expressions))
end

--     if ::= 'if' exp 'then' block {elif} ['else' block] 'end'
--     elif ::= 'elseif' exp 'then' block

function parseIfStatement() 
  local clauses = {}
  local condition
  local body
  local marker

  -- IfClauses begin at the same location as the parent IfStatement.
  -- It ends at the start of `end`, `else`, or `elseif`.
  if trackLocations then
    marker = locations[#locations]
    table.insert(locations, marker)
  end
  
  condition = parseExpectedExpression()
  expect('then')
  
  if options.scope then
    createScope()
  end

  body = parseBlock()
  if options.scope then
    destroyScope()
  end

  table.insert(clauses, finishNode(ast.ifClause(condition, body)))

  if trackLocations then
    marker = createLocationMarker()
  end

  while consume('elseif') do
    pushLocation(marker)
    condition = parseExpectedExpression()
    expect('then')
    
    if options.scope then 
      createScope()
    end

    body = parseBlock()

    if options.scope then 
      destroyScope()
    end

    table.insert(clauses, finishNode(ast.elseifClause(condition, body)))

    if trackLocations then 
      marker = createLocationMarker()
    end
  end

  if consume('else') then
    -- Include the `else` in the location of ElseClause.
    if trackLocations then
      marker = Marker.new(previousToken)
      table.insert(locations, marker)
    end

    if options.scope then 
      createScope()
    end

    body = parseBlock()

    if options.scope then
      destroyScope()
    end

    table.insert(clauses, finishNode(ast.elseClause(body)))
  end

  expect('end')
  return finishNode(ast.ifStatement(clauses))
end


-- There are two types of for statements, generic and numeric.
--
--     for ::= Name '=' exp ',' exp [',' exp] 'do' block 'end'
--     for ::= namelist 'in' explist 'do' block 'end'
--     namelist ::= Name {',' Name}
--     explist ::= exp {',' exp}

function parseForStatement()
  local variable = parseIdentifier()
  local body

  -- The start-identifier is local.

  if options.scope then
    createScope()
    scopeIdentifier(variable)
  end

  -- If the first expression is followed by a `=` punctuator, this is a
  -- Numeric For Statement.
  if consume('=') then
    -- Start expression
    local start = parseExpectedExpression()
    expect(',')
    -- End expression
    local _end = parseExpectedExpression()
    -- Optional step expression
    local step = consume(',') and parseExpectedExpression() or {}

    expect('do')
    body = parseBlock()
    expect('end')

    if options.scope then
      destroyScope()
    end

    return finishNode(ast.forNumericStatement(variable, start, _end, step, body))
  
  else
    -- If not, it's a Generic For Statement
    -- The namelist can contain one or more identifiers.
    local variables = { variable }

    while consume(',') do
      variable = parseIdentifier()

      -- Each variable in the namelist is locally scoped.
      if options.scope then 
        scopeIdentifier(variable)
      end

      table.insert(variables, variable)
    end

    expect('in')
    local iterators = {}

    -- One or more expressions in the explist.
    repeat
      local expression = parseExpectedExpression()
      table.insert(iterators, expression)
    until not consume(',')

    expect('do')
    body = parseBlock()
    expect('end')

    if options.scope then
      destroyScope()
    end

    return finishNode(ast.forGenericStatement(variables, iterators, body))
  end
end


-- Local statements can either be variable assignments or function
-- definitions. If a function definition is found, it will be delegated to
-- `parseFunctionDeclaration()` with the isLocal flag.
--
-- This AST structure might change into a local assignment with a function
-- child.
--
--     local ::= 'local' 'function' Name funcdecl
--        | 'local' Name {',' Name} ['=' exp {',' exp}]

function parseLocalStatement()
  local name

  if Identifier == token.type then
    local variables = {}
    local init = {}

    repeat
      name = parseIdentifier()
      table.insert(variables, name)
    until not consume(',')

    if consume('=') then
      repeat
        local expression = parseExpectedExpression()
        table.insert(init, expression)
      until not consume(',')
    end

    -- Declarations doesn't exist before the statement has been evaluated.
    -- Therefore assignments can't use their declarator. And the identifiers
    -- shouldn't be added to the scope until the statement is complete.
    if options.scope then
      for i = 1, #variables do
        scopeIdentifier(variables[i])
      end
    end

    return finishNode(ast.localStatement(variables, init))
  end

  if consume('function') then
    name = parseIdentifier()

    if options.scope then
      scopeIdentifier(name)
      createScope()
    end

    -- MemberExpressions are not allowed in local function statements.
    return parseFunctionDeclaration(name, true)

  else
    raiseUnexpectedToken('<name>', token)
  end
end


function validateVar(node)
  -- @TODO we need something not dependent on the exact AST used. see also isCallExpression()
  if node.inParens or (node.type ~= 'Identifier' and node.type ~= 'MemberExpression' and node.type ~= 'IndexExpression') then
    raise(token, errors.invalidVar, token.value)
  end
end



--     assignment ::= varlist '=' explist
--     var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
--     varlist ::= var {',' var}
--     explist ::= exp {',' exp}
--
--     call ::= callexp
--     callexp ::= prefixexp args | prefixexp ':' Name args

function parseAssignmentOrCallStatement()
  -- Keep a reference to the previous token for better error messages in case
  -- of invalid statement
  local previous = token
  local expression 
  local marker

  if trackLocations then 
    marker = createLocationMarker()
  end

  expression = parsePrefixExpression()

  if nil == expression then
    return unexpected(token)
  end

  if token.value == ',' or token.value == '=' then
    local variables = { expression }
    local init = {}
    local exp

    validateVar(expression)

    while consume(',') do
      exp = parsePrefixExpression()
      
      if null == exp then
        raiseUnexpectedToken('<expression>', token)
      end

      validateVar(exp)
      table.insert(variables, exp)
    end

    expect('=')

    repeat
      exp = parseExpectedExpression()
      table.insert(init, exp)
    until not consume(',')

    pushLocation(marker)
    return finishNode(ast.assignmentStatement(variables, init))
  end

  if isCallExpression(expression) then
    pushLocation(marker)
    return finishNode(ast.callStatement(expression))
  end

  -- The prefix expression was neither part of an assignment or a
  -- callstatement, however as it was valid it's been consumed, so raise
  -- the exception on the previous token to provide a helpful message.
  return unexpected(previous)
end




-- ### Non-statements

--     Identifier ::= Name

function parseIdentifier()
  markLocation()
  local identifier = token.value

  if Identifier ~= token.type then
    raiseUnexpectedToken('<name>', token)
  end

  next()
  return finishNode(ast.identifier(identifier))
end

-- Parse the functions parameteras and body block. The name should already
-- have been parsed and passed to this declaration function. By separating
-- this we allow for anonymous functions in expressions.
--
-- For local functions there's a boolean parameter which needs to be set
-- when parsing the declaration.
--
--     funcdecl ::= '(' [parlist] ')' block 'end'
--     parlist ::= Name {',' Name} | [',' '...'] | '...'

function parseFunctionDeclaration(name, isLocal)
  local parameters = {}
  expect('(')

  -- The declaration has arguments
  if not consume(')') then
    -- Arguments are a comma separated list of identifiers, optionally ending
    -- with a vararg.
    while true do
      local continue = false

      if Identifier == token.type then
        local parameter = parseIdentifier()
        -- Function parameters are local.
        if options.scope then
          scopeIdentifier(parameter)
        end

        table.insert(parameters, parameter)

        if consume(',') then
          -- continue (noop)
        elseif consume(')') then
          break
        end

      elseif VarargLiteral == token.type then
        -- No arguments are allowed after a vararg.
        table.insert(parameters, parsePrimaryExpression())
        expect(')')
        break
      else
        raiseUnexpectedToken('<name> or \'...\'', token)
      end
    end
  end

  local body = parseBlock()
  expect('end')

  if options.scope then
    destroyScope()
  end
  
  isLocal = isLocal or false
  return finishNode(ast.functionStatement(name, parameters, isLocal, body))
end

-- Parse the function name as identifiers and member expressions.
--
--     Name {'.' Name} [':' Name]

function parseFunctionName()
  local base
  local name
  local marker

  if trackLocations then
    marker = createLocationMarker()
  end

  base = parseIdentifier()

  if options.scope then
    attachScope(base, scopeHasName(base.name))
    createScope()
  end

  while consume('.') do
    pushLocation(marker)
    name = parseIdentifier()
    base = finishNode(ast.memberExpression(base, '.', name))
  end

  if consume(':') then
    pushLocation(marker)
    name = parseIdentifier()
    base = finishNode(ast.memberExpression(base, ':', name))

    if options.scope then
      scopeIdentifierName('self')
    end
  end

  return base
end

--     tableconstructor ::= '{' [fieldlist] '}'
--     fieldlist ::= field {fieldsep field} fieldsep
--     field ::= '[' exp ']' '=' exp | Name = 'exp' | exp
--
--     fieldsep ::= ',' | ';'

function parseTableConstructor()
  local fields = {}
  local key
  local value

  while true do
    markLocation()
    if Punctuator == token.type and consume('[') then
      key = parseExpectedExpression()
      expect(']')
      expect('=')
      value = parseExpectedExpression()
      table.insert(fields, finishNode(ast.tableKey(key, value)))

    elseif Identifier == token.type then
      if '=' == lookahead.value then
        key = parseIdentifier()
        next()
        value = parseExpectedExpression()
        table.insert(fields, finishNode(ast.tableKeyString(key, value)))
      else
        value = parseExpectedExpression()
        table.insert(fields, finishNode(ast.tableValue(value)))
      end

    else
      value = parseExpression()
      if nil == value then
        table.remove(locations)
        break
      end
      table.insert(fields, finishNode(ast.tableValue(value)))
    end

    if token.value == ',' or token.value == ';' then
      next()
    else
      break
    end
  end

  expect('}')
  return finishNode(ast.tableConstructorExpression(fields))
end



-- Expression parser
-- -----------------
--
-- Expressions are evaluated and always return a value. If nothing is
-- matched null will be returned.
--
--     exp ::= (unop exp | primary | prefixexp ) { binop exp }
--
--     primary ::= nil | false | true | Number | String | '...'
--          | functiondef | tableconstructor
--
--     prefixexp ::= (Name | '(' exp ')' ) { '[' exp ']'
--          | '.' Name | ':' Name args | args }
--

function parseExpression()
  local expression = parseSubExpression(0)
  return expression
end

-- Parse an expression expecting it to be valid.

function parseExpectedExpression()
  local expression = parseExpression()

  if nil == expression then
    raiseUnexpectedToken('<expression>', token)
  else 
    return expression;
  end
end


-- Return the precedence priority of the operator.
--
-- As unary `-` can't be distinguished from binary `-`, unary precedence
-- isn't described in this table but in `parseSubExpression()` itself.
--
-- As this function gets hit on every expression it's been optimized due to
-- the expensive CompareICStub which took ~8% of the parse time.

function binaryPrecedence(operator)
  local charCode = charCodeAt(operator, 1)
  local length = #operator

  if 1 == length then
    if charCode == 94 then
      return 12 -- ^
    elseif charCode == 42 or charCode == 47 or charCode == 37 then
      return 10 -- * / %
    elseif charCode == 43 or charCode == 45 then
      return 9 -- + -
    elseif charCode == 38 then
      return 6 -- &
    elseif charCode == 126 then
      return 5 -- ~
    elseif charCode == 124 then
      return 4 -- |
    elseif charCode == 60 or charCode == 62 then
      return 3 -- < >
    end

  elseif 2 == length then
    if charCode == 47 then
      return 10 -- --
    elseif charCode == 46 then
      return 8 -- ..
    elseif charCode == 60 or charCode == 62 then
      if '<<' == operator or '>>' == operator then
        return 7 -- << >>
      else
        return 3 -- <= >=
      end
    elseif charCode == 61 or charCode == 126 then
      return 3 -- == ~=
    elseif charCode == 111 then
      return 1 -- or
    end

  elseif 97 == charCode and 'and' == operator then
    return 2
  end

  return 0
end

-- Implement an operator-precedence parser to handle binary operator
-- precedence.
--
-- We use this algorithm because it's compact, it's fast and Lua core uses
-- the same so we can be sure our expressions are parsed in the same manner
-- without excessive amounts of tests.
--
--     exp ::= (unop exp | primary | prefixexp ) { binop exp }

function parseSubExpression(minPrecedence)
  local operator = token.value
  -- The left-hand side in binary operations.
  local expression
  local marker

  if trackLocations then
    marker = createLocationMarker()
  end

  -- UnaryExpression
  if isUnary(token) then
    markLocation()
    next()
    local argument = parseSubExpression(10)

    if argument == nil then 
      raiseUnexpectedToken('<expression>', token)
    end

    expression = finishNode(ast.unaryExpression(operator, argument))
  end

  if nil == expression then
    -- PrimaryExpression
    expression = parsePrimaryExpression()
    -- PrefixExpression
    if nil == expression then
      expression = parsePrefixExpression()
    end
  end

  -- This is not a valid left hand expression.
  if nil == expression then
    return
  end

  local precedence
  while true do
    operator = token.value

    if Punctuator == token.type or Keyword == token.type then
      precedence = binaryPrecedence(operator)
    else
      precedence = 0
    end

    if precedence == 0 or precedence <= minPrecedence then
      break
    end

    -- Right-hand precedence operators
    if '^' == operator or '..' == operator then
      precedence = precedence - 1
    end

    next()
    local right = parseSubExpression(precedence)

    if nil == right then
      raiseUnexpectedToken('<expression>', token)
    end

    -- Push in the marker created before the loop to wrap its entirety.
    if trackLocations then
      table.insert(locations, marker)
    end

    expression = finishNode(ast.binaryExpression(operator, expression, right))
  end

  return expression
end




--     prefixexp ::= prefix {suffix}
--     prefix ::= Name | '(' exp ')'
--     suffix ::= '[' exp ']' | '.' Name | ':' Name args | args
--
--     args ::= '(' [explist] ')' | tableconstructor | String

function parsePrefixExpression()
  local base
  local name
  local marker

  if trackLocations then
    marker = createLocationMarker()
  end

  -- The prefix
  if Identifier == token.type then
    name = token.value
    base = parseIdentifier()

    -- Set the parent scope.
    if options.scope then
      attachScope(base, scopeHasName(name))
    end

  elseif consume('(') then
    base = parseExpectedExpression()
    expect(')')
    base.inParens = true -- XXX: quick and dirty. needed for validateVar
  
  else
    return
  end

  -- The suffix
  local expression
  local identifier

  while true do
    if Punctuator == token.type then
      if token.value == '[' then
        pushLocation(marker)
        next()
        expression = parseExpectedExpression()
        base = finishNode(ast.indexExpression(base, expression))
        expect(']')

      elseif token.value == '.' then
        pushLocation(marker)
        next()
        identifier = parseIdentifier()
        base = finishNode(ast.memberExpression(base, '.', identifier))

      elseif token.value == ':' then
        pushLocation(marker)
        next()
        identifier = parseIdentifier()
        base = finishNode(ast.memberExpression(base, ':', identifier))
        -- Once a : is found, this has to be a CallExpression, otherwise
        -- throw an error.
        pushLocation(marker)
        base = parseCallExpression(base)

      elseif token.value == '(' or token.value == '{' then
        pushLocation(marker)
        base = parseCallExpression(base)

      else
        return base
      end

    elseif StringLiteral == token.type then
      pushLocation(marker)
      base = parseCallExpression(base)
    else
      break
    end
  end

  return base
end

--     args ::= '(' [explist] ')' | tableconstructor | String

function parseCallExpression(base)
  if Punctuator == token.type then
    if token.value == '(' then
      next()

      -- List of expressions
      local expressions = {}
      local expression = parseExpression()

      if nil ~= expression then
        table.insert(expressions, expression)
      end

      while consume(',') do
        expression = parseExpectedExpression()
        table.insert(expressions, expression)
      end

      expect(')')
      return finishNode(ast.callExpression(base, expressions))

    elseif token.value == '{' then
      markLocation()
      next()
      local table = parseTableConstructor()
      return finishNode(ast.tableCallExpression(base, table))
    end

  elseif StringLiteral == token.type then
    return finishNode(ast.stringCallExpression(base, parsePrimaryExpression()))
  end

  raiseUnexpectedToken('function arguments', token)
end

--     primary ::= String | Numeric | nil | true | false
--          | functiondef | tableconstructor | '...'

function parsePrimaryExpression()
  local literals = bit.bor(StringLiteral, NumericLiteral, BooleanLiteral, NilLiteral, VarargLiteral)
  local value = token.value
  local type = token.type
  local marker

  if trackLocations then 
    marker = createLocationMarker()
  end

  if bit.band(type, literals) ~= 0 then
    pushLocation(marker)
    local raw = string.sub(input, token.range[1], token.range[2] - 1)
    next()
    return finishNode(ast.literal(type, value, raw))

  elseif Keyword == type and 'function' == value then
    pushLocation(marker)
    next()
    
    if options.scope then 
      createScope()
    end

    return parseFunctionDeclaration(nil)

  elseif consume('{') then
    pushLocation(marker)
    return parseTableConstructor()
  end
end




-- Parser
-- ------

-- Export the main parser.
--
--   - `wait` Hold parsing until end() is called. Defaults to false
--   - `comments` Store comments. Defaults to true.
--   - `scope` Track identifier scope. Defaults to false.
--   - `locations` Store location information. Defaults to false.
--   - `ranges` Store the start and end character locations. Defaults to
--     false.
--   - `onCreateNode` Callback which will be invoked when a syntax node is
--     created.
--   - `onCreateScope` Callback which will be invoked when a new scope is
--     created.
--   - `onDestroyScope` Callback which will be invoked when the current scope
--     is destroyed.
--
-- Example:
--
--     var parser = require('luaparser');
--     parser.parse('i = 0');

function parse(_input, _options)

  if type(_options) == 'nil' and type(_input) == 'table' then
    _options = _input
    _input = nil
  end

  if not _options then
    _options = {}
  end

  input = _input or ''
  options = extend(defaultOptions, _options)

  -- Rewind the lexer
  index = 1
  line = 1
  lineStart = 0
  length = #input
  -- When tracking identifier scope, initialize with an empty scope.
  scopes = {{}}
  scopeDepth = 0
  globals = {}
  locations = {}

  if options.comments then 
    comments = {}
  end

  if not options.wait then
    return terminate()
  end

  return Parser
end

-- Write to the source code buffer without beginning the parse.
function write(_input)
  input = input..tostring(_input)
  length = #input
  return Parser
end

-- Send an EOF and begin parsing.
function terminate(_input)
  if type(_input) ~= 'nil' then
    write(_input)
  end

  -- Ignore shebangs.
  if input and input.sub(1, 2) == '#!' then
    input = string.gsub(input, '^.*', function (line)
      return string.rep(' ', #line)
    end)
  end

  length = #input
  trackLocations = options.locations or options.ranges
  -- Initialize with a lookahead token.
  lookahead = lex()

  local chunk = parseChunk()

  if options.comments then
    chunk.comments = comments
  end

  if options.scope then
    chunk.globals = globals
  end

  if #locations > 0 then
    error('Location tracking failed. This is most likely a bug in luaparse')
  end

  return chunk
end




--]]



Parser.defaultOptions = defaultOptions
Parser.errors = errors
Parser.ast = ast
Parser.lex = lex
Parser.terminate = terminate
Parser.write = write
Parser.parse = parse

return Parser