
local Parser = require('parse')

function log(value, indent)
  indent = indent or 2

  if type(value) == 'table' then
    print('{')
    for i, v in pairs(value) do
      io.write(string.rep(' ', indent)..i..' = ')
      log(v, indent + 2)
    end
    print(string.rep(' ', indent - 2)..'}')

  elseif type(value) == 'nil' then
    print('nil')
  elseif type(value) == 'string' then
    print('"'..value..'"')
  else 
    print(value)
  end
end  
        

local code = 'print("hello world")'
local ast = Parser.parse(code)

log(ast)