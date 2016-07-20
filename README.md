# parse.lua
A Lua parser, in Lua.

`parse.lua` is a manual, line-by-line port of [`luaparse`](https://github.com/oxyc/luaparse) from JavaScript to Lua. It parses Lua code into an AST tree akin to [ESTree](https://github.com/estree/estree) (formerly the [Mozilla Parser API](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API)).


## Example usage
```lua
local parser = require('parse')
local ast = parser.parse('print("hello world")')
```
See `/demo.lua` for more.


## Status
This project is still fairly new and relatively untested. It is a manual translation and therefore will likely contain some issues still. I've set up a [Gitter channel](https://gitter.im/paulcuth/parse.lua) for the project and will happily respond to issues in a timely fashion (especially if you provide concise non-working examples).


## Contact
[![Join the chat at https://gitter.im/paulcuth/parse.lua](https://img.shields.io/badge/gitter-join%20chat-green.svg)](https://gitter.im/paulcuth/parse.lua)

Please feel free to ask anything about the project on the [Gitter channel](https://gitter.im/paulcuth/parse.lua). Any pull requests for bugfixes are gratefully appreciated.


##  Ackowledgements
Many thanks to [Oskar Schöldström](https://github.com/oxyc) and the [`luaparse`](https://github.com/oxyc/luaparse) project, on which solid foundation this project is entirely based.

