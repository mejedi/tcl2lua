# tcl2lua

An attempt at automatic conversion from TCL to Lua.

The project leverages a parser from JIM TLC interpretor.

The conversion tool `tcl2.lua` recognizes patterns specific for sqlite TCL test suit.
It is capable of converting ~99% of the code automatically.
The remaining 1% are too complex and require manual work.

When automatic conversion fails, the tool emits a call to `X` function. AST is passed as a JSON string.
`X`-s must be removed manually.

```
X(26, "X!cmd", [=[["sqlite3_shutdown"]]=])
```
