# lua-range

Provides a function to create a range iterator. After creation of said iterator it generates values from a lower bound up to an upper bound with an optional step width. If the iterator position is greater then the upper bound the iterator will return ```nil```.

## Usage

With numbers:

```lua
local range = require ("lua-range.init")

local sum = 0
for value in range (42, 1337) do
	sum = sum + value
end

print (sum)
```

It is also possible to create ranges with strings:

```lua
local range = require ("lua-range.init")

local str = ""
for value in range ("A", "C") do
	str = str .. value
end

print (str)
```


