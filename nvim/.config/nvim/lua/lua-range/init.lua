local NUMBER_EPSILON = 0.00000000000001

-- create a table with values :from until :to
local function number_range (from, to, step)
	local t = {}

	for value = from, to, step do
		t[#t + 1] = value
	end

	return t
end

-- create a table with values char of :from to char of :to
local function string_range (from, to, step)
	local t = {}

	for value = from:byte (), to:byte (), step do
		t[#t + 1] = string.char (value)
	end

	return t
end

-- table holding the mapping from data type to creator function
local creators = {
	number = number_range,
	string = string_range
}

-- range function export
return function (from, to, step)
	if not from then
		error ("lua-range needs at least a start parameter (from)")
	end

	local type_name = type (from)

	local range_creator = creators[type_name]
	if not range_creator then
		error ("Unknown type " .. type_name)
	end

	-- if no step width is specified default to 1
	if not step then
		step = 1
	end
	-- if no upper bound is set, end where it started
	if not to then
		to = from
	end
	-- if upper bound is bigger than lower bound swap both
	local is_reversed = false
	if to < from then
		from, to = to, from
		is_reversed = true
	end

	-- create the closure/iterator
	local range = range_creator (from, to, step)
	local pos = 0
	local size = #range
	if is_reversed then
		local buffer = range
		range = {}

		while #buffer > 0 do
			table.insert (range, table.remove (buffer))
		end
	end

	return function ()
		local r
		pos = pos + 1

		if pos <= size then
			r = range[pos]
		else
			r = nil
		end

		return r
	end
end
