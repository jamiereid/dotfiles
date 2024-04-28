range = require ("init")

function testIntegerRangeWithoutEnd ()
	local sum = 0
	for i in range (2) do
		sum = sum + i
	end

	local expected = 2
	assert (
		expected == sum,
		"sum should be " .. expected .. " but is " .. sum
	)
end

function testIntegerRange ()
	local sum = 0
	for i in range (1, 10) do
		sum = sum + i
	end

	local expected = 55
	assert (
		expected == sum,
		"sum should be " .. expected .. " but is " .. sum
	)
end

function testIntegerRangeReversed ()
	local str = ""
	local sum = 0
	for i in range (4, 1) do
		sum = sum + i
		str = str .. i
	end

	local expected_sum = 10
	assert (
		expected_sum == sum,
		"sum should be " .. expected_sum .. " but is " .. sum
	)

	local expected_str = "4321"
	assert (
		expected_str == str,
		"str should be " .. expected_str .. " but is " .. str
	)
end

function testIntegerRangeWithStep ()
	local sum = 0
	for i in range (42, 1337, 2) do
		sum = sum + i
	end

	local expected = 446472
	assert (
		expected == sum,
		"sum should be " .. expected .. " but is " .. sum
	)
end

function testFloatRange ()
	local sum = 0
	for i in range (0, 0.1, 0.01) do
		sum = sum + i
	end

	local expected = 0.55
	local epsilon = 0.00000000000001
	assert (
		math.abs (expected - sum) < epsilon,
		"sum should be " .. expected .. " but is " .. sum
	)
end

function testStringRangeWithoutEnd ()
	local alpha = ""
	for i in range ("W") do
		alpha = alpha .. i
	end

	local expected = "W"
	assert (
		expected == alpha,
		"alpha should be " .. expected .. " but is " .. alpha
	)
end

function testStringRange ()
	local alpha = ""
	for i in range ("W", "Z") do
		alpha = alpha .. i
	end

	local expected = "WXYZ"
	assert (
		expected == alpha,
		"alpha should be " .. expected .. " but is " .. alpha
	)
end

function testStringRangeReversed ()
	local alpha = ""
	for i in range ("Z", "W") do
		alpha = alpha .. i
	end

	local expected = "ZYXW"
	assert (
		expected == alpha,
		"alpha should be " .. expected .. " but is " .. alpha
	)
end

function main ()
	testIntegerRangeWithoutEnd ()
	print ("succeeded testIntegerRangeWithoutEnd")
	testIntegerRange ()
	print ("succeeded testIntegerRange")
	testIntegerRangeReversed ()
	print ("succeeded testIntegerRangeReversed")
	testIntegerRangeWithStep ()
	print ("succeeded testIntegerRangeWithStep")
	testFloatRange ()
	print ("succeeded testFloatRange")
	testStringRange ()
	print ("succeeded testStringRange")
	testStringRangeWithoutEnd ()
	print ("succeeded testStringRangeWithoutEnd")
	testStringRangeReversed ()
	print ("succeeded testStringRangeReversed")
end

main ()
