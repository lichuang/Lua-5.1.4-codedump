-- print
print("a")  -- string
print(1)    -- number

-- global
b = true
print(b)

-- local
local a = true
print(a)

-- table
local t = {}
t["1"] = "foo"
t[1] = "a"
print(t["1"])
print(t[1])

function fun1()
    print("1")
end
fun1()
