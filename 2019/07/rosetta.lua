-- https://rosettacode.org/wiki/Permutations#Lua

local M = {}
function M.permutation(a, n, cb)
	if n == 0 then
		cb(a)
	else
		for i = 1, n do
			a[i], a[n] = a[n], a[i]
			M.permutation(a, n - 1, cb)
			a[i], a[n] = a[n], a[i]
		end
	end
end
return M
 
--Usage
--local function callback(a)
--	print('{'..table.concat(a, ', ')..'}')
-- end
-- permutation({1,2,3}, 3, callback)
