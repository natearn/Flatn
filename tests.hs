import Flatn

-- Manual tests for the flatn module

-- sanity check
test1 =
	let
		nests =
			[
				[],                                                  -- []
				[Value 1],                                           -- [1]
				[Nest [Value 2]],                                    -- [[2]]
				[Value 3,Value 4],                                   -- [3,4]
				[Value 5,Nest [Value 6, Value 7]],                   -- [5,[6,7]]
				[Nest [Value 8, Value 9],Nest [Value 10, Value 11]], -- [[8,9],[10,11]]
				[Nest [Value 12, Value 13],Value 14],                -- [[12,13],14]
				[Value 15,Nest [], Value 16]                         -- [15,[],16]
			]
		flats =
			[
				[],
				[1],
				[2],
				[3,4],
				[5,6,7],
				[8,9,10,11],
				[12,13,14],
				[15,16]
			]
	in zipWith (\a b -> if a == b then "PASS" else "FAIL") (map flatn nests) flats

-- Infinite structures should not explode the call stack, which implies fininte structures wont either.
-- This is supposed to be a non-terminating expression.
test2 =
	let infinest = [Nest infinest]
	in flatn infinest
