safeSecond [] = Nothing

safeSecond (_:xs) = if null xs then
						Nothing
					else
						Just (head xs)

betterSecond (_:x:_) = Just x

betterSecond _ = Nothing
