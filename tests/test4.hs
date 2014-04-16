z = 5

main
 | z == 1    = print $ addsome 1
 | otherwise = print $ addsome 2

addsome :: Integer -> Integer
addsome n
 | z == 1    = n+2
 | otherwise = n+z
