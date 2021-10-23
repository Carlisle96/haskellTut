toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

isEvenLength :: [Integer] -> Bool
isEvenLength [] = True
isEvenLength (_:xs) = not $ isEvenLength xs

sumDigits ::  [Integer] -> Integer
sumDigits list  = foldl (+) 0 list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list
    | isEvenLength list = zipWith (*) list $ cycle[2,1]
    | otherwise         = zipWith (*) list $ cycle[1,2]

validate :: Integer -> Bool
validate n = mod ( sumDigits
    $ map sumDigits
    $ map toDigits
    $ doubleEveryOther
    $ toDigits n ) 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0 = []
    | n > 0 = hanoi (n-1) a c b ++ ( a, b ) : hanoi (n-1) c b a