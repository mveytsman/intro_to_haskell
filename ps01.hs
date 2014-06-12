import Test.HUnit

-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
-- Exercise 1

ex1Tests = test ["helpertest" ~: toStr '1' ~?= "1",
                 "test1" ~: toDigits 1234 ~?= [1,2,3,4],
                 "test2" ~: toDigitsRev 1234 ~?= [4,3,2,1],
                 "test3" ~: toDigits 0 ~?= [],
                 "test4" ~: toDigits (-17) ~?= []]
toStr :: Char -> [Char]
toStr c = [c]

toDigits :: Integer -> [Integer]
toDigits i
  | i > 0 = map (read . toStr) $ show i
  | otherwise = []

toDigitsRev :: Integer -> [Integer]     
toDigitsRev = reverse . toDigits


-- Exercise 2

ex2Tests = test ["test1" ~: doubleEveryOther [8,7,6,5] ~?= [16,7,12,5],
                 "test2" ~: doubleEveryOther [1,2,3] ~?= [1,4,3]]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [a] = [a]
doubleEveryOther [a,b] = [2*a,b]
-- hacky!!
doubleEveryOther lst@(a:b:rest) 
  | even $ length lst = [2*a,b] ++ (doubleEveryOther rest)
  | otherwise =  [a,2*b] ++ doubleEveryOther rest


-- Exercise 3

ex3Tests = test ["test1" ~: sumDigits [16,7,12,5] ~?= 22]

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits) 


-- Exercise 4

ex4Tests = test ["test1" ~: validate 4012888888881881 ~?= True,
                 "test2" ~: validate 4012888888881882 ~?= False]

isZero :: Integer -> Bool
isZero x = x == 0

validate :: Integer -> Bool
validate = isZero . (flip rem 10) . sumDigits . doubleEveryOther . toDigits


-- Exercise 5

ex5Tests = test ["test1" ~: hanoi 2 "a" "b" "c" ~?= [("a","c"), ("a","b"), ("c","b")]]

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDisks src dst tmp 
  | numDisks == 0 = []
  | numDisks == 1 = [  (src, dst)]
  | otherwise = (hanoi (numDisks - 1) src tmp dst)
                ++ (hanoi 1 src dst tmp)
                ++ (hanoi (numDisks -1) tmp dst src)
                    
                    

-- Exercise 6 is optimal 4 peg Tower of Hanoi. Possible TODO
