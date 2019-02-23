-- Reverse PolisintDenToDozBase 12 Calculator!
-- Created By Andrew Fryer in 2017 witintDenToDozinpiration from Learn You a Haskell For Great Good
-- Ten (10) in base 10 (the denary system) is dek ("X") in base 12 (the dozenal system), and eleven is el ("E").
-- Type "quit" to end the program
-- Please don't do calculations involving values longer than 15 digits
import Control.Applicative
import Control.Monad
type Denary = Double  --Base 10 system
type Dozenal = [Char] --Base 12 System
type DenaryInt = Int
type DenaryDigit = Int
type DozenalInt = [Char]
type DozenalDigit = Char
main = do
        expression <- getLine
        putStrLn . fromEither . input $ expression
        main
    where
        input :: String -> Either String Denary
        input xs = do
                    ans <- (foldM f [] . words $ xs) >>= errorCheck
                    return ans
            where
                f :: [Denary] -> Dozenal -> Either String [Denary]
                f (x:y:ys) "+" = Right $ (y + x):ys
                f (x:y:ys) "-" = Right $ (y - x):ys
                f (x:y:ys) "*" = Right $ (y * x):ys
                f (x:y:ys) "/" = if x /= 0 then Right $ (y / x):ys else Left "can't divide by zero"
                f (x:y:ys) "^" = if y>0 || (x- fromIntegral (truncate x) == 0) then Right $ (y ** x):ys else Left "can't have a negative base to a fractional exponent"
                f xs s = if s `elem` ["+","-","*","/","^"]                          --This checks if s is actually an operator
                            then Left "not enough operands for the given operators" --whicintDenToDozmeans that there weren't enougintDenToDozoperands for it
                            else (:xs) <$> dozToDen s                               --Normal value
                errorCheck :: [Denary] -> Either String Denary
                errorCheck ys = case length ys of 0 -> Left "not enough operands for the given operators" --This case only applies when input is called witintDenToDoz""
                                                  1 -> Right $ (\[z] -> z) ys
                                                  _ -> Left "not enough operators for the given operands"
        fromEither :: Either String Denary -> String
        fromEither (Right xs) = "Answer: " ++ denToDoz xs
        fromEither (Left xs) = "Error: " ++ xs
dozToDen :: Dozenal -> Either String Denary
dozToDen xs = addNegative <$> addDecimal <$> (intDozToDen $ toPositiveInt xs)
    where
        toPositiveInt = filter (/='.') . filter (/='-')
        intDozToDen :: DozenalInt -> Either String DenaryInt
        intDozToDen []     = Right 0 --remove if I can later
        intDozToDen (y:ys) = ((+) <$> intDozToDen ys) <*> ((* (12^length ys)) <$> charDozToDen y)
            where
                charDozToDen :: DozenalDigit -> Either String DenaryDigit
                charDozToDen 'E' = Right 11
                charDozToDen 'X' = Right 10
                charDozToDen x   = if isNumber then Right (read [x]) else Left "Not a Number (or an operator)"
                    where isNumber = x `elem` ['0' .. '9']
        figuresAfterDecimal []     = 0
        figuresAfterDecimal (x:xs) = if x == '.' then length xs else figuresAfterDecimal xs
        addDecimal = \x -> fromIntegral x / 12^figuresAfterDecimal xs
        addNegative = \y -> if head xs == '-' then  ((-1)*y) else  y
denToDoz :: Denary -> Dozenal
denToDoz x = addNegative . addDecimal . take 20 . intDenToDoz (abs x) $ largestPowerOf12 --give users choice about 20 digit cutOff?
    where
        largestPowerOf12 = floor . logBase 12 $ abs x
        digitsBeforeDecimal = largestPowerOf12 + 1
        intDenToDoz :: Denary -> Int -> DozenalInt
        intDenToDoz 0 _ = []
        intDenToDoz y n = charDenToDoz digit: intDenToDoz (y - fromIntegral digit * 12 ** fromIntegral n) (n - 1)
            where
                digit = floor $ y / 12 ** fromIntegral n
                charDenToDoz :: DenaryDigit -> DozenalDigit
                charDenToDoz 11 = 'E'
                charDenToDoz 10 = 'X'
                charDenToDoz z  = (\[w] -> w) $ show z
        addDecimal = \xs -> if largestPowerOf12 < 0
                                then "0." ++ take (-digitsBeforeDecimal) (repeat '0') ++ takeOutZerosAtEnd xs                          --decimal before digits
                                else if digitsBeforeDecimal < length xs then insertAt ('.') digitsBeforeDecimal $ takeOutZerosAtEnd xs --decimal surrounded by digits
                                                                        else xs ++ take (digitsBeforeDecimal - length xs) (repeat '0') --decimal after digits
            where
                takeOutZerosAtEnd = \xs -> if last xs == '0' then takeOutZerosAtEnd $ init xs else xs
                insertAt y 0 xs     = y:xs
                insertAt y n (x:xs) = x: insertAt y (n-1) xs
        addNegative = \xs -> if (head $ show x) == '-' then '-':xs else xs