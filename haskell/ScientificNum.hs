module ScientificNum where

import Data.List.Split

type Base = Float
type Exponent = Int

data SciNum = Sci (Base, Exponent) deriving Show

instance Num SciNum where
  abs = absSci
  negate = negateSci
  signum = signSci
  (+) = addSci
  (-) = subSci
  (*) = multSci
  fromInteger = sciFromInteger

absSci :: SciNum -> SciNum
absSci n@(Sci (b, e)) | b > 0 = n
                      | otherwise = Sci (negate b, e)

negateSci :: SciNum -> SciNum
negateSci (Sci (b, e)) = Sci (negate b, e)

signSci :: SciNum -> SciNum
signSci (Sci (b, e))  | b >= 0 = Sci (1, 0)
                      | otherwise = Sci (negate 1, 0)

addSci :: SciNum -> SciNum -> SciNum
addSci n@(Sci (b1, e1)) m@(Sci (b2, e2)) 
  | e1 > e2 = addSci n $ Sci (b2 / 10, e2 + 1)
  | e1 < e2 = addSci m n
  | otherwise = Sci (b1 + b2, e1)

subSci :: SciNum -> SciNum -> SciNum
subSci n m = addSci n (negate m)

multSci :: SciNum -> SciNum -> SciNum
multSci (Sci (b1, e1)) (Sci (b2, e2)) = Sci (b1 * b2, e1 + e2)

sciFromInteger :: Integer -> SciNum
sciFromInteger n = reduce $ Sci (fromInteger n, 0)

reduce :: SciNum -> SciNum
reduce s@(Sci (b, e)) | b < 10 = s
                      | otherwise = reduce (Sci (b / 10, e + 1))

instance Eq SciNum where
  n@(Sci (b1, e1)) == m@(Sci (b2, e2)) 
    | e1 > e2 = n == Sci (b2 / 10, e2 + 1)
    | e1 < e2 = m == n
    | otherwise = b1 == b2

  (/=) m n = not (m == n)

instance Ord SciNum where
  (<) n@(Sci (b1, e1)) m@(Sci (b2, e2))
    | e1 > e2 = n < (Sci (b2 / 10, e2 + 1))
    | e1 < e2 = (Sci (b1 / 10, e1 + 1)) < m
    | otherwise = b1 < b2

  (>=) n m = not (n < m)
  (>) n m  = (n >= m) && (not (n == m))
  (<=) n m = not (n > m)

fromString :: String -> SciNum
fromString s = let [b, e] = splitOn "e" s
               in reduce $ Sci (read b, read (filter (/= '+') e))

sciToFloat :: SciNum -> Float
sciToFloat (Sci (b, e)) = b * (10 ^^ e)