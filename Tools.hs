module Tools where
import Data.Time
import Data.Time.Clock.POSIX
import System.Random
import Control.Parallel.Strategies


-- Time an IO action
time :: IO a -> IO (Double, a)
time action = do
    d1 <- getCurrentTime
    o  <- action
    d2 <- getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), o)


-- Time a pure function. A should reduce to a single value, instead seq
-- won't work correctly.
pureTime :: a -> IO (Double, a)
pureTime action = do
    d1 <- getCurrentTime
    let a = action
    d2 <- a `seq` getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), a)


-- Round to n digits.
roundN :: Int -> Double -> Double
roundN digits x =
    let (_, k) = properFraction x
    in (x-) $ (/(10.0^digits)) $ snd $ properFraction (k * (10.0^digits))


-- Shuffles a list
shuffle seed list num =
    let gen   = mkStdGen seed
        rnds  = randomRs (0, length list - 1) gen
        poss  = take num $ mkPairs rnds
    in foldl switch list poss
  where mkPairs (x:y:xs) = (x,y) : mkPairs xs
        set list i v = 
            let (r,l) = splitAt i list
            in r ++ [v] ++ tail l

        switch list (i,j) =
            let v1 = list !! i
                v2 = list !! j
                l1 = set list j v1
                l2 = set l1 i v2
            in l2


split' a [] = []
split' a xs =
    let (b,c) = splitAt a xs
    in b : split' a c


-- Split a list at the provided position, e.g. 
--
--   divide [1..10] 4 = ([1..3], 4, [5..10])
divide :: [a] -> Int -> ([a], a, [a])
divide list pos =
    let (pre, post') = splitAt pos list
    in (pre, head post', tail post')
