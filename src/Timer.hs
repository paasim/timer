module Timer ( Timer
             , mkDecrTimer
             ) where

import Control.Arrow ( (&&&) )
import Control.Monad
import Data.List ( unfoldr )
import Text.Read ( readMaybe )

data Timer = T { h :: Int, m :: Int, s :: Int }

instance Show Timer where
  show (T 0 0 s) = show s
  show (T 0 m s) = show m ++ ":" ++ paddedShow s
  show (T h m s) = show h ++ ":" ++ paddedShow m ++ ":" ++ paddedShow  s

paddedShow i = if i < 10 then '0' : show i else show i

decr :: Timer -> Maybe Timer
decr (T 0 0 0) = Nothing
decr (T h 0 0) = Just $ T (h-1) 59 59
decr (T h m 0) = Just $ T h (m-1) 59
decr (T h m s) = Just $ T h m (s-1)

mkDecrTimer :: [String] -> Maybe [Timer]
mkDecrTimer = liftM decrTimer . readTimer

decrTimer :: Timer -> [Timer]
decrTimer = unfoldr (sequence . (&&&) id decr)

readTimer :: [String] -> Maybe Timer
readTimer = traverse readMaybe >=> listToTimer

listToTimer :: [Int] -> Maybe Timer
listToTimer l
  | length l == 1 = mkTimer 0 0 (l !! 0)
  | length l == 2 = mkTimer 0 (l !! 0) (l !! 1)
  | length l == 3 = mkTimer (l !! 0) (l !! 1) (l !! 2)
  | otherwise = Nothing

mkTimer :: Int -> Int -> Int -> Maybe Timer
mkTimer h m s = T <$> hOk h <*> msOk m <*> msOk s where
  msOk x = if x >= 0 && x < 60 then Just x else Nothing
  hOk x = if x >= 0 then Just x else Nothing

