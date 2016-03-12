{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Subd where

import System.Random

import Data.Array
import Data.List
import Control.Monad
import Control.Lens

import Score 

-- events, see Score
type Events = [E NE]

-- a stream of random pickings from a list
stream :: [a] -> IO [a]
stream xs = map (a !) . randomRs (bounds a) <$> newStdGen where
    a = listArray (1,length xs) xs 

-- a subdivision structure
data Subd = Subd [Subd] | Tab deriving Show

-- flatten an Subd to a subdivision of a unity
-- flattenSubd $ Subd [Subd [Tab,Tab],Subd [Tab]] -> [0.25,0.25,0.5]
flattenSubd = flattenSubd' 1 where
  flattenSubd' :: Double -> Subd -> [Double]
  flattenSubd' k Tab = [k]
  flattenSubd' k (Subd xs) = xs >>= flattenSubd' (k/fromIntegral (length xs)) 

-- random Subd from the number of subdivisions the number of levels, a pool of splitting numbers
randomSubd :: Int -> [Int] -> IO Subd
randomSubd 0 _ = return Tab
randomSubd n ps = do 
  c <- randomRIO (0,length ps - 1)
  let (as,l:bs) = splitAt c ps
  rs <- replicateM l (randomSubd (n - 1) $ as ++ bs) 
  return $ Subd rs



