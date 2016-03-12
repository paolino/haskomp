
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Score where

import Control.Lens
import Control.Lens.TH
import Sound.OSC

data E a = E {
  _event :: a,
  _timet :: Time
  } deriving (Show,Read)

makeLenses ''E

dFloor x s = (s *) . fromIntegral . floor . (/s) $ x
dMod t s = t - (t `dFloor` s) 
dNorm = flip dMod 1
  
data Linear = Linear Double Double

project :: Linear -> Time -> Time
project (Linear a b ) t = dNorm (t * a + b)

projectE :: Linear -> E a -> E a
projectE l e = over timet (project l) e

data Span = Span Time Time

x `inSpan` Span ((<= view timet x) -> True) ((> view timet x) -> True) =  True
_ `inSpan` _ = False

type Board a = [E a]

pickBoard s = map (view event) . filter (flip inSpan s . over timet dNorm)

data N = N Int Int Int Time deriving (Show,Read)

data NE = Off Int Int |  On Int Int Int | Control Int Int Int deriving (Eq,Ord, Show,Read)

offset t (Off c x) = Off c $ t + x
offset t (On c x v) = On c (t + x) v
modifyVelOn k (On c p v) = On c p $ k v
modifyVelOn k x = x

convert :: E N  ->  [E NE]
convert (E (N c p v dt) t) = over (traverse . timet) dNorm [E (On c p  v) t, E (Off c p) $ t + dt]

type Events = [E NE]
