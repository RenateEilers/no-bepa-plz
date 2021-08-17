{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Subset 
    ( Subset()
    , Superset()
    , Intersection()
    , filterGDP
    , supersetIsSubsetFlipped
    , filterIsSubset
    , intersectGDP
    ) where

import GDP
import Data.List

newtype Subset xs ys = Subset Defn

type xs ⊆ ys = Subset xs ys

newtype Superset xs ys = Superset Defn
type xs ⊇ ys = Superset xs ys

-- definitions for filtering
newtype Filtered p xs = Filtered Defn

filterGDP :: Defining (p ()) => (a -> Bool) ~~ (p n) -> [a] ~~ xs -> [a ? p] ~~ Filtered (p n) xs
filterGDP p xs = defn $ map assert $ filter (the p) (the xs)

supersetIsSubsetFlipped _ = axiom
supersetIsSubsetFlipped :: Proof (Subset xs ys) -> Proof (Superset ys xs)

filterIsSubset :: Proof (Filtered p xs ⊆ xs)
filterIsSubset = axiom

-- definitions for intersection
newtype Intersection xs ys = Intersection Defn

intersectGDP :: Eq a => [a] ~~ xs -> [a] ~~ ys -> [a] ~~ Intersection xs ys
intersectGDP xs ys = defn $ (the xs) `intersect` (the ys)
