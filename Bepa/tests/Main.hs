{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- imports
import Tree
import Test.QuickCheck 
import Data.List (sort, isInfixOf)
import qualified Data.Text as T
import Test.Tasty (defaultMain,localOption)
import Test.Tasty.QuickCheck 
import GDP (the, name, name2)
import Subset

  -- generators
instance Arbitrary Cost where
  arbitrary = Cost <$> arbitrary `suchThat` (>=0)

instance CoArbitrary Cost where
  coarbitrary (Cost c)  = coarbitraryReal c

instance Function Cost where  
  function = functionMap _amount Cost

instance Arbitrary NodeName where
  -- We strongly limit the set of generated names
  --  to increase the chance of finding counterexamples 
  arbitrary = NodeName <$> elements names
    where 
      names = "Bepa" : "bepa" : ["Node" ++ show n | n <- [1..10]]
    
instance Arbitrary NodeInfo where
  arbitrary = NodeInfo <$> arbitrary <*> arbitrary

instance Arbitrary TypeB where
  arbitrary = sized arbitraryTypeB
    where 
      arbitraryTypeB n = TypeB 
        <$> arbitrary
        <*> arbitrary 
        <*> halfingList n arbitrary

instance Arbitrary Tree where
  arbitrary = oneof [arbitraryTypeBTree
    , sized arbitraryTypeATree
    ]
      where                 
        arbitraryTypeBTree = Tree_TypeB <$> arbitrary
        arbitraryTypeATree n = Tree_TypeA
          <$> arbitrary 
          <*> (getPrintableString <$> arbitrary )
          <*> halfingList n arbitrary

-- to guarantee termination:
-- create list of at most n elements
-- which in turn are generated with halfed size
halfingList :: Int -> Gen a -> Gen [a]
halfingList n gen = do
    m <- choose (0,n)
    vectorOf m $ scale (`div` 2) gen

-- auxiliary function to remove embellishments from the 
-- result of getCommonNodeNamesExceptBepa for testing
stripGhostsCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
stripGhostsCommonNodeNamesExceptBepa t1 t2 = 
  name2 t1 t2 $  \t1 t2 -> 
    map the $ the $ getCommonNodeNamesExceptBepa t1 t2

-- same as above, but for getNodeNames
stripGhostsNodeNames :: Tree -> [NodeName]
stripGhostsNodeNames t1 = name t1 $ \t1 -> 
  the $ getNodeNames t1

-- auxiliary properties
noBepa :: NodeName -> Bool
noBepa = not . hasBepa 

-- case insensitive version
noBepaCI :: NodeName -> Bool
noBepaCI = 
  not . isInfixOf (toCaseless "Bepa") . toCaseless . _nodeName
    where toCaseless = T.unpack . T.toCaseFold . T.pack

-- properties

-- 1. "Bepa" does not occur in the results
--  of getCommonNodeNamesExceptBepa
prop_bepaNotInGetCommonNamesExceptBepa :: Tree -> Tree -> Bool
prop_bepaNotInGetCommonNamesExceptBepa t1 t2 =
  all noBepa $ stripGhostsCommonNodeNamesExceptBepa t1 t2
  

-- 2. If a name occurs in both trees and doesn't 
-- contian "Bepa", it will be in the results
prop_noMissingNames :: Tree -> Tree -> Bool
prop_noMissingNames t1 t2 = all there candidateNames
  where 
    candidateNames = [name | name <- stripGhostsNodeNames t1
      , name `elem` stripGhostsNodeNames t2
      , noBepa name]
    there = flip elem $ stripGhostsCommonNodeNamesExceptBepa t1 t2

-- 3. All names in the result should be in 
--  the original trees
prop_noExtraNames :: Tree -> Tree -> Bool
prop_noExtraNames t1 t2 = all inInput results 
  where results = stripGhostsCommonNodeNamesExceptBepa t1 t2
        inInput n = n `elem` stripGhostsNodeNames t1 
          || n `elem` stripGhostsNodeNames t2

-- 4. Calling the function on two identical trees
--  results in the nodes of that do not contain "Bepa"
prop_getCommonNodeNamesExceptBepaOnSameTree :: Tree -> Bool
prop_getCommonNodeNamesExceptBepaOnSameTree t1 = 
  stripGhostsCommonNodeNamesExceptBepa t1 t1 
   == filter noBepa (stripGhostsNodeNames t1)

-- 5.  The order in which the arguments are 
--  passed to the function is irrelevant (modulo sorting)
prop_orderIrrelevant :: Tree -> Tree -> Bool
prop_orderIrrelevant t1 t2 =
  sort (stripGhostsCommonNodeNamesExceptBepa t1 t2)
   == sort (stripGhostsCommonNodeNamesExceptBepa t2 t1)

-- 6. Applying a function over the costs of nodes
--  does not affect the result of the function
-- prop_costIrrelevant :: (Fun Cost  Cost) -> Tree -> Tree -> Bool
prop_costIrrelevant (Fun _ f) t1 t2 = 
  stripGhostsCommonNodeNamesExceptBepa t1 t2
   == stripGhostsCommonNodeNamesExceptBepa (f `over` t1) (f `over` t2)
  where 
    over = flip treeCostTraversal

-- 7. Node with "bepa" (case insensitive)
-- should never be present in the results
-- (this property should fail)
prop_bepaNotInGetCommonNamesExceptBepaCI :: Tree -> Tree -> Bool
prop_bepaNotInGetCommonNamesExceptBepaCI t1 t2 =
  all noBepaCI $ stripGhostsCommonNodeNamesExceptBepa t1 t2
    
return [] -- required for 'quickCheckAll'

-- run this function to test all given properties
testAllProperties :: IO Bool
testAllProperties = $quickCheckAll 

-- test All properties with given maxSize
testAllPropertiesWithSize :: Int -> IO Bool
testAllPropertiesWithSize n = 
  $forAllProperties $ quickCheckWithResult stdArgs {maxSize = n}

main = defaultMain $ localOption (QuickCheckMaxSize 10) $ testProperties "All" $allProperties