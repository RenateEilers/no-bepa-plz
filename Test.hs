{-# LANGUAGE TemplateHaskell #-}

module Test where

-- imports
import Nodes
import Test.QuickCheck
-- import Test.QuickCheck.Function
import Data.List
import qualified Data.Text as T
import Control.Applicative

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
    ,sized arbitraryTypeATree
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

-- auxiliary properties
noBepa :: NodeName -> Bool
noBepa = not . hasBepa 

-- case insensitive version
noBepaCI :: NodeName -> Bool
noBepaCI = 
  not . isInfixOf (toCaseless "Bepa") . toCaseless . _name
    where toCaseless = T.unpack . T.toCaseFold . T.pack

-- properties

-- 1. "Bepa" does not occur in the results
--  of getCommonNodeNamesExceptBepa
prop_bepaNotInGetCommonNamesExceptBepa :: Tree -> Tree -> Bool
prop_bepaNotInGetCommonNamesExceptBepa t1 t2 =
  all noBepa $ getCommonNodeNamesExceptBepa t1 t2

-- 2. If a name occurs in both trees and doesn't 
-- contian "Bepa", it will be in the results
prop_noMissingNames :: Tree -> Tree -> Bool
prop_noMissingNames t1 t2 = all there candidateNames
  where 
    candidateNames = [name | name <- getNodeNames t1 
      , name `elem` getNodeNames t2
      , noBepa name]
    there = flip elem $ getCommonNodeNamesExceptBepa t1 t2

-- 3. All names in the result should be in 
--  the original trees
prop_noExtraNames :: Tree -> Tree -> Bool
prop_noExtraNames t1 t2 = all inInput results 
  where results = getCommonNodeNamesExceptBepa t1 t2
        inInput n = n `elem` getNodeNames t1 
          || n `elem` getNodeNames t2

-- 4. Calling the function on two identical trees
--  results in the nodes of that do not contain "Bepa"
prop_getCommonNodeNamesExceptBepaOnSameTree :: Tree -> Bool
prop_getCommonNodeNamesExceptBepaOnSameTree t1 = 
  getCommonNodeNamesExceptBepa t1 t1 
   == filter noBepa (getNodeNames t1)

-- 5. Applying a function over the costs of nodes
--  does not affect the result of the function
prop_costIrrelevant :: (Fun Cost  Cost) -> Tree -> Tree -> Bool
prop_costIrrelevant (Fun _ f) t1 t2 = 
  getCommonNodeNamesExceptBepa t1 t2
   == getCommonNodeNamesExceptBepa (f `over` t1) (f `over` t2)
  where 
    over = flip treeCostTraversal

-- 6. Node with "bepa" (case insensitive)
-- should never be present in the results
-- (this property should fail)
prop_bepaNotInGetCommonNamesExceptBepaCI :: Tree -> Tree -> Bool
prop_bepaNotInGetCommonNamesExceptBepaCI t1 t2 =
  all noBepaCI $ getCommonNodeNamesExceptBepa t1 t2
    
return [] -- required for 'quickCheckAll'

-- run this function to test all given properties
testAllProperties :: IO Bool
testAllProperties = $quickCheckAll 

-- test All properties with given maxSize
testAllPropertiesWithSize :: Int -> IO Bool
testAllPropertiesWithSize n = 
  $forAllProperties $ quickCheckWithResult stdArgs {maxSize = n}