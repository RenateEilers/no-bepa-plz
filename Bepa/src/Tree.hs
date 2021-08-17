{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Tree
  ( Cost (..)
  , NodeName (..)
  , NodeInfo (..)
  , TypeB (..)
  , Tree (..)
  , NodeNames()
  , BepaFree()
  , bepaFree
  , getNodeNames  
  , getCommonNodeNamesExceptBepa
  , treeCostTraversal
  , hasBepa
  ) where

-- imports

import Data.List (intersect, nub, isInfixOf)
import Control.Lens (makeLenses, view, (^.), (%~), (&), folded, foldMapOf, Traversal')
import Subset
import GDP (type (?),type (~~),Defn,defn,the,name,name2,(...),unname)

-- types
newtype Cost = Cost 
  {_amount :: Float} deriving (Show,Eq,Ord)
makeLenses ''Cost

newtype NodeName = NodeName 
  {_nodeName :: String}  deriving (Show,Eq,Ord)
makeLenses ''NodeName

data NodeInfo = NodeInfo
  { _cost :: Cost
  , _nodeInfoName :: NodeName
  } deriving (Show,Eq)
makeLenses ''NodeInfo

data TypeB = TypeB 
  {_bCost :: Cost
  , _bName :: NodeName 
  , _bChildren :: [TypeB]
  } deriving (Show,Eq)
makeLenses ''TypeB

data Tree = Tree_TypeA 
  {_info :: NodeInfo
  , _misc :: String 
  , _aChildren :: [Tree]
  }
  | Tree_TypeB 
  { _typeB :: TypeB }
  deriving (Show,Eq)
makeLenses ''Tree


newtype NodeNames a = NodeNames Defn

newtype BepaFree a = BepaFree Defn

bepaFree :: (NodeName -> Bool) ~~ BepaFree a 
bepaFree = defn (not . hasBepa)

-- functions
getCommonNodeNamesExceptBepa :: 
  Tree ~~ a 
  -> Tree ~~ b 
  -> [NodeName ? BepaFree] ? (Superset (Intersection (NodeNames a) (NodeNames b)))  
getCommonNodeNamesExceptBepa t1 t2 =
    unname $ filtered ... (supersetIsSubsetFlipped filterIsSubset)
    where 
      namesT1 = getNodeNames t1
      namesT2 = getNodeNames t2
      commonNodeNames = namesT1 `intersectGDP` namesT2
      filtered = filterGDP bepaFree commonNodeNames       

getNodeNames :: Tree ~~ a ->[NodeName] ~~ NodeNames a
getNodeNames = defn . nub . getNodeNames' . the
  where 
    getNodeNames' (Tree_TypeA i _ ts) = 
      i ^. nodeInfoName : foldMapOf folded getNodeNames' ts
    getNodeNames' (Tree_TypeB t) =  getNodeNamesTypeB t
      where 
        getNodeNamesTypeB t = 
          t ^. bName : foldMap getNodeNamesTypeB (t ^. bChildren)

-- -- wrapper function
treeCostTraversal :: Tree -> (Cost -> Cost) -> Tree 
treeCostTraversal t f = t & treeCostTraversal' %~ f

treeCostTraversal' :: Traversal' Tree Cost
treeCostTraversal' f t = case t of
  (Tree_TypeA i m ts) -> Tree_TypeA 
    <$> nodeInfoCostTraversal f i
    <*> pure m 
    <*> traverse (treeCostTraversal' f) ts
  (Tree_TypeB t) -> Tree_TypeB <$> typeBCostTraversal f t

nodeInfoCostTraversal :: Traversal' NodeInfo Cost 
nodeInfoCostTraversal f (NodeInfo c n) = NodeInfo <$> f c <*> pure n

typeBCostTraversal :: Traversal' TypeB Cost 
typeBCostTraversal f (TypeB c n bs) = TypeB 
  <$> f c 
  <*> pure n 
  <*> traverse (typeBCostTraversal f) bs

hasBepa :: NodeName -> Bool
hasBepa = isInfixOf "Bepa" . view nodeName
