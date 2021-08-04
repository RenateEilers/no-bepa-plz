{-# LANGUAGE TemplateHaskell #-}

module Nodes where

-- imports

import Data.List
import Control.Lens

-- types
newtype Cost = Cost 
  {_amount :: Float} deriving (Show,Eq)
makeLenses ''Cost

newtype NodeName = NodeName 
  {_name :: String}  deriving (Show,Eq)
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

-- functions
getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 = 
  getNodeNames t1 `intersect` getNodeNames t2
     & filter (not . hasBepa)

getNodeNames ::  Tree ->[NodeName]
getNodeNames t@(Tree_TypeA _ _ ts) =
  t ^. nodeNameLens : ts ^.. folded . nodeNameLens  
getNodeNames (Tree_TypeB t) = 
  t ^. bName : t ^.. bChildren . folded . bName

-- wrapper function
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
       
nodeNameLens :: Lens' Tree NodeName
nodeNameLens = lens getter setter
  where 
    getter (Tree_TypeA i _ ts) = i ^. nodeInfoName
    getter (Tree_TypeB t) = t ^. bName
    setter t@(Tree_TypeA _ _ _) n = t & info . nodeInfoName .~ n
    setter t@(Tree_TypeB _) n = t & typeB . bName .~ n
       
hasBepa :: NodeName -> Bool
hasBepa = isInfixOf "Bepa" . view name
