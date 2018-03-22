{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module SemanticAnalyzer.IsType
  ( (<==)
  , toType
  , IsType
  , lookupClass
  , (/>)
  , (\/)
  , (>==<)
  ) where

import qualified Data.Set as S

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as M
import Data.String (fromString)
import SemanticAnalyzer.Class
import SemanticAnalyzer.Maybe (runMaybe)
import SemanticAnalyzer.PrimitiveTypes (primitiveTypes)
import SemanticAnalyzer.SemanticAnalyzer
       (SemanticAnalyzer, SemanticAnalyzerM)
import SemanticAnalyzer.Type (Type(SELF_TYPE, TypeName))
import SemanticAnalyzer.TypedAST (ExpressionT, computeType)

class IsType a where
  toTypeView :: a -> TypeView

data TypeView
  = RegularType Type
  | TypeClass ClassRecord
  deriving (Eq)

getTypeName :: TypeView -> SemanticAnalyzer String
getTypeName (RegularType SELF_TYPE) = fmap fst ask
getTypeName (RegularType (TypeName typeName)) = return typeName
getTypeName (TypeClass ObjectClass) = return "Object"
getTypeName (TypeClass (ClassRecord name _ _ _)) = return name

instance IsType Type where
  toTypeView = RegularType

instance IsType String where
  toTypeView = RegularType . fromString

instance IsType ClassRecord where
  toTypeView = TypeClass

instance IsType ExpressionT where
  toTypeView = RegularType . computeType

toType :: IsType a => a -> Type
toType (toTypeView -> TypeClass (ClassRecord typeName _ _ _)) = TypeName typeName
toType (toTypeView -> TypeClass ObjectClass) = TypeName "Object"
toType (toTypeView -> RegularType typeVal) = typeVal

-- determines if the left argument is a subset of the right
(<==) ::
     IsType a
  => IsType b =>
       a -> b -> SemanticAnalyzerM Bool
(toTypeView -> TypeClass (ClassRecord possibleSubtypeName possibleParentRecord _ _)) <== parentRecord@(toTypeView -> TypeClass (ClassRecord superClassName _ _ _))
  | possibleSubtypeName == superClassName = return True
  | superClassName `elem` primitiveTypes = return False
  | otherwise = possibleParentRecord <== parentRecord
(toTypeView -> TypeClass ObjectClass) <== (toTypeView -> TypeClass ClassRecord {}) = return False
(toTypeView -> TypeClass _) <== (toTypeView -> TypeClass ObjectClass) = return True
possibleSubtype <== (toTypeView -> RegularType SELF_TYPE) = do
  (className, _) <- ask
  possibleSubtypeName <- lift $ getTypeName $ toTypeView possibleSubtype
  return (possibleSubtypeName == className)
possibleSubtype <== possibleParentType = do
  possibleSubtypeClassRecord <- lookupClass possibleSubtype
  possibleParentClassRecord <- lookupClass possibleParentType
  possibleSubtypeClassRecord <== possibleParentClassRecord

-- determines the lowest upper bound (lub) of two types
(\/) ::
     IsType a
  => IsType b =>
       a -> b -> SemanticAnalyzer Type
(toTypeView -> TypeClass leftClassRecord) \/ (toTypeView -> TypeClass rightClassRecord) =
  case leftClassRecord of
    ObjectClass -> objectType
    _ ->
      case rightClassRecord of
        ObjectClass -> objectType
        _ -> do
          let rightClassAncestors = computeAncestors rightClassRecord
          return $ toType $ lub leftClassRecord rightClassAncestors
  where
    computeAncestors ObjectClass = S.fromList ["Object"]
    computeAncestors (ClassRecord className' parent' _ _) = className' `S.insert` computeAncestors parent'
    lub ObjectClass _ = ObjectClass
    lub classRecord@(ClassRecord className' parent' _ _) ancestors
      | className' `elem` ancestors = classRecord
      | otherwise = lub parent' ancestors
    objectType = return $ TypeName "Object"
(toTypeView -> RegularType SELF_TYPE) \/ (toTypeView -> RegularType SELF_TYPE) = return SELF_TYPE
leftType \/ rightType =
  runMaybe (TypeName "Object") $ do
    possibleSubtypeClassRecord <- lookupClass leftType
    possibleParentClassRecord <- lookupClass rightType
    lift $ possibleSubtypeClassRecord \/ possibleParentClassRecord

(>==<) ::
     IsType a
  => IsType b =>
       a -> b -> SemanticAnalyzer Bool
leftType >==< rightType = (==) <$> coerceType leftType <*> coerceType rightType
  where
    coerceType :: IsType c => c -> SemanticAnalyzer String
    coerceType = getTypeName . toTypeView

-- temporarily adds a variable to the object environment
(/>) :: IsType a => (String, a) -> SemanticAnalyzer out -> SemanticAnalyzer out
(identifier', typeVal) /> semanticAnalyzer = do
  objectEnvironment <- get
  put $ M.insert identifier' (toType typeVal) objectEnvironment
  result <- semanticAnalyzer
  put objectEnvironment
  return result

lookupClass :: IsType a => a -> SemanticAnalyzerM ClassRecord
lookupClass (toTypeView -> TypeClass classRecord) = return classRecord
lookupClass typeVal = do
  (_, classEnvironment) <- ask
  typeString <- lift $ getTypeName $ toTypeView typeVal
  MaybeT $ return $ typeString `M.lookup` classEnvironment
