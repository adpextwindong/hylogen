{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


module Hylogen.Types.Mat where

import GHC.TypeLits
import Data.VectorSpace

import Hylogen.Expr

-- | Floating matrix singleton type tag
data FloatMat (n :: Nat) (m :: Nat) where
    FloatMat :: (Mattable n m) => FloatMat n m

-- | A list of vecs is mattable if it can be the dimensions of a GLSL matrix
class (ToGLSLType (FloatMat n m), KnownNat n, KnownNat m) => Mattable n m where
    --I really don't know if these are needed for now
    --TODO ponder more about Veccable
    copyM :: M11 -> Mat n m
    --copyV :: Vec1 -> Mat n m
    --toListM :: Mat n m -> [M11]

-- | Expriemental Hylogen floating-point Matrix type
type Mat n m = Expr (FloatMat n m)
type M11 = Mat 1 1
type M12 = Mat 1 2
type M21 = Mat 2 1
type M22 = Mat 2 2
type M23 = Mat 2 3
type M32 = Mat 3 2
type M33 = Mat 3 3
type M34 = Mat 3 4
type M43 = Mat 4 3
type M44 = Mat 4 4

instance ToGLSLType (FloatMat 1 1) where
    toGLSLType _ = GLSLFloat
    tag = FloatMat

instance ToGLSLType (FloatMat 2 2) where
    toGLSLType _ = GLSLMat2
    tag = FloatMat

instance Mattable 1 1 where
    copyM = id

instance Mattable 2 2 where
    copyM v = op4pre' "mat2" v v v v
--TODO more instances

instance (Mattable n m) => Num (Mat n m) where
    (+) = op2' "+"
    (-) = op2' "-"
    (*) = op2' "*" -- TODO yeah idk about this one
    negate = op1 "-"
    fromInteger x = copyM . uniform . show $ (fromInteger x :: Float)

mat22 :: (M11, M11, M11, M11) -> M22
mat22 (a, b, c, d) = op4pre' "mat2" a b c d

--TEST FIXTURES
ts22 = mat22 (1, 1, 1, 1)
