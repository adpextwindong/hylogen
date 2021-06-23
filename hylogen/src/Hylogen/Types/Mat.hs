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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Hylogen.Types.Mat where

import GHC.TypeLits
import Data.VectorSpace

import Hylogen.Expr
import Hylogen.Types.Vec

type family OrMatVec' a where
    OrMatVec' (Expr (FloatMat n m)) = 'True
    OrMatVec' (Expr (FloatVec n)) = 'True

type OrMatVec t = (OrMatVec' t ~ 'True)

type family MulR a b where
    MulR (Expr (FloatVec 2)) (Expr (FloatMat 2 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatMat 2 2)) (Expr (FloatVec 2)) = Expr (FloatVec 2)
    -- TODO fill out this table reasonably enough

--The type error for this when partially applied sucks ASS
--The tilde stuff was because it wasn't able to deduce enough from just this
--baz :: (OrMatVec a, OrMatVec b) => a -> b -> MulR a b
mul :: (OrMatVec a, OrMatVec b, MulR a b ~ Expr c, b ~ Expr b0, a ~ Expr a0, ToGLSLType a0, ToGLSLType b0, ToGLSLType c) => a -> b -> MulR a b
mul = op2 "*"

--"Illegal type synonym family application 'MulR a b' in instance"
--hmmm
--instance (OrMatVec a, OrMatVec b) => Num (MulR a b) where
    --(*) = op2 "*"

--Holy shit this works
tbaz = mul ts22 tx
tbaz2 = mul tx ts22

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
type M22 = Mat 2 2
type M33 = Mat 3 3
type M44 = Mat 4 4

instance ToGLSLType (FloatMat 1 1) where
    toGLSLType _ = GLSLFloat
    tag = FloatMat

instance ToGLSLType (FloatMat 2 2) where
    toGLSLType _ = GLSLMat2
    tag = FloatMat

instance ToGLSLType (FloatMat 3 3) where
    toGLSLType _ = GLSLMat3
    tag = FloatMat

instance ToGLSLType (FloatMat 4 4) where
    toGLSLType _ = GLSLMat4
    tag = FloatMat

instance Mattable 1 1 where
    copyM = id

instance Mattable 2 2 where
    copyM v = op4pre' "mat2" v v v v
--TODO more instances

instance Mattable 3 3 where
    --copyM = op9pre' "mat3"
    copyM v = op9pre' "mat3" v v v
                             v v v
                             v v v

instance Mattable 4 4 where
    copyM v = op16pre' "mat4" v v v v
                              v v v v
                              v v v v
                              v v v v

instance (Mattable n m) => Num (Mat n m) where
    (+) = op2' "+"
    (-) = op2' "-"
    (*) = op2' "*" -- TODO yeah idk about this one
    negate = op1 "-"
    fromInteger x = copyM . uniform . show $ (fromInteger x :: Float)

--Darn looks like we need to figure this out some more
--instance {-#OVERLAPPABLE#-} (a ~ Mattable n n, b ~ Veccable n) => Num (Vec n) where

--mul :: (ToGLSLType (FloatVec n), ToGLSLType (FloatMat n n)) => Mat n n -> Vec n  -> Vec n
--mul m v = op2 "*" m v

--instance (ToGLSLType (FloatVec n), ToGLSLType (FloatMat n n)) => Num (Mat n n) where
--    (*) = op2 "*"



type (<) x y = (x + 1  <=? y) ~ 'False

mat22 :: (M11, M11, M11, M11) -> M22
mat22 (a, b, c, d) = op4pre' "mat2" a b c d

--TEST FIXTURES
ts22 :: M22
ts22 = mat22 (1, 1, 1, 1)
tv :: Vec2
tv = vec2 (1.0, 1.0)
tx :: Vec2
tx = mul ts22 tv

tprog :: Vec4
tprog = vec4 (tx, tx)
