{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--For OrMatVec MulR type-level machinery
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
    MulR (Expr (FloatVec 3)) (Expr (FloatMat 3 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatVec 4)) (Expr (FloatMat 4 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatVec 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatVec 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatVec 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatMat 2 2)) = Expr (FloatMat 2 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatMat 3 3)) = Expr (FloatMat 3 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatMat 4 4)) = Expr (FloatMat 4 4)
    -- TODO nonsquare mats are supported in #version 120, we can think about that later

--The type error for this when partially applied sucks ASS
--The tilde stuff was because it wasn't able to deduce enough from just this
mul :: (OrMatVec a, OrMatVec b, MulR a b ~ Expr c, b ~ Expr b0, a ~ Expr a0, ToGLSLType a0, ToGLSLType b0, ToGLSLType c) => a -> b -> MulR a b
mul = op2 "*"

(*^*) :: (OrMatVec a, OrMatVec b, MulR a b ~ Expr c, b ~ Expr b0, a ~ Expr a0, ToGLSLType a0, ToGLSLType b0, ToGLSLType c) => a -> b -> MulR a b
(*^*) = op2 "*"
infix 6 *^*

-- | Floating matrix singleton type tag
data FloatMat (n :: Nat) (m :: Nat) where
    FloatMat :: (Mattable n m) => FloatMat n m

-- | A bunch of vecs is mattable if it can be the dimensions of a GLSL matrix
class (ToGLSLType (FloatMat n m), KnownNat n, KnownNat m) => Mattable n m where
    --I really don't know if these are needed for now
    copyM :: M11 -> Mat n m
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
    copyM v = op4pre' "mat2" v v
                             v v

instance Mattable 3 3 where
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
    (*) = op2' "*"
    abs = op1pre "abs"
    signum = op1pre "sign"
    negate = op1 "-"
    fromInteger x = copyM . uniform . show $ (fromInteger x :: Float)

instance (Mattable n m) => Fractional (Mat n m) where
    (/) = op2' "/"
    fromRational x = copyM . uniform . show $ (fromRational x :: Float)

instance (Mattable n m) => Floating (Mat n m) where
    -- pi = copy $ uniform "pi"
    pi = copyM $ uniform "3.141592653589793238462643383"
    exp = op1pre "exp"
    log = op1pre "log"
    sqrt = op1pre "sqrt"
    (**) = op2pre' "pow"
    sin = op1pre "sin"
    cos = op1pre "cos"
    tan = op1pre "tan"
    asin = op1pre "asin"
    acos = op1pre "acos"
    atan = op1pre "atan"
    sinh x = (exp x - exp (negate x)) / 2
    cosh x = (exp x + exp (negate x))/2
    tanh x = sinh x / cosh x
    asinh x = log $ x + sqrt(x**2 + 1)
    acosh x = log $ x + sqrt(x**2 - 1)
    atanh x = 0.5 * log ((1 + x)/(1 - x))

mat22 :: (M11, M11, M11, M11) -> M22
mat22 (a, b, c, d) = op4pre' "mat2" a b c d

mat33 :: (M11, M11, M11
         ,M11, M11, M11
         ,M11, M11, M11) -> M33
mat33 (a,b,c,d,e,f,g,h,i) = op9pre' "mat3" a b c d e f g h i

mat44 :: (M11, M11, M11, M11
         ,M11, M11, M11, M11
         ,M11, M11, M11, M11
         ,M11, M11, M11, M11) -> M44
mat44 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = op16pre' "mat4" a b c d e f g h i j k l m n o p
--We should think about https://github.com/sleexyz/hylogen/issues/60
--TODO other constructors
