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

instance Mattable 1 1 where
    copyM = id

-- | Floating vector singleton type tag
data FloatVec (n :: Nat) where
  FloatVec :: (Veccable n) => FloatVec n

-- | Hylogen floating-point Vector type
type Vec n = Expr (FloatVec n)
type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4

instance ToGLSLType (FloatVec 1) where
  toGLSLType _ = GLSLFloat
  tag = FloatVec
instance ToGLSLType (FloatVec 2) where
  toGLSLType _ = GLSLVec2
  tag = FloatVec
instance ToGLSLType (FloatVec 3) where
  toGLSLType _ = GLSLVec3
  tag = FloatVec
instance ToGLSLType (FloatVec 4) where
  toGLSLType _ = GLSLVec4
  tag = FloatVec



-- | A Nat is veccable if it can be the dimension of a GLSL vector
class (ToGLSLType (FloatVec n), KnownNat n) => Veccable n where
  -- | Creates a Vec n from a Vec1
  copy :: Vec1 -> Vec n
  -- | Transforms a Vec n into a list of Vec1's
  --toList :: Vec n -> [Vec1]


instance Veccable 1 where
  copy = id
  --toList v = [v]
instance Veccable 2 where
  copy v = op2pre' "vec2" v v
  --toList v = [x_ v, y_ v]
instance Veccable 3 where
  copy v = op3pre' "vec3" v v v
  --toList v = [x_ v, y_ v, z_ v]
instance Veccable 4 where
  copy v = op4pre' "vec4" v v v v
  --toList v = [x_ v, y_ v, z_ v]



instance (Veccable n) => Num (Vec n) where
  (+) = op2' "+"
  (-) = op2' "-"
  (*) = op2' "*"
  abs = op1pre "abs"
  signum = op1pre "sign"
  negate = op1 "-"
  fromInteger x = copy . uniform . show $ (fromInteger x :: Float)


instance (Veccable n) => Fractional (Vec n) where
  (/) = op2' "/"
  fromRational x = copy . uniform . show $ (fromRational x :: Float)

instance (Veccable n) => Floating (Vec n) where
  -- pi = copy $ uniform "pi"
  pi = copy $ uniform "3.141592653589793238462643383"
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

instance Veccable n => AdditiveGroup (Vec n) where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance Veccable n => VectorSpace (Vec n) where
  type Scalar (Vec n) = Vec 1
  a *^ b = copy a * b

instance Veccable n => InnerSpace (Vec n) where
  a <.> b = Expr fv (Tree (Op2Pre, GLSLFloat, "dot") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec 1





-- | Exposed constructor for making vec2's
vec2 :: (Vec1, Vec1) -> Vec2
vec2 (x, y) = op2pre' "vec2" x y


class ToVec3 tuple where
  -- | Exposed constructor for making vec3's
  vec3 :: tuple -> Vec3

instance (a ~ Vec m, b ~ Vec (3 - m)) => ToVec3 (a, b) where
  vec3 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec3") [toMono x, toMono y])
      where fv = FloatVec :: FloatVec 3

instance (a ~ Vec1, b ~ Vec1, c ~ Vec1) => ToVec3 (a, b, c) where
  vec3 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec3") (fmap toMono [x, y, z]))
      where fv = FloatVec :: FloatVec 3


class ToVec4 tuple where
  -- | Exposed constructor for making vec4's
  vec4 :: tuple -> Vec4

instance (a ~ Vec m, b ~ Vec (4 - m)) => ToVec4 (a, b) where
  vec4 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec4") [toMono x,toMono y])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (b ~ Vec1, c ~ Vec1) => ToVec4 (Vec2, b, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (a ~ Vec1, c ~ Vec1) => ToVec4 (a, Vec2, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (a ~ Vec1, b ~ Vec1) => ToVec4 (a, b, Vec2) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4


instance (a ~ Vec1, b ~ Vec1, c ~ Vec1, d ~ Vec1) => ToVec4 (a, b, c, d) where
  vec4 (x, y, z, w) = Expr fv (Tree (Op4Pre, toGLSLType fv, "vec4") (fmap toMono [x, y, z, w]))
      where fv = FloatVec :: FloatVec 4
