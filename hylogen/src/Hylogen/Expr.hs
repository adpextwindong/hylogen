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
{-# LANGUAGE DeriveFunctor #-}

{- |
Internal AST representation.
-}

module Hylogen.Expr where


import Data.Reify


-- | Internal type tag
data GLSLType = GLSLFloat
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLMat2
              | GLSLMat3
              | GLSLMat4
              | GLSLBool
              | GLSLTexture
              deriving (Eq, Ord)

instance Show GLSLType where
  show x = case x of
    GLSLFloat -> "float"
    GLSLVec2 -> "vec2"
    GLSLVec3 -> "vec3"
    GLSLVec4 -> "vec4"
    GLSLMat2 -> "mat2"
    GLSLMat3 -> "mat3"
    GLSLMat4 -> "mat4"
    GLSLBool -> "bool"
    GLSLTexture -> "sampler2D"

-- | Internal form tag
data ExprForm = Uniform
              | Variable
              | Op1
              | Op1Pre
              | Op2
              | Op2Pre
              | Op3Pre
              | Op4Pre
              | Op9Pre
              | Op16Pre
              | Select
              | Access
                deriving (Show)

-- | Rose tree. Internal AST data structure
data Tree a  = Tree { getElem     :: a
                    , getChildren :: [Tree a]
                    }


-- | Untyped Expr representation
-- Carries type information in type tag
type ExprMono = Tree (ExprForm, GLSLType, String)

instance Show ExprMono where
  show (Tree (form, _, str) xs) = case form of
    Uniform  -> str
    Variable -> str
    Op1      -> mconcat ["(", str, show (xs!!0), ")"]
    Op1Pre   -> mconcat [ str, "(", show (xs!!0), ")"]
    Op2      -> mconcat ["(", show (xs !! 0), " ", str, " ", show (xs !! 1), ")"]
    Op2Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ")"]
    Op3Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ")"]
    Op4Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ", ", show (xs!!3), ")"]

    --FOR MATRIX
    Op9Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2),
                              ", ",show (xs!!3), ", ", show (xs!!4), ", ", show (xs!!5),
                              ", ",show (xs!!6), ", ", show (xs!!7), ", ", show (xs!!8), ")"]

    --FOR MATRIX
    Op16Pre  -> mconcat [str, "(",
                              ", ", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ", ", show (xs !! 3),
                              ", ", show (xs!!4), ", ", show (xs!!5), ", ", show (xs!!6), ", ", show (xs !! 7),
                              ", ", show (xs!!8), ", ", show (xs!!9), ", ", show (xs!!10), ", ", show (xs !! 11),
                              ", ", show (xs!!12), ", ", show (xs!!13), ", ", show (xs!!14), ", ", show (xs !! 15), ")"]


    Select   -> mconcat ["( ", show (xs!!0), " ? ", show (xs!!1), " : ", show (xs!!2), ")"]
    Access   -> mconcat [show (xs!!0), ".", str]

-- | Light type wrapper
--
-- Note the internal type tag is not directly dependent on the actual type!
--
-- We use the ToGLSLType typeclass to genenerate dependence from types to values
data Expr ty = Expr { getTypeTag :: ty
                    , toMono ::  Tree (ExprForm, GLSLType, String)
                    }

instance ToGLSLType ty => Show (Expr ty) where
  show = show . toMono

class ToGLSLType  ty where
  -- | Gives us dependence from typed singleton tags to untyped tags
  toGLSLType :: ty -> GLSLType
  -- | Singleton tag
  tag :: ty -- TODO: fill in!

-- | Uniform expression.
uniform :: forall a
           . ToGLSLType a
           => String -> Expr a
uniform str = Expr t (Tree (Uniform, toGLSLType t, str) [])
  where t = tag :: a

-- | Unary operator.
-- Most generally typed.
op1 :: forall a b
       . (ToGLSLType a, ToGLSLType b)
       => String -> Expr a -> Expr b
op1 str a = Expr t (Tree (Op1, toGLSLType t, str) [toMono a])
  where t = tag :: b

-- | Unary operator.
-- Input and output values have the same type.
op1'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a
op1'' str a = Expr t (Tree (Op1, toGLSLType t, str) [toMono a])
  where t = tag :: a

-- | Unary operator.
-- Prefix function call style.
-- Most generally typed.
op1pre :: forall a b
          . (ToGLSLType a, ToGLSLType b)
          => String -> Expr a -> Expr b
op1pre str a = Expr t (Tree (Op1Pre, toGLSLType t, str) [toMono a])
  where t = tag :: b

-- | Unary operator.
-- Prefix function call style.
-- Input and output values have the same type.
op1pre'' :: forall a
          . (ToGLSLType a)
          => String -> Expr a -> Expr a
op1pre'' str a = Expr t (Tree (Op1Pre, toGLSLType t, str) [toMono a])
  where t = tag :: a

-- | Binary operator.
-- Most generally typed.
op2 :: forall a b c
       . (ToGLSLType a, ToGLSLType b, ToGLSLType c)
       => String -> Expr a -> Expr b -> Expr c
op2 str a b = Expr t (Tree (Op2, toGLSLType t, str) [toMono a, toMono b])
  where t = tag :: c

-- | Binary operator.
-- Arguments have the same type.
op2' :: forall a c
       . (ToGLSLType a, ToGLSLType c)
       => String -> Expr a -> Expr a -> Expr c
op2' str a b = Expr t (Tree (Op2, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: c

-- | Binary operator.
-- Input and output values have the same type.
op2'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a -> Expr a
op2'' str a b = Expr t (Tree (Op2, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: a


-- | Binary operator.
-- Prefix function call style.
-- Most generally typed.
op2pre :: forall a b c
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c)
          => String -> Expr a -> Expr b -> Expr c
op2pre str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) [toMono a, toMono b])
  where t = tag :: c

-- | Binary operator.
-- Prefix function call style.
-- Arguments have the same type.
op2pre' :: forall a c
       . (ToGLSLType a, ToGLSLType c)
       => String -> Expr a -> Expr a -> Expr c
op2pre' str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: c

-- | Binary operator.
-- Prefix function call style.
-- Input and output values have the same type.
op2pre'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a -> Expr a
op2pre'' str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: a

-- | Ternary operator.
-- Prefix function call style.
-- Most generally typed.
op3pre :: forall a b c d
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c, ToGLSLType d)
          => String -> Expr a -> Expr b -> Expr c -> Expr d
op3pre str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) [toMono a, toMono b, toMono c])
  where t = tag :: d

-- | Ternary operator.
-- Prefix function call style.
-- Arguments have the same type.
op3pre' :: forall a d
          . (ToGLSLType a, ToGLSLType d)
          => String -> Expr a -> Expr a -> Expr a -> Expr d
op3pre' str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) (fmap toMono [a, b, c]))
  where t = tag :: d

-- | Ternary operator.
-- Prefix function call style.
-- Input and output values have the same type.
op3pre'' :: forall a
          . (ToGLSLType a)
          => String -> Expr a -> Expr a -> Expr a -> Expr a
op3pre'' str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) (fmap toMono [a, b, c]))
  where t = tag :: a


-- | Quaternary operator.
-- Prefix function call style.
-- Most generally typed.
op4pre :: forall a b c d e
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c, ToGLSLType d, ToGLSLType e)
          => String -> Expr a -> Expr b -> Expr c -> Expr d -> Expr e
op4pre str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) [toMono a, toMono b, toMono c, toMono d])
  where t = tag :: e

-- | Quaternary operator.
-- Prefix function call style.
-- Arguments have the same type.
op4pre' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a -> Expr a -> Expr e
op4pre' str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) (fmap toMono [a, b, c, d]))
  where t = tag :: e

-- | Quaternary operator.
-- Prefix function call style.
-- Input and output values have the same type.
op4pre'' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a -> Expr a -> Expr e
op4pre'' str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) (fmap toMono [a, b, c, d]))
  where t = tag :: e


-- NOTE THIS IS FOR THE MATRIX STUFF
-- | 9nary operator.
-- Prefix function call style.
-- Arguments have the same type.
op9pre' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a
                    -> Expr a -> Expr a -> Expr a
                    -> Expr a -> Expr a -> Expr a
                    -> Expr e
op9pre' str a b c d e f g h i = Expr t (Tree (Op9Pre, toGLSLType t, str) (fmap toMono [a, b, c, d, e, f, g, h, i]))
  where t = tag :: e

-- NOTE THIS IS FOR THE MATRIX STUFF
-- | 16nary operator.
-- Prefix function call style.
-- Arguments have the same type.
op16pre' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a -> Expr a
                    -> Expr a -> Expr a -> Expr a -> Expr a
                    -> Expr a -> Expr a -> Expr a -> Expr a
                    -> Expr a -> Expr a -> Expr a -> Expr a
                    -> Expr e
op16pre' str a b c d
             e f g h
             i j k l
             m n o p = Expr t (Tree (Op9Pre, toGLSLType t, str) (fmap toMono [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]))
  where t = tag :: e

-- | Open tree type, to be used for explicit recursion with data-reify for preserving sharing.
--
-- Note the second argument of the constructor is a list of Maybe b's.
-- We use Maybe's to determine whether or not a child expression gets inlined.
data TreeF a b = TreeF { getElemF     :: a
                       , getChildrenF   :: [Maybe b]
                       }
                 deriving (Functor)

-- | Open untyped expression representation, to be used for explicit recursion with data-reify for preserving sharing.
--
-- Note the presence of a list of closed ExprMono's in the tuple.
-- We use this list to recover unshared child expressions when they need to be inlined.
type ExprMonoF = TreeF (ExprForm, GLSLType, String, [ExprMono])

-- | Returns the string representation of the nth child of an open untyped expression, accounting for inlining
emfStringAt :: (Show a) => ExprMonoF a -> Int -> String
emfStringAt (TreeF (_, _, _, xs) ys)  i = zipWith fn xs ys !! i
  where
    fn x Nothing = show x
    fn _ (Just y)= show y

instance (Show a) => Show (ExprMonoF a) where
  show expr@(TreeF (form, _, str, _) _) = case form of
    Uniform  -> str
    Variable -> str
    Op1      -> mconcat ["(", str, strAt 0, ")"]
    Op1Pre   -> mconcat [ str, "(", strAt 0, ")"]
    Op2      -> mconcat ["(", strAt 0, " ", str, " ", strAt 1, ")"]
    Op2Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ")"]
    Op3Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ")"]
    Op4Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ", ", strAt 3, ")"]
    Op9Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ", ", strAt 3, ", ", strAt 4, ", ", strAt 5, ", ", strAt 6, ", ", strAt 7, ", ", strAt 8, ", ", strAt 9,")"]
    Op16Pre  -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ", ", strAt 3, ", ", strAt 4, ", ", strAt 5, ", ", strAt 6, ", ", strAt 7, ", ", strAt 8, ", ", strAt 9, ", ", strAt 10, ", ", strAt 11, ", ", strAt 12, ", ", strAt 13, ", ", strAt 14, ", ", strAt 15, ", ", strAt 16, ")"]
    Select   -> mconcat ["( ", strAt 0, " ? ", strAt 1, " : ", strAt 2, ")"]
    Access   -> mconcat [strAt 0, ".", str]
    where
      strAt = emfStringAt expr

-- | Currently only inlines uniforms.
instance MuRef ExprMono where
  type DeRef ExprMono = ExprMonoF
  mapDeRef func (Tree (form, ty, str) xs) = TreeF (form, ty, str, xs) <$> g xs
    where
      g (x:xs) = (:) <$> (traverse func $ shouldShare x) <*> (g $  xs)
      g [] = pure []

      shouldShare :: ExprMono -> Maybe ExprMono
      shouldShare (Tree (Uniform, _, _) _) = Nothing
      shouldShare expr = Just expr
