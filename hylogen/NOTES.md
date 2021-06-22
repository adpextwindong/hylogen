# Matrixes

We've added to the GLSLType internal type tag in Expr
    - GLSLMat2
    - GLSLMat3
    - GLSLMat4

To support operations we need to have instances for
    - Num
    - Fractional
    - Floating
    - AdditiveGroup
    - VectorSpace
    - InnerSpace --TODO look more at these Data.VectorSpace stuff

The ToVecN typeclass constructors and overlapping instances to support the different constructor styles
For now we can just implement the most basic constructor.

https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)#Matrices

I'm not sure yet where the type safety for dimension handling will come in.

GLSL also supports matnxm matrices. We can leave that for later while we prototype some stuff.

We will need some tests to make sure its type safe and emmits correct code for GLSL mats.
