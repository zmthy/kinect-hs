-- | A few operators to make working with function composition easier.
module Helpers.Composition ((.>), (.<)) where

-- | A composition operator that gives the function on the left a parameter
-- in the generated function along with the one used by the right.
(.>) :: (a -> b -> c) -> (d -> b) -> (a -> d -> c)
a .> b = \x y -> a x (b y)
infixr 1 .>

-- | A composition operator intended to be used in conjuction with '(.>)',
-- which consumes the function on the left and passes the result of the
-- function on the right to it.
(.<) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
a .< b = \x y -> a (b x y)
infixr 0 .<
