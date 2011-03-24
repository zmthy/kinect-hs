-- | A few operators for composing monad binds together.
module Helpers.Composition.Monad ((.>>), (.>>=), (<<=), (<>)) where

-- | A late application of '(>>)' that leaves a final parameter required
-- before completing the wrap into the given monad.
(.>>) :: Monad m => m a -> (b -> m c) -> (b -> m c)
a .>> b = \x -> a >> b x

-- | A late application of '(>>=)' that leaves a final parameter required
-- before completing the wrap into the given monad.
(.>>=) :: Monad m => m a -> (a -> b -> m c) -> (b -> m c)
a .>>= b = \x -> a >>= \y -> b y x

-- | A wrapper for '(>>=)' which leaves the parameter off of the left
-- function so that the resulting output takes it instead.
(<<=) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
a <<= b = \x -> a x >>= b
infixl 1 <<=

-- | A wrapper for '(>>)' which takes two functions with the same parameter
-- and applies it to them both in the specified order.
(<>) :: Monad m => (a -> m b) -> (a -> m c) -> (a -> m c)
a <> b = \x -> a x >> b x
infixl 1 <>
