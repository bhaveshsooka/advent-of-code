module Util.ExtraUtils where

import Control.Applicative (liftA3)

liftA4 :: (Applicative f) => (a1 -> b1 -> c -> a2 -> b2) -> f a1 -> f b1 -> f c -> f a2 -> f b2
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 :: (Applicative f) => (a1 -> b1 -> c -> a2 -> a -> b) -> f a1 -> f b1 -> f c -> f a2 -> f a -> f b
liftA5 f a b c d e = liftA4 f a b c d <*> e

liftA6 :: (Applicative f) => (a1 -> b1 -> c -> a2 -> a -> a3 -> b) -> f a1 -> f b1 -> f c -> f a2 -> f a -> f a3 -> f b
liftA6 f a b c d e g = liftA5 f a b c d e <*> g
