{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where

  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) f (Compose fga) =
    let
      fa2fb = (f <$>)      -- :: f a -> f b
      lgb = fa2fb <$> fga  -- :: f (g b)
    in
      Compose lgb

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

  -- Implement the pure function for an Applicative instance for Compose
  pure x = Compose (pure (pure x))

  -- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose a2bInFG) (Compose fga) =
    let
      -- a2bInFG                   -- :: f (g (a -> b))
      -- fga                       -- :: f (g a)

      liftF = lift1                -- :: (a -> b) -> f a -> f b

      applyInG = (<*>)             -- ::    g (a -> b)  ->    g a -> g b

      applyInFG = liftF applyInG   -- :: f (g (a -> b)) -> f (g a -> g b)

      ga2gbInF = applyInFG a2bInFG -- ::                   f (g a -> g b)

                                   -- :: f (a   -> b)   -> f a     -> f b
      applyFG = (<*>)              -- :: f (g a -> g b) -> f (g a) -> f (g b)
      fga2fgb = applyFG ga2gbInF   -- ::                   f (g a) -> f (g b)
      fgb = fga2fgb fga            -- ::                              f (g b)
    in
      Compose fgb

      -- short version
      --
      -- Compose (((<*>) <$> ff) <*> fx)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
