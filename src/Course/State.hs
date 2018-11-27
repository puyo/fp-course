{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import Debug.Trace
import Data.Char

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState :: s -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec :: State s a -> s -> s
exec state s = snd (runState state s)

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval state s = fst (runState state s)

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\x -> (x, x))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put x = State (\_ -> ((), x))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  f <$> state = State (\result1 ->
                        let
                          -- get x
                          (x, result2) = runState state result1
                        in
                          -- call (f x) to produce a new value
                          (f x, result2)
                      )

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\y -> (x, y))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  stateF <*> stateX = State (\result1 ->
                              let
                                -- get f from stateF
                                (f, result2) = runState stateF result1

                                -- get x from stateX
                                (x, result3) = runState stateX result2
                              in
                                -- call (f x) to produce a new value
                                (f x, result3)
                            )

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)

-- Earlier attempt: this passes tests but causes later exercises to fail, I'm sure it's stupid
-- but I'll leave it here to come back and understand why
--
-- instance Monad (State s) where
--   (=<<) :: (a -> State s b) -> State s a -> State s b
--   (=<<) f2 s1 = State (\x -> (e1 x, e2 x))
--     where
--       s2 x = f2 (eval s1 x) -- 2nd state to apply is a function of the first state's evaluation
--       e1 x = eval (s2 x) x
--       e2 x = exec (s2 x) (exec s1 x)

instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  (=<<) f state = State (\result1 ->
                          let
                            -- get x
                            (x, result2) = runState state result1
                          in
                            -- call (f x) to produce a new state
                            runState (f x) result2
                        )

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
--
--
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil = return Empty
findM p (x :. xs) =
  p x
  >>= \b -> if b then return (Full x) else findM p xs

-- attempt 2: foldRight, but maybe this forces it to inspect every value in the list rather than short circuit?
-- findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
-- findM p xs = foldRight folder (return Empty) xs
--   where
--     folder x result =
--       p x >>= \b -> if b then return (Full x) else result

-- rewriting this...
--
-- let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- let p x = get >>= (\s -> (const $ pure (x == 'c')) =<< put (1+s)) in runState (findM p $ listh ['a'..'h']) 0
-- let p x = get >>= \s -> put (1 + s) >>= \_ -> pure (x == 'c') in runState (findM p (listh ['a'..'h'])) 0
--                    0         1 + 0       ()         False
--                    1         1 + 1       ()         False
--                    2         1 + 2       ()         True

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.

-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
--
-- attempt 1: my misconception of what was being asked because I couldn't
-- read the test cases
--
-- firstRepeat :: Ord a => List a -> Optional a
-- firstRepeat Nil = Empty
-- firstRepeat (_ :. Nil) = Empty
-- firstRepeat (x1 :. (x2 :. xs))
--   | x1 == x2 = Full x1
--   | otherwise = firstRepeat (x2 :. xs)

-- attempt 2: just doin' it my way, worked first time
--
-- firstRepeat :: Ord a => List a -> Optional a
-- firstRepeat xs = firstRepeat1 xs S.empty
--   where
--     firstRepeat1 :: Ord a => List a -> S.Set a -> Optional a
--     firstRepeat1 Nil _ = Empty
--     firstRepeat1 (x :. rest) set
--       | S.member x set = Full x
--       | otherwise = firstRepeat1 rest (S.insert x set)

-- attempt 3: using findM and a State
--
firstRepeat :: Ord a => List a -> Optional a
firstRepeat xs = eval (findM p xs) S.empty
  where
    p x = State $ \set -> (S.member x set, S.insert x set)

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
-- debugging with ints

traceMonad :: (Show a, Monad m) => [Char] -> a -> m a
traceMonad str x = trace (str P.++ show x) (return x)

findMN :: Monad f => (Int -> f Bool) -> List Int -> f (Optional Int)
findMN p xs = foldRight (findMNFold p) (return Empty) xs

findMNFold :: Monad f => (Int -> f Bool) -> Int -> f (Optional Int) -> f (Optional Int)
findMNFold p x result =
      p x
      >>= \b -> if b then return (Full x) else result
      >>= traceMonad "result = "

firstRepeatN :: List Int -> Optional Int
firstRepeatN xs = fst $ runState (findMN p xs) S.empty
  where
    p x | trace ("firstRepeat p x, x = " P.++ show x) False = undefined
    p x = get >>= \set -> State $ \xyz -> (S.member x (trace (show xyz) set), S.insert x xyz)

memberAndAddN :: Int -> State (S.Set Int) Bool
memberAndAddN x | trace ("memberAndAddN x = " P.++ show x) False = undefined
memberAndAddN x = State (\set -> memberAndAddSetN x set)

memberAndAddSetN :: Int -> S.Set Int -> (Bool, S.Set Int)
memberAndAddSetN x set | trace ("memberAndAddSetN x = " P.++ show x P.++ ", set = " P.++ show set) False = undefined
memberAndAddSetN x set = (S.member x set, S.insert x set)

--
-- at ghci
-- let p x = get >>= \s -> put (Data.Set.insert x s) >>= \_ -> return (Data.Set.member x s) in runState (findM p (listh [1,2,3,1,2,3])) Data.Set.empty
--
-- simpler without bind:
--
-- let p x = State (\set -> (Data.Set.member x set, Data.Set.insert x set)) in runState (findM p (listh [1,2,3,1,2,3])) Data.Set.empty

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--
-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)

-- attempt 1: not using filtering and State
--
-- distinct :: Ord a => List a -> List a
-- distinct xs = distinct1 xs S.empty
--   where
--     distinct1 :: Ord a => List a -> S.Set a -> List a
--     distinct1 Nil _ = Nil
--     distinct1 (x :. rest) set
--       | S.member x set = distinct1 rest (S.insert x set)
--       | otherwise = x :. (distinct1 rest (S.insert x set))

-- attempt 2: using filtering and State
distinct :: Ord a => List a -> List a
distinct xs = eval (filtering p xs) S.empty
  where
    p x = State $ \set -> (not $ S.member x set, S.insert x set)

-- A happy number is a positive integer, where the sum of the square of its
-- digits eventually reaches 1 after repetition. In contrast, a sad number
-- (not a happy number) is where the sum of the square of its digits never
-- reaches 1 because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True

-- attempt 1: NOT using firstRepeat, produce, join, contains
--
-- isHappy :: Integer -> Bool
-- isHappy n = isHappy1 n S.empty
--   where
--     isHappy1 :: Integer -> S.Set Integer -> Bool
--     isHappy1 1 _ = True
--     isHappy1 m set
--       | S.member m set = False
--       | otherwise = isHappy1 (sumOfSquareOfDigits m) (S.insert m set)
--     sumOfSquareOfDigits :: Integer -> Integer
--     sumOfSquareOfDigits m = sumIntegers (map square (digits m))
--     sumIntegers :: List Integer -> Integer
--     sumIntegers ms = foldRight (+) 0 ms
--     square :: Integer -> Integer
--     square m = m * m
--     digits :: Integer -> List Integer
--     digits m = map charToInteger (listh (show m))
--     charToInteger :: Char -> Integer
--     charToInteger = toInteger . Data.Char.digitToInt

-- attempt 2: using firstRepeat, produce, join, contains
isHappy :: Integer -> Bool
isHappy n = contains 1 $ firstRepeat (produce sumOfSquareOfDigits n)
  where
    sumOfSquareOfDigits :: Integer -> Integer
    sumOfSquareOfDigits m = sumIntegers (map square (digits m))

    sumIntegers :: List Integer -> Integer
    sumIntegers = foldRight (+) 0

    square :: Integer -> Integer
    square = join (*)

    digits :: Integer -> List Integer
    digits m = map charToInteger (listh (show m))

    charToInteger :: Char -> Integer
    charToInteger = toInteger . Data.Char.digitToInt
