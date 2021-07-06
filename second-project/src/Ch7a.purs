module Ch7a where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (||), (<>))

data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  -- eq :: a -> a -> Boolean
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false


-- data Ordering = LT | GT | EQ
instance ordMaybe :: Ord a => Ord (Maybe a) where
  --compare :: a -> a -> Ordering
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare Nothing _ = LT
  compare _ Nothing = GT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == GT || cmp == EQ where cmp = compare x y

infixl 4 greaterThanOrEq as >=

instance showMaybe :: Show a =>  Show (Maybe a) where
  -- show :: a -> String
  show Nothing = "Nothing"
  show (Just x) = "(Just " <> show x <> ")"


test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5 -- true
  log $ show $ Just 5 == Just 2 -- false
  log $ show $ Just 5 == Nothing -- false
  log $ show $ Nothing == Just 5 -- false
  log $ show $ Nothing == (Nothing :: Maybe Unit) -- true
  log "-----------------------"
  log $ show $ Just 1 < Just 5 -- true
  log $ show $ Just 5 <= Just 5 -- true
  log $ show $ Just 5 > Just 10 -- false
  log $ show $ Just 10 >= Just 10 -- true
  log $ show $ Just 99 > Nothing -- true
  log $ show $ Just 99 < Nothing -- false
  log "-----------------------"
  log $ show $ Just "abc" -- (Just "abc")
  log $ show $ (Nothing :: Maybe Unit) -- Nothing