module Ch7a where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (<),  (<=), (==),  (>), (>=))

data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow


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
  log "-----------------------"
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)