module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

-- newtype CSV
newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

-- newtype FullName
newtype FullName = FullName String

derive instance newtypeFullName :: Newtype FullName _
derive newtype instance eqFullName :: Eq FullName

instance showFullName :: Show FullName where
  show (FullName name) = name

-- newtype Age
newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

-- newtype Occupation
data Occupation = Developer | Dentist | Lawyer | Unemployed

derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

data Person = Person 
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  -- toCSV  :: a -> CSV
  toCSV (Person {name, age, occupation}) =
    CSV $ show name <> "," <> show age <> "," <> show occupation

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Developer" -> Just Developer
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

instance fromCSVPerson :: FromCSV Person where
  -- fromCSV :: CSV -> Maybe a
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person
          { name: FullName name
          , age: Age age'
          , occupation: occupation'
          }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

test :: Effect Unit
test = do
  -- log $ show $ toCSV 
  --   (Person 
  --     { name: FullName "Andres Villarreal"
  --     , age: Age 38
  --     , occupation: Developer })
  log $ show $ toCSV 
    (Person 
      { name: FullName "Andres Villarreal"
      , age: Age 38
      , occupation: Developer }) == CSV "Andres Villarreal,38,Developer"      
  let person = Person
          { name: FullName "Andres Villarreal"
          , age: Age 38
          , occupation: Developer
          }
  log $ show $ (toCSV person # fromCSV) == Just person 