module Ch9 where
  
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Show, class Eq, ($), discard, show)


class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a -> a


data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue -- ATrue
  log $ show $ ATrue <> AFalse -- AFalse
  log $ show $ AFalse <> AFalse -- AFalse




