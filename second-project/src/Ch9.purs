module Ch9 where
  
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (&&), (==))
import Prim.Boolean (True)


class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a


data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue) -- true

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 test)"
  log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue -- true
  log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse -- true

data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool
derive instance  genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue) -- true

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid laws (2 tests)"
  log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue -- true
  log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse -- true

test :: Effect Unit
test = do
  log $ show $ "--------Semigroup------------"
  log $ show $ ATrue <> ATrue -- ATrue
  log $ show $ ATrue <> AFalse -- AFalse
  log $ show $ AFalse <> AFalse -- AFalse
  log $ show $ "--------Monoid------------"
  log $ show $ mempty <> ATrue == ATrue -- true
  log $ show $ mempty <> AFalse == ATrue -- false
  log $ show $ "--------Verifying Laws------------"
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid



