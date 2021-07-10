module Ch9 where
  
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (&&), (==))

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

-- Modular arithmetic
data Mod4 = Zero | One | Two | Three

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero x = x
  append x Zero = x

  append One One = Two
  append One Two = Three
  append One Three = Zero

  append Two One = Three
  append Two Two = Zero
  append Two Three = One

  append Three One = Zero
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero

class Monoid a <= Group a where
  ginverse :: a -> a

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One

derive instance eqMod4 :: Eq Mod4
derive instance  genericMod4 :: Generic Mod4 _

instance showMod4 :: Show Mod4 where
  show = genericShow


verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws (1 test)"
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three) -- true

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws (1 test)"
  log $ show $ mempty <> One == One <> mempty && One <> mempty == One -- true


newtype First a = First (Maybe a)
newtype Last a = Last (Maybe a)

instance semigroupFirst :: Semigroup (First a) where
  append (First Nothing) last = last
  append first _ = first

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

instance semigroupLast :: Semigroup (Last a) where
  append first (Last Nothing) = first
  append _ last = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

derive instance genericFirst :: Generic (First a) _

instance showFirst :: Show a => Show (First a) where
  show = genericShow

derive instance genericLast :: Generic (Last a) _

instance showLast :: Show a => Show (Last a) where
  show = genericShow


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
  log $ show $ "--------Maybe------------"
  log $ show $ First Nothing <> First (Just 77)
  log $ show $ Last (Just 1) <> Last (Just 99)





