module Ch5 where
  
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), optional)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (==), (>), (>=), (<<<), (>>>), max)




flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixr 0 apply as $
infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just(x)

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just(xs)

last :: ∀ a. List a -> Maybe a 
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index l i = go 0 l where
  go _ Nil = Nothing
  go ci (x : xs) -- ci means current index
    | i < 0 = Nothing
    | ci == i =  Just x
    | otherwise = go (ci + 1) xs

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex pred l = go 0 l where
  go _ Nil = Nothing
  go i (x : xs) = if pred x then Just i else go (i+1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = go Nothing 0 l where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go fi _ Nil = fi
  go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs

reverse :: List ~> List
reverse Nil = Nil
reverse ol = go Nil ol where
  go rl Nil = rl
  go rl (x: xs) = go (x : rl) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x: xs) : xss) = x : concat (xs : xss)
{-- 
-- todo: why it doesn't work?
concat ol = go Nil ol where
  go al Nil = al
  go al (x : xs) = go (x: al) xs --}

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter pred ol = reverse $ go Nil ol where
  go nl Nil = nl
  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

range :: Int -> Int -> List Int
range start end = go Nil end start where
  go rl start' end'
    | start' == end' = start' : rl
    | otherwise = go (start' : rl) (start' + step) end'
  step = if start < end then (-1) else 1

take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n) where
  go nl _ Nil = nl
  go nl 0 _  = nl 
  go nl n' (x : xs) = go (x : nl) (n' - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 l = l
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \tup@(Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else tup

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs 
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else  x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) =
  unzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log 
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123": Nil)
  log $ show (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1: Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil ) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4: 5: Nil) : (6: Nil): (Nil): Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12: 13 : 45 : 976: -19 : Nil)
  log $ show $ drop 2 (1 : 2 : 3: 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log $ show $ takeWhile (_ > 3) (5: 4 : 3: 99: 101: Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ dropWhile (_ > 3) (5: 4 : 3: 99: 101: Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ takeEnd 3 (1 : 2 : 3: 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log $ show $ dropEnd 3 (1: 2: 3: 4: 5: 6: Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log $ show $ zip (1 : 2: 3: Nil) ("a": "b": "c": "d": "e": Nil)
  log $ show $ zip ("a": "b": "c": "d": "e": Nil) (1: 2: 3: Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c": Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List(Tuple Unit Unit))