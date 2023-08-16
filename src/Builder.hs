module Builder where

import Prelude hiding (All)

import SAT.MiniSat
import Control.Monad.Writer(Writer,tell, execWriter)

data Var =
  Region Int Int Int -- x,y is in region r
  | Edge Int Int Bool -- True is Vertical
  deriving stock (Eq,Ord,Show)

type Builder = ReaderT (Int,Int,Int) (Writer Conjunction) ()

newtype Conjunction = Conj{getConj :: Formula Var}

instance Semigroup Conjunction where
  (<>) = coerce $ (:&&:) @Var

instance Monoid Conjunction where
  mempty = coerce $ Yes @Var
  mconcat = coerce $ All @Var

assert :: Formula Var -> Builder
assert = tell . Conj

exactlyN :: Int -> [Formula v] -> Formula v
exactlyN 0 fs = None fs
exactlyN 1 fs = ExactlyOne fs
exactlyN n fs = let
  l = length fs
  h = l `div` 2
  ls = take h fs
  rs = drop h fs
    in if | n > l -> No
          | l == n -> All fs
          | otherwise -> Some [ exactlyN i ls :&&: exactlyN (n-i) rs | i <- [0..n] ]
-- can this be any better?

makeRegions :: Builder
makeRegions = do
  (x,y,rs) <- ask
  let rm = (x*y) `div` rs
  -- Each cell in one region
  forM_ ((,) <$> [0..x-1] <*> [0..y-1]) $ \(i,j) -> do
    assert $ ExactlyOne [ Var $ Region i j r | r <- [0..rm-1]]
  -- Each region is the right size
  forM_ [0..rm-1] $ \r ->
    assert $ exactlyN rs
      [ Var $ Region i j r
      | i <- [0..x-1]
      , j <- [0..y-1] ]
  -- vertical edges
  forM_ ((,) <$> [0..x-2] <*> [0..y-1]) $ \(i,j) -> do
    assert $ Var (Edge i j True)
      :<->: None
        [ Var (Region i j r) :&&: Var (Region (i+1) j r)
        | r <- [0..rm-1] ]
  -- horizontal edges
  forM_ ((,) <$> [0..x-1] <*> [0..y-2]) $ \(i,j) -> do
    assert $ Var (Edge i j False)
      :<->: None
        [ Var (Region i j r) :&&: Var (Region i (j+1) r)
        | r <- [0..rm-1] ]

setCount :: Int -> Int -> Int -> Builder
setCount i j c = do
  (x,y,_rs) <- ask
  let es =
        [ Edge i j True | i < x ] <>
        [ Edge i j False | j < y ] <>
        [ Edge (i-1) j True | i > 0] <>
        [ Edge i (j-1) False | j > 0]
      base = 4 - length es
      -- number of boundry edges
      -- needed because they count toward cell numbers
  assert $ exactlyN (c-base) $ Var <$> es

run :: Int -> Int -> Int -> [(Int,Int,Int)] -> Maybe (Map Var Bool)
run x y rs es = solve $ getConj $ execWriter $ (`runReaderT` (x,y,rs)) $ do
  makeRegions
  forM_ es $ \(i,j,c) -> setCount i j c

test :: Maybe (Map Var Bool)
test =
  --run 3 3 3 []
  run 5 5 5
  [(2,0,2),(3,0,3),(4,0,2)
  ,(0,1,2),(1,1,3),(2,1,2),(2,3,3)
  ,(1,2,2),(3,2,3)
  ,(1,3,1),(4,3,3)
  ,(4,4,3)
  ]

