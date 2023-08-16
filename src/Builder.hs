module Builder where

import Prelude hiding (All)

import SAT.MiniSat
import Control.Monad.Writer(Writer,tell, execWriter)

data Var =
  Region Point Int
  | Edge Point Bool -- True is Vertical
  | Dist Point Point Int
  deriving stock (Eq,Ord,Show)

type Point = (Int,Int)

type Builder = ReaderT (Int,Int,Int) (Writer Conjunction)

newtype Conjunction = Conj{getConj :: Formula Var}

instance Semigroup Conjunction where
  (<>) = coerce $ (:&&:) @Var

instance Monoid Conjunction where
  mempty = coerce $ Yes @Var
  mconcat = coerce $ All @Var

assert :: Formula Var -> Builder ()
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

makeRegions :: Builder ()
makeRegions = do
  (x,y,rs) <- ask
  let rm = (x*y) `div` rs
  -- Each cell in one region
  let pts = (,) <$> [0..x-1] <*> [0..y-1]
  forM_  pts $ \p -> do
    assert $ ExactlyOne [ Var $ Region p r | r <- [0..rm-1]]
  -- Each region is the right size
  forM_ [0..rm-1] $ \r ->
    assert $ exactlyN rs
      [ Var $ Region (i,j) r
      | i <- [0..x-1]
      , j <- [0..y-1] ]
  -- vertical edges
  forM_ ((,) <$> [0..x-2] <*> [0..y-1]) $ \(i,j) -> do
    assert $
        Var (Edge (i,j) True)
        :<->: None
        [ Var (Region (i,j) r) :&&: Var (Region (i+1,j) r)
        | r <- [0..rm-1] ]
    assert $
      Not (Var (Edge (i,j) True)) :<->: Var (Dist (i,j) (i+1,j) 1)
    assert $
      Not (Var (Edge (i,j) True)) :<->: Var (Dist (i+1,j) (i,j) 1)
  -- horizontal edges
  forM_ ((,) <$> [0..x-1] <*> [0..y-2]) $ \(i,j) -> do
    assert $ Var (Edge (i,j) False)
      :<->: None
        [ Var (Region (i,j) r) :&&: Var (Region (i,j+1) r)
        | r <- [0..rm-1] ]
    assert $
      Not (Var (Edge (i,j) False)) :<->: Var (Dist (i,j) (i,j+1) 1)
    assert $
      Not (Var (Edge (i,j) False)) :<->: Var (Dist (i,j+1) (i,j) 1)
  -- Define distances
  forM_ ((,) <$> pts <*> [2..rs-1]) $ \(p1,d) -> do
    pts2 <- neighbors p1 d
    forM_ pts2 $ \p2 -> do
      ns <- neighbors p2 1
      assert $ Var (Dist p1 p2 d)
        :<->: Some
          [ Var (Dist p1 n (d-1))
          :&&: Var (Dist n p2 1)
          | n <- ns]
  forM_ ((,) <$> pts <*> pts) $ \(p1,p2) -> do
    assert $
      None [ Var $ Dist p1 p2 d | d <- [0..dist p1 p2-1] ]
    assert $
      Some
      [ Var (Region p1 r) :&&: Var (Region p2 r)
      | r <- [0..rm-1] ]
      :->:
      Some
      [ Var $ Dist p1 p2 d
      | d <- [0..rs-1]
      ]

dist :: Point -> Point -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

neighbors :: Point -> Int -> Builder [Point]
neighbors (i,j) r = do
  (x,y,_) <- ask
  pure $ do
    dx <- [-r..r]
    let ady = r - abs dx
    dy <- [-ady..ady]
    let i' = i+dx
    guard $ i' >= 0
    guard $ i' < x
    let j' = j+dy
    guard $ j' >= 0
    guard $ j' < y
    pure (i',j')

setCount :: Int -> Int -> Int -> Builder ()
setCount i j c = do
  (x,y,_rs) <- ask
  let es =
        [ Edge (i,j) True | i < x-1 ] <>
        [ Edge (i,j) False | j < y-1 ] <>
        [ Edge (i-1,j) True | i > 0] <>
        [ Edge (i,j-1) False | j > 0]
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
  run 5 5 5
  [(0,0,2),(1,0,1)
  ,(2,1,2)
  ,(2,2,2)
  ,(1,3,2),(2,3,2)
  ,(1,4,3),(2,4,1)
  ]

