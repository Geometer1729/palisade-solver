module Render where
import qualified Data.Map.Lazy as M
import Builder (Var (..))
import qualified Data.Text as T
import Data.Maybe (fromJust)

render :: Int -> Int -> Int -> Map Var Bool -> Text
render x y rs sol = let
  rm = (x*y) `div` rs
  in unlines
  [ T.concat [
  show $ fromJust
    $ find
      (\r -> M.lookup (Region i j r) sol == Just True)
      [0..rm-1]
  | i <- [0..x-1] ]
  | j <- [0..y-1] ]

