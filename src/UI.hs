module UI where

import qualified Data.Map.Lazy as M
import Builder (Var (..))
import qualified Data.Text as T

parse :: Text -> [(Int,Int,Int)]
parse w = do
  (y,l) <- zip [0..] (lines w)
  (x,c) <- zip [0..] (toString l)
  guard $ '0' <= c && c <= '9'
  pure (x,y,ord c - ord '0')

render :: [(Int,Int,Int)] -> Int -> Int -> Map Var Bool -> Text
render cs x y sol = let
  end = T.replicate x "*-" <> "*\n"
  csm = M.fromList [ ((i,j),c) | (i,j,c) <- cs ]
  con i j = fromMaybe " " (M.lookup (i,j) csm <&> show)
  vert j = "|" <> con 0 j <>  mconcat
    [ (if M.lookup (Edge (i,j) True) sol == Just True then "|" else " ") <> con (i+1) j
    | i <- [0..x-2]] <> "|\n"
  horiz j = "*" <>  mconcat
    [ (if M.lookup (Edge (i,j) False) sol == Just True then "-" else " ") <> "*"
    | i <- [0..x-1]] <> "\n"
    in  end <>
      mconcat [ vert j <> horiz j | j <- [0..y-2] ]
      <> vert (y-1) <> end

