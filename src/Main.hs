module Main where

import Main.Utf8 qualified as Utf8
import Builder (test)
import Render (render)
import qualified Data.Map.Lazy as M

main :: IO ()
main = Utf8.withUtf8 $ do
  case test of
    Just t -> do
      mapM_ (print . fst) $ filter snd $ M.toList t
      putTextLn $ render 5 5 5 t
    Nothing -> putTextLn "no solution"


