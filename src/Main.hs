{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Main.Utf8 qualified as Utf8
import Builder (run)
import UI (render,parse)
import qualified Data.Map.Lazy as M

main :: IO ()
main = Utf8.withUtf8 $ do
  c <- parse <$> readFileText "input"
  case run 5 5 5 c of
    Just t -> do
      -- handy for debug
      when False $ mapM_ (print . fst) $ filter snd $ M.toList t
      putTextLn $ render c 5 5 t
    Nothing -> putTextLn "no solution"


