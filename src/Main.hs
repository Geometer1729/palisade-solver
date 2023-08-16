{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import Builder (run)
import Data.Map.Lazy qualified as M
import Main.Utf8 qualified as Utf8
import UI (parse, render)

main :: IO ()
main = Utf8.withUtf8 $ do
  [name] <- getArgs
  readFileText name <&> parse >>= \case
    Just (x, y, r, cs) ->
      case run x y r cs of
        Just t -> do
          -- handy for debug
          when False $ mapM_ (print . fst) $ filter snd $ M.toList t
          putTextLn $ render cs x y t
        Nothing -> putTextLn "no solution"
    Nothing -> putTextLn "failed to parse"
