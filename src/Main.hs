module Main where

import Main.Utf8 qualified as Utf8
import Builder (test)
import Render (render)
import Data.Maybe (fromJust)

main :: IO ()
main = Utf8.withUtf8 $ do
  putTextLn $ render 5 5 5 (fromJust test)


