{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Db
import Lib


main :: IO ()
main = do
  ensureDb
  startApp
