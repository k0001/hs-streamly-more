{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------

expAct :: (Eq a, Show a) => a -> a -> IO ()
expAct e a | e == a    = pure ()
           | otherwise = fail $ "Expected " <> show e <> ", got " <> show a


main :: IO ()
main = pure ()
