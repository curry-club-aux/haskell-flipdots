module Main where

import Graphics.Flipdots
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)

main :: IO ()
main = forM_ (cycle imgs) $ \img -> do
  sendImage img flipboardConfig
  threadDelay delay
  where
    fps = 128/60
    delay = round (1000000 / fps)
    dims = Dims { width = 80, height = 16 }
    flipboardConfig = FlipboardConfig "flipdot.openlab.lan" 2323 dims
    imgs =
      [ \x y -> (x `div` s) `mod` 2 == 0 && (y `div` s) `mod` 2 == 0
             | s <- [1..16]
      ]
