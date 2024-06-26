module Main (main) where

import Control.Concurrent (forkOS)
import Graphics.Gloss
  ( Display (InWindow),
    Picture (Color, Line, Pictures),
    black,
    display,
    green,
    red,
  )
import Graphics.Gnuplot.Simple (Attribute (..), plotPath)
import Lib (iHat, kHat, projectilePos, xComp, zComp, (*^), (^+^))

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

xAxis :: Picture
xAxis = Color red $ Line [(0, 0), (100, 0)]

yAxis :: Picture
yAxis = Color green $ Line [(0, 0), (0, 100)]

axes :: Picture
axes = Pictures [xAxis, yAxis]

printGraph :: IO ()
printGraph = display displayMode black axes

main :: IO ()
main = do
  printGraph
  putStrLn "Press enter to exit"
  _ <- getLine
  return ()

-- main =
--   let posInitial = 10 *^ kHat
--       velInitial = 20 *^ cos (pi / 6) *^ iHat ^+^ 20 *^ sin (pi / 6) *^ kHat
--       posFunc = projectilePos posInitial velInitial
--       pairs = [(xComp r, zComp r) | t <- [0, 0.01 ..], let r = posFunc t]
--       plotingPairs = takeWhile (\(_, z) -> z >= 0) pairs
--    in plotPath
--         [Title "Projectile Motion", XLabel "Horizontal Motion (m)", YLabel "Vertical Motion (m)", PNG "trajectory.png", Key Nothing]
--         plotingPairs
