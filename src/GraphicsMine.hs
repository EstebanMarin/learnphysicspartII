module GraphicsMine
  ( displayMode,
    xAxis,
    yAxis,
    axes,
    -- printGraph,
  )
where

import Graphics.Gloss
  ( Display (InWindow),
    Picture (Color, Line, Pictures),
    green,
    red,
  )

displayMode :: Display
displayMode = InWindow "Axes" (1000, 700) (10, 10)

xAxis :: Picture
xAxis = Color red $ Line [(0, 0), (100, 0)]

yAxis :: Picture
yAxis = Color green $ Line [(0, 0), (0, 100)]

axes :: Picture
axes = Pictures [xAxis, yAxis]
