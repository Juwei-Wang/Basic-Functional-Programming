data Point = Point Float Float deriving (show)
data Point = Circle Point Float | Rectangle Point Point deriving (show) 

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
