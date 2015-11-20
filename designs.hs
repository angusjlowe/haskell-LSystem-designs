--data Command = Go Distance
--             | Turn Angle
--             | Sit
--             | Command :#: Command
--             | Branch Command
--             | GrabPen Pen
--               deriving (Eq, Ord, Show)

import theLSystem
import Test.QuickCheck

join :: [Command] -> Command
join [] = Sit
join (x:xs) = x :#: (join xs)

fractalPlant :: Int -> Command
fractalPlant x = f x
               where
                 f 0 = Sit
                 f x = g (x-1) :#: n :#: Branch (Branch (f (x-1)) :#: p :#: f (x-1)) :#: p :#: g (x-1) :#: Branch (z :#: g (x-1) :#: f (x-1)) :#: n :#: f (x-1)
                 g 0 = GrabPen snowblue :#: Go 3 :#: GrabPen white :#: Go 3 :#: GrabPen green :#: Go 2
                 g x = g (x-1) :#: g (x-1)
                 p = Turn 60
                 n = Turn (-60)
                 z = Turn 60

tree :: Int -> Command
tree x  =  f x
  where
  f 0      = GrabPen white :#: Go 10
  f x  = g x :#: Branch (n :#: f (x-1))
                 :#: Branch (p :#: f (x-1))
                 :#: Branch (g (x-1) :#: f (x-1))
  g 0      = GrabPen blue :#: Go 10
  g x  = g (x-1) :#: g (x-1)
  n        = Turn 45
  p        = Turn (-45)


snowflake :: Int -> Command
snowflake x = l x :#: f x :#: n :#: n :#: f x :#: n :#:
               n :#: f x :#: n :#: n
              where
                f 0 = Go 1
                f x = f (x-1) :#: p :#: f (x-1) :#:
                      n :#: n :#:f (x-1) :#: p :#: f (x-1)
                h 0 = GrabPen red :#: Go 30
                h x = g (x-1) :#: p :#: h (x-1) :#: p :#: g (x-1)
                l 0 = h 6
                l x = Sit
                g 0 = GrabPen blue :#: Go 30
                g x = f (x-1) :#: n :#: g (x-1) :#: n :#: h (x-1)
                p = GrabPen blue :#: Turn 60
                n = GrabPen white :#: Turn (-60)

main :: IO ()
main = display (fractalPlant 8)
