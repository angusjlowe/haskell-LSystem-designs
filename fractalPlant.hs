--data Command = Go Distance
--             | Turn Angle
--             | Sit
--             | Command :#: Command
--             | Branch Command
--             | GrabPen Pen
--               deriving (Eq, Ord, Show)

import TheLSystem
import Test.QuickCheck

fractalPlant :: Int -> Command
fractalPlant x = f x
               where
                 f 0 = Sit
                 f x = g (x-1) :#: n :#: Branch (Branch (f (x-1)) :#: p :#: f (x-1)) :#: p :#: g (x-1) :#: Branch (z :#: g (x-1) :#: f (x-1)) :#: n :#: f (x-1)
                 g 0 = GrabPen blue :#: Go 3 :#: GrabPen white :#: Go 3 :#: GrabPen green :#: Go 2
                 g x = g (x-1) :#: g (x-1)
                 p = Turn 60
                 n = Turn (-60)
                 z = Turn 60



main :: IO ()
main = display (fractalPlant 8)
