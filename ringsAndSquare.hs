--data Command = Go Distance
--             | Turn Angle
--             | Sit
--             | Command :#: Command
--             | Branch Command
--             | GrabPen Pen
--               deriving (Eq, Ord, Show)

import TheLSystem
import Test.QuickCheck

--Start: F+F+F+F
--Rules: F=FF+F+F+F+F+F-F
--Order: 4, Step Length: 5, Left Angle: 90, Right Angle: 90

rings :: Int -> Command
rings x = f x :#: p :#: f x :#: p :#: f x :#: p :#: f x
        where
          f 0 = Go 5
          f x = GrabPen snowblue :#: f (x-1) :#: GrabPen blue :#: f (x-1) :#: p :#: f (x-1) :#: p :#: GrabPen red :#: f (x-1) :#: p :#: GrabPen yellow :#: f (x-1) :#: p :#: GrabPen blue :#: f (x-1) :#: p :#: GrabPen white :#: f (x-1) :#: n :#: GrabPen snowblue :#: f (x-1)
          p = Turn 90
          n = Turn (-90)

--Start: X–F–X–F
--Rules: X=+Y-F-Y+;Y=-X+F+X-;F=F
--Order: 10, Step Length: 5, Left Angle: 45, Right Angle: 45

square :: Int -> Command
square x = z x :#: n :#: n :#: f x :#: n :#: n :#: z x :#: n :#: n :#: f x
         where
           z 0 = GrabPen snowblue :#: Go 5
           z x = p :#: y (x-1) :#: n :#: f (x-1) :#: n :#: y (x-1) :#: p
           y 0 = GrabPen blue :#: Go 5
           y x = n :#: z (x-1) :#: p :#: f (x-1) :#: p :#: z (x-1) :#: n
           f 0 = GrabPen red :#: Go 5
           f x = f (x-1)
           p = Turn 45
           n = Turn (-45)

main :: IO ()
main = display (square 12)
