module Turbo where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map

import           TurboDef


-- "Run" a Turbo program to produce SVG path commands.
-- This is the only function that will be tested.
runTurbo :: Stmt -> [SVGPathCmd]
runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
-- But the actual execution engine and recursion are in "turbo" and "evalReal"
-- below.


-- Evaluate an expression. The State monad is needed to look up variables.
evalReal :: RealExpr -> State TurboMem Double
evalReal (RLit num) = return num
evalReal (RVar str) = do
    (val) <- (getVar str)
    return val
evalReal (Neg r1) = do
     val <- evalReal(r1)
     return (-val)
evalReal (r1 :+ r2) = do
    (val) <- evalReal(r1)
    (val2) <- evalReal(r2)
    return (val + val2)
evalReal (r1 :- r2) = do
    (val) <- evalReal(r1)
    (val2) <- evalReal(r2)
    return (val - val2)
evalReal (r1 :* r2) = do
    (val) <- evalReal(r1)
    (val2) <- evalReal(r2)
    return (val * val2)
evalReal (r1 :/ r2) = do
    (val) <- evalReal(r1)
    (val2) <- evalReal(r2)
    return (val / val2)

-- Run a Turbo statement. Use the State monad to keep state. Return SVG path
-- commands.
turbo :: Stmt -> State TurboMem [SVGPathCmd]
turbo (str := expr) = error "TODO"
turbo PenDown = do
    setPen True
    return [(MoveTo 0 0)]
turbo PenUp = do
    setPen False
    return [(MoveTo 0 0)]
turbo (Turn expr) = do
    angle <- evalReal expr
    turn angle
    return []
turbo (Forward expr) = do
    pen <- getPen
    case pen of
        True -> [LineTo (evalReal expr) 0]
turbo (Seq (x:xs)) = error "TODO"
turbo (For str from to list) = error "TODO"





-- Turbo state:
-- * dictionary of variables->values
-- * current direction (degrees away from x-axis, counterclockwise, e.g.,
--   0 points east, 90 points north)
-- * pen state (True means touching paper)
data TurboMem = TurboMem (Map String Double) Double Bool
    deriving (Eq, Show)

-- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
initTurboMem = TurboMem Map.empty 0 False

-- If you could code up the following helpers, your "turbo" implementation could
-- be pretty clean and easy to follow.  fmap, get, modify, Map.lookup, and
-- Map.insert will get you a long way.

-- Get current direction.
getAngle :: State TurboMem Double
getAngle = do 
    (TurboMem dict direction pen) <- get
    return direction

-- Change direction by adding the given angle.
turn :: Double -> State TurboMem ()
turn angle = do 
    (TurboMem dict direction pen) <- get
    put (TurboMem dict angle pen)

-- Get pen state.
getPen :: State TurboMem Bool
getPen = do
    (TurboMem dict direction pen) <- get
    return pen

-- Set pen state.
setPen :: Bool -> State TurboMem ()
setPen state = do
    (TurboMem dict direction pen) <- get
    put (TurboMem dict direction state)

-- Get a variable's current value.
getVar :: String -> State TurboMem Double
getVar var = do
    (TurboMem dict direction pen) <- get
    return (dict Map.! var)
    
-- Set a variable to value.
setVar :: String -> Double -> State TurboMem ()
setVar key value = do 
    (TurboMem dict direction pen) <- get
    put (TurboMem (Map.insert key value dict) direction pen)

