module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as G2
import System.IO  
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 20
width = 1400
height = 500 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100

window = InWindow "Solar" (width, height) (offset, offset)
background = black

data Type = Star | Planet | Moon deriving (Show)

data LifeGame = Game
  { 
    objects :: [Object],
    paused :: Bool
  } deriving Show 

data Object = Object
  {
    name :: String,
    objType :: Type,
    mass :: Int,
    diameter :: Float,
    distFromSun :: Float,
    col :: G2.Color
  } deriving Show

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderObjects g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Dashboard"

renderObjects g = pictures $ map renderObject (objects g)

renderObject :: Object -> Picture
renderObject p
 = translate x y $ G2.color (col p) $ circleSolid radius
  where
    (x, y) = ((distFromSun p)*20, 0)
    radius = (diameter p) / 2
   

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys _ game = game

togglePaused g = g { paused   = not (paused g) }

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game) = game
 | otherwise     = updateGame game

updateGame g = g

initialObjects = 
  [obj "Sun"     Star   0    1391400 yellow,
   obj "Mercury" Planet 0.39 4878 yellow,
   obj "Venus"   Planet 0.723 12104 orange,
   obj "Earth"   Planet 1 12756 blue,
   obj "Mars"    Planet 1.524 6787 red,
   obj "Jupiter" Planet 5.203 142796 orange,
   obj "Saturn"  Planet 9.539 120660 blue,
   obj "Uranus"  Planet 19.18 51118 (light blue),
   obj "Neptune" Planet 30.06 48600 blue,
   obj "Pluto"   Planet 39.53 2274 blue]

  where obj n t dist d c = Object { name = n, objType = t, mass = 1, distFromSun = dist, diameter = 5, col = c }

initGame = do 
  let initialState = Game { paused = False, objects = initialObjects }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
