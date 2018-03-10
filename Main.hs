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
    temperature :: Float,
    numMoons :: Int,
    orbitTime :: Float,
    rotationTime :: Float,
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
    (x, y) = ((distFromSun p)*30 - 500, 0)
    radius = 4
   

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
  [obj "Sun"     Star   0     1391400 5778 0 yellow,
   obj "Mercury" Planet 0.39  4878    452  0 yellow,
   obj "Venus"   Planet 0.723 12104   726  0 orange,
   obj "Earth"   Planet 1     12756   280  1 blue,
   obj "Mars"    Planet 1.524 6787    250  2 red,
   obj "Jupiter" Planet 5.203 142796  120  67 orange,
   obj "Saturn"  Planet 9.539 120660  88   62 blue,
   obj "Uranus"  Planet 19.18 51118   59   27 (light blue),
   obj "Neptune" Planet 30.06 48600   48   13 blue,
   obj "Pluto"   Planet 39.53 2274    37   4 blue]

  where obj n t dist d tmp nm c = Object { name = n, objType = t, mass = 1, distFromSun = dist, diameter = d, temperature = tmp, numMoons = nm, orbitTime = 0, rotationTime = 0, col = c }

initGame = do 
  let initialState = Game { paused = False, objects = initialObjects }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
