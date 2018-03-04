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
width = 1200
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
    pos :: (Int, Int),
    vel :: (Int, Int),
    mass :: Int,
    radius :: Float,
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
 = translate x' y' $ G2.color (col p) $ circleSolid (radius p)
  where
    (x, y) = pos p
    (x', y') = (fromIntegral x, fromIntegral y)
   

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
  [obj "Sun"     Star   0   50 yellow,
   obj "Mercury" Planet 80  10 yellow,
   obj "Venus"   Planet 130 20 orange,
   obj "Earth"   Planet 180 25 blue,
   obj "Mars"    Planet 220 20 red,
   obj "Jupiter" Planet 300 40 orange,
   obj "Saturn"  Planet 360 40 blue,
   obj "Uranus"  Planet 400 30 (light blue),
   obj "Neptune" Planet 430 25 blue,
   obj "Pluto"   Planet 470 10 blue]

  where obj n t x r c = Object { name = n, objType = t, pos = (x, 0), vel = (0, 0), mass = 1, radius = r, col = c }

initGame = do 
  let initialState = Game { paused = False, objects = initialObjects }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
