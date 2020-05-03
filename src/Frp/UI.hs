module Frp.UI where

import           Control.Monad                 (forM_)
import           Data.Function                 ((&))
import           Data.Functor                  (void, ($>))
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Frp.Game

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Canvas (Canvas)
import qualified Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Core   (Event, UI, Window, askWindow,
                                                defaultConfig, getBody, liftIO,
                                                mkElement, register, runUI, set,
                                                startGUI, title, ( #+ ), ( #. ))
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  return w & set title "A Reactive game of life (clone)"
  canvas <- mkElement "canvas" #. "reactive-life-clone"
  timer <- UI.timer & set UI.interval 100
  let updates = UI.tick timer $> step
  let init = fromList 100
        [
          (12,10), (17,10),
          (10,11), (11,11), (13,11), (14,11), (15,11), (16,11),(18,11),(19,11),
          (12,12), (17,12) ]
  grids <- UI.accumE init updates
  registerUpdate canvas grids
  timer & UI.start
  getBody w #+ [return canvas]
--  let grids = take 10 $ iterate step init
--  forM_ grids (\ g -> putStrLn (toString g) *> putStrLn "")

pt :: (Int, Int) -> Canvas.Point
pt (x, y) = (fromIntegral x, fromIntegral y)
drawGrid :: Grid -> Canvas -> UI ()
drawGrid grid canvas = do
  let width = V.length grid
  let scale = 10
  let cell = fromIntegral scale
  Canvas.clearCanvas canvas
  return canvas & set Canvas.fillStyle (Canvas.htmlColor "black")
                & set UI.width (width * scale)
                & set UI.height (width * scale)
  forM_ (livePoints grid) $ \ (x, y) -> Canvas.fillRect (pt (x * scale, y * scale)) cell cell canvas

registerUpdate :: Canvas -> Event Grid -> UI ()
registerUpdate canvas grids = do
  window <- askWindow
  liftIO . void . register grids $ \grid -> runUI window $ drawGrid grid canvas

