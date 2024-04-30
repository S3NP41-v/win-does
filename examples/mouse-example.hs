import System.Mouse
import Control.Concurrent ( threadDelay )


main :: IO ()
main = do
  -- giving the absolute coordinate functions the screen dimensions
  initMouse

  -- moving the cursor around by absolute
  moveCursorTo (0, 0)
  delay
  moveCursorTo (500, 500)
  
  -- moving the cursor around by relative
  delay
  moveCursorBy (-100, 0)
  delay
  moveCursorBy (200, 0)

  -- buttons
  delay
  clickMouse LeftButton
  delay
  clickMouse RightButton
  delay
  clickMouse MiddleButton
  delay
  pressMouse LeftButton
  delay
  releaseMouse LeftButton

  -- scrolling
  delay
  scroll 100
  delay
  scroll (-100)

  -- dragging
  dragMouse LeftButton (500, 0) (500, 500)

  -- cursor position
  pos <- getMousePos
  putStrLn $ "mouse at " <> show pos


delay :: IO ()
delay = threadDelay (10^(6 :: Int))
