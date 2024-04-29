module System.Mouse ( initMouse, dragMouse, moveCursorTo, moveCursorBy, clickMouse, pressMouse, releaseMouse, scroll, MouseButton(..) ) where

import System.Win32.Automation.Input
import qualified System.Win32.Automation.Input.Mouse as M

import System.Win32.Info.Computer ( getSystemMetrics )
import System.Win32               ( sM_CXSCREEN, sM_CYSCREEN )
import Control.Concurrent.MVar    ( putMVar, readMVar, MVar )
import Control.Monad              ( void )


-- | Initialise the absolute coordinate functions (give them the screen dimensions)
initMouse :: IO ()
initMouse = do
  x <- getSystemMetrics sM_CXSCREEN
  y <- getSystemMetrics sM_CYSCREEN

  putMVar screenSizeMVar (x, y)

-- | Drag the cursor from one position to the other while pressing the specified button
--   Important: initialise first! (initMouse)
dragMouse :: MouseButton -> (Int, Int) -> (Int, Int) -> IO ()
dragMouse mouseButton start destination = do
  moveCursorTo start
  pressMouse mouseButton
  moveCursorTo destination
  releaseMouse mouseButton


-- | Move cursor to absolute position
--   Important: initialise first! (initMouse)
moveCursorTo :: (Int, Int) -> IO ()
moveCursorTo (x, y) = do
  (sX, sY) <- readMVar screenSizeMVar

  -- normalised vectors
  let nX = floor (fromIntegral x / fromIntegral sX :: Float) * 65535
  let nY = floor (fromIntegral y / fromIntegral sY :: Float) * 65535

  void $ sendInput [Mouse (M.MOUSEINPUT {M.dx = toEnum nX, M.dy = toEnum nY, M.mouseData = 0, M.dwFlags = 0x8001, M.time = 0, M.dwExtraInfo = 0})]
  
-- | Move cursor by relative amount from its current position
moveCursorBy :: (Int, Int) -> IO ()
moveCursorBy (x, y) = void $ sendInput [Mouse (M.MOUSEINPUT {M.dx = toEnum x, M.dy = toEnum y, M.mouseData = 0, M.dwFlags = 0x0001, M.time = 0, M.dwExtraInfo = 0})]

-- | Press and release a mouse button
clickMouse :: MouseButton -> IO ()
clickMouse button = void $ sendInput [ Mouse (M.MOUSEINPUT {M.dx = 0, M.dy = 0, M.mouseData = 0, M.dwFlags = toEnum $ buttonDownToCode button, M.time = 0, M.dwExtraInfo = 0})
                                     , Mouse (M.MOUSEINPUT {M.dx = 0, M.dy = 0, M.mouseData = 0, M.dwFlags = toEnum $ buttonUpToCode button, M.time = 0, M.dwExtraInfo = 0})
                                     ]

-- | Press and hold a mouse button
pressMouse :: MouseButton -> IO ()
pressMouse button = void $ sendInput [Mouse (M.MOUSEINPUT {M.dx = 0, M.dy = 0, M.mouseData = 0, M.dwFlags = toEnum $ buttonDownToCode button, M.time = 0, M.dwExtraInfo = 0})]

-- | Release a pressed mouse button
releaseMouse :: MouseButton -> IO ()
releaseMouse button = void $ sendInput [Mouse (M.MOUSEINPUT {M.dx = 0, M.dy = 0, M.mouseData = 0, M.dwFlags = toEnum $ buttonUpToCode button, M.time = 0, M.dwExtraInfo = 0})]

-- | Scroll with the mouse wheel, positive means scroll up, negative scroll down
scroll :: Int -> IO ()
scroll by = void $ sendInput [Mouse (M.MOUSEINPUT {M.dx = 0, M.dy = 0, M.mouseData = toEnum by, M.dwFlags = 0x0800, M.time = 0, M.dwExtraInfo = 0})]


-- internal

screenSizeMVar :: MVar (Int, Int)
screenSizeMVar = undefined

buttonDownToCode :: MouseButton -> Int
buttonDownToCode LeftButton   = 0x0002
buttonDownToCode RightButton  = 0x0008
buttonDownToCode MiddleButton = 0x0020

buttonUpToCode :: MouseButton -> Int
buttonUpToCode LeftButton   = 0x0004
buttonUpToCode RightButton  = 0x0010
buttonUpToCode MiddleButton = 0x0040


data MouseButton
  = LeftButton
  | RightButton
  | MiddleButton
  deriving ( Show, Read, Eq )
