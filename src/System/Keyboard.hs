module System.Keyboard ( typeString, click, press, release, getKeyState, waitUntil, Key(..) ) where


import System.Win32.Automation.Input ( sendInput, INPUT(Keyboard), KEYBDINPUT(KEYBDINPUT, dwExtraInfo, wVk, wScan, dwFlags, time) )
import Graphics.Win32                ( getAsyncKeyState )
import Control.Monad                 ( void )
import Data.List                     ( singleton )
import Data.Bits                     ( shift, (.&.) )


-- | Emulate keyboard inputs to type a unicode string
typeString :: String -> IO ()
typeString = mapM_ (click . KChar)

-- | Click (press and release) a key
click :: Key -> IO ()
click key = press key >> release key

-- | Press (and hold) a key
press :: Key -> IO ()
press = void . sendInput . singleton . keyToPressInput

-- | Release a pressed key
release :: Key -> IO ()
release = void . sendInput . singleton . keyToReleaseInput

-- | Block until a key is pressed
waitUntil :: Key -> IO ()
waitUntil key = do
  state <- getKeyState key
  if state == Down then
    return ()
  else waitUntil key

-- | Get the current async key state
getKeyState :: Key -> IO KeyState
getKeyState key = do
  state <- getAsyncKeyState (keyToCode key)
  case (state .&. 1, shift state (-15)) of
    (1, 1) -> return Down
    (0, 0) -> return Up
    (1, 0) -> return Down
    (0, 1) -> return Down
    _      -> return Up


-- internal


keyToPressInput :: Key -> INPUT
keyToPressInput key = case key of
  KChar         c -> unicodePress (fromEnum c)
  KNum          n -> rawPress     (0x5f + n)
  KFun          n -> rawPress     (0x6f + n)
  OtherUnicode  n -> unicodePress n
  other           -> rawPress $ keyToCode other

keyToReleaseInput :: Key -> INPUT
keyToReleaseInput key = case key of
  KChar         c -> unicodeRelease (fromEnum c)
  KNum          n -> rawRelease     (0x5f + n)
  KFun          n -> rawRelease     (0x6f + n)
  OtherUnicode  n -> unicodeRelease n
  other           -> rawPress $ keyToCode other


keyToCode :: Key -> Int
keyToCode key = case key of
  KChar c         -> fromEnum c
  KNum  n         -> 0x5f + n
  KFun  n         -> 0x6f + n
  OtherWin n      -> n
  OtherUnicode n  -> n
  ESC             -> 0x1B
  TAB             -> 0x09
  ENTER           -> 0x0D
  BACK            -> 0x08
  LWin            -> 0x5B
  RWin            -> 0x5C
  LShift          -> 0xA0
  RShift          -> 0xA1
  LCtrl           -> 0xA2
  RCtrl           -> 0xA3
  LAlt            -> 0xA4
  RAlt            -> 0xA5
  KApp            -> 0x5D
  NumL            -> 0x90
  CapL            -> 0x14
  ScrL            -> 0x91


unicodePress :: Int -> INPUT
unicodePress n = Keyboard (KEYBDINPUT {wVk = 0, wScan = toEnum n, dwFlags = 0x0004, time = 0x0, dwExtraInfo = 0x0})

unicodeRelease :: Int -> INPUT
unicodeRelease n = Keyboard (KEYBDINPUT {wVk = 0, wScan = toEnum n, dwFlags = 0x0006, time = 0x0, dwExtraInfo = 0x0})


rawPress :: Int -> INPUT
rawPress n = Keyboard (KEYBDINPUT {wVk = toEnum n, wScan = 0x0, dwFlags = 0, time = 0x0, dwExtraInfo = 0x0})

rawRelease :: Int -> INPUT
rawRelease n = Keyboard (KEYBDINPUT {wVk = toEnum n, wScan = 0x0, dwFlags = 0x0002, time = 0x0, dwExtraInfo = 0x0})


-- https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
data Key
  = KChar Char  -- [0-9, a-z == 0x30-0x39, 0x41-0x5A]      spacebar == 0x20
  | ESC         -- 0x1B
  | TAB         -- 0x09
  | ENTER       -- 0x0D
  | BACK        -- 0x08
  | LWin        -- 0x5B
  | RWin        -- 0x5C
  | LShift      -- 0xA0
  | RShift      -- 0xA1
  | LCtrl       -- 0xA2
  | RCtrl       -- 0xA3
  | LAlt        -- 0xA4
  | RAlt        -- 0xA5
  | KApp        -- 0x5D
  | KNum Int    -- Numpad 0-9 == 0x60-0x69
  | KFun Int    -- F 1-24 == 0x70-0x87
  | NumL        -- 0x90
  | CapL        -- 0x14
  | ScrL        -- 0x91
  | OtherWin Int      -- 0x01-0xFE
  | OtherUnicode Int
  deriving ( Show, Read, Eq )

data KeyState
  = Down
  | Up
  deriving ( Show, Read, Eq )
