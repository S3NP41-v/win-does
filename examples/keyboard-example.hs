import System.Keyboard
import Control.Concurrent ( threadDelay )


main :: IO ()
main = do
  -- typing a UNICODE string (can also include escapes)
  typeString "Hello World!\nHow are you?"

  -- singular key's
  delay
  click (KChar 'O')
  delay
  click (KChar 'K')
  delay
  click LWin
  delay
  click ScrL
  delay
  click ScrL

  -- pressing
  delay
  press (KChar 'A')
  delay
  release (KChar 'A')

  -- listening
  waitUntil ESC

  putStrLn "Done!"



delay :: IO ()
delay = threadDelay (5^(6 :: Int))