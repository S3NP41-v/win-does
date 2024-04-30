# win-does


A haskell library for controlling the keyboard and mouse in windows


### Examples [[Keyboard Control](./examples/keyboard-example.hs)] [[Mouse Control](./examples/mouse-example.hs)]

```haskell
import System.Keyboard
import Control.Concurrent ( threadDelay )

main :: IO ()
main = do
  click LWin
  delay
  typeString "notepad"
  delay
  click ENTER

  threadDelay 1000000
  typeString "Hello World!"


delay :: IO ()
delay = threadDelay 500000
```

### Plans
- [ ] Ability to insert special keys into a string to be typed out
- [ ] Control over the speed at which keys are pressed in `typeString`
- [x] Key press listening events
- [ ] Setting hotkeys
- [ ] Control over the speed of mouse cursor
- [x] Get Mouse Position

### Contributing
I don't have that much time on hands, but if you have a proposition, issue, or a question,
use the appropriate github function and i will try to look into it as fast as i can!
