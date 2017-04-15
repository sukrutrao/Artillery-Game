module Input where

import qualified Graphics.UI.GLUT
import Types

-- | To represent the key that makes the tank move left
moveLeft :: Key
moveLeft = Key $ Graphics.UI.GLUT.SpecialKey Graphics.UI.GLUT.KeyLeft

-- | To represent the key that makes the tank move right
moveRight :: Key
moveRight = Key $ Graphics.UI.GLUT.SpecialKey Graphics.UI.GLUT.KeyRight

-- | To represent the key that is responsible for launching a weapon
launch :: Key
launch = Key $ Graphics.UI.GLUT.Char ' '

-- | To represent the key that increases the turret power
increasePower :: Key
increasePower = Key $ Graphics.UI.GLUT.Char '+'

-- | To represent the key that decreases the turret power
decreasePower :: Key
decreasePower = Key $ Graphics.UI.GLUT.Char '-'

-- | To represent the key that increases the turret angle
increaseAngle :: Key 
increaseAngle  = Key $ Graphics.UI.GLUT.Char 'D'

-- | To represent the key that decreases the turret angle
decreaseAngle :: Key 
decreaseAngle = Key $ Graphics.UI.GLUT.Char 'A'

-- | To represent the key to be pressed for weapon of type 0
weapon0 :: Key 
weapon0 = Key $ Graphics.UI.GLUT.Char '0'

-- | To represent the key to be pressed for weapon of type 1
weapon1 :: Key 
weapon1 = Key $ Graphics.UI.GLUT.Char '1'

-- | To represent the key to be pressed for weapon of type 2
weapon2 :: Key 
weapon2 = Key $ Graphics.UI.GLUT.Char '2'