module Input where

import qualified Graphics.UI.GLUT
import Types

moveLeft :: Key
moveLeft = Key $ Graphics.UI.GLUT.SpecialKey Graphics.UI.GLUT.KeyLeft

moveRight :: Key
moveRight = Key $ Graphics.UI.GLUT.SpecialKey Graphics.UI.GLUT.KeyRight

launch :: Key
launch = Key $ Graphics.UI.GLUT.Char ' '

increasePower :: Key
increasePower = Key $ Graphics.UI.GLUT.Char '+'

decreasePower :: Key
decreasePower = Key $ Graphics.UI.GLUT.Char '-'

increaseAngle :: Key 
increaseAngle  = Key $ Graphics.UI.GLUT.Char 'D'

decreaseAngle :: Key 
decreaseAngle = Key $ Graphics.UI.GLUT.Char 'A'
