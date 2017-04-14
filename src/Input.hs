module Input where

import qualified Graphics.UI.GLUT
import Types
import Graphics.Gloss.Interface.Pure.Game

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

weapon0 :: Key 
weapon0 = Key $ Graphics.UI.GLUT.Char '0'

weapon1 :: Key 
weapon1 = Key $ Graphics.UI.GLUT.Char '1'

weapon2 :: Key 
weapon2 = Key $ Graphics.UI.GLUT.Char '2'
