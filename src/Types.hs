module Types where

import qualified Graphics.UI.GLUT

--Input Keys
data Key = Key Graphics.UI.GLUT.Key deriving (Eq)

--Physics Types
data Point = Position Float Float | Velocity Float Float | Acceleration Float Float deriving (Show, Eq)

--Tank Types
data Turret = Turret {
    angle :: Float, 
    power :: Float  
} deriving (Show)

data Direction = FacingLeft | FacingRight  
           deriving (Enum , Show)

data TankState = TankState {
    direction :: Direction,
    position :: Point,
    velocity :: Point,
    inclineAngle :: Float,
    turret :: Turret
} deriving (Show)

data Tank = Tank {
    tankState :: TankState,
    score :: Integer,
    color :: Graphics.UI.GLUT.Color4 Float,
    currentWeapon :: Int,
    weaponCount :: [Integer]
} deriving (Show)


--GameState
data Tile = Tile {
    tilePosition :: Point,
    isObstacle :: Bool
} deriving (Show)

data GameState = GameState {
    tileMatrix :: [[Tile]],
    tankList :: [Tank],
    weapon :: [WeaponGraphics],
    chance :: Int,
    noOfPlayers :: Int,
    isAcceptingInput :: Bool
} deriving (Show)

--Weapon
data Weapon = GenericWeapon {
    currentPosition :: Point,
    currentVelocity :: Float,
    velocityMultiplyingFactor :: Float,
    currentAngle :: Float, 
    impactRadius :: Float,
    isLaunched :: Bool,
    hasImpacted :: Bool,
    launchDirection :: Direction
} deriving (Show)

data WeaponGraphics = WeaponGraphics {
    weaponPhysics :: Weapon,
    bulletColor :: Graphics.UI.GLUT.Color4 Float,
    turretColor :: Graphics.UI.GLUT.Color4 Float,
    bulletRotation :: Graphics.UI.GLUT.Vector3 Float,
    turretThickness :: Float,
    lengthOfTurret :: Float
} deriving (Show)


tileMatrixRowSize :: Int
tileMatrixRowSize = 201

tileMatrixColumnSize :: Int
tileMatrixColumnSize = 201

widthOfTile :: Float
widthOfTile = 0.01

heightOfTile :: Float
heightOfTile = 0.01

widthOfTank :: Integer
widthOfTank = 9

heightOfTank :: Integer
heightOfTank = 6

powerIncrement :: Float
powerIncrement = 1

angleIncrement :: Float
angleIncrement = 0.1

isIndexInRange :: [a] -> Int -> Bool
isIndexInRange list index  = if (index < 0 || index >= length list) then False else True


