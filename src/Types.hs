module Types where

import qualified Graphics.UI.GLUT

-- | Type to represent different keys on the keyboard
data Key = Key Graphics.UI.GLUT.Key deriving (Eq)

-- | Type to represent a point in two dimensions, where each constructor
--   specifies what the point is for
data Point = Position Float Float | Velocity Float Float | Acceleration Float Float deriving (Show, Eq)

-- | Type for representing the properties of the turrret of a tank
data Turret = Turret {
    angle :: Float, -- ^ The angle of the turret with respect to the tank 
    power :: Float  -- ^ The power chosen by the user for launching
} deriving (Show)

-- | Type to specify the direction, left or right. Used by
--   weapons and tanks
data Direction = FacingLeft | FacingRight  
           deriving (Enum , Show)

-- | Type to represent the state variables of a tank
data TankState = TankState {
    direction :: Direction, -- ^ The direction the tank is facing
    position :: Point, -- ^ The position of the bottom left corner of the tank
    velocity :: Point, -- ^ The current velocity of the tank
    inclineAngle :: Float, -- ^ The angle of inclination of the tank to the horizontal
    turret :: Turret -- ^ The state properties of the turret of the tank
} deriving (Show)

-- | Type to represent a tank in the game
data Tank = Tank {
    tankState :: TankState, -- ^ The state of the tank
    score :: Float, -- ^ The score of the player using the tank, i.e., the health of the tank
    color :: Graphics.UI.GLUT.Color4 Float, -- ^ The colour of the tank, which depends on the player and score
    currentWeapon :: Int, -- ^ The weapon being currently used
    weaponCount :: [Integer] -- ^ The number of weapons of the current type available
} deriving (Show)


-- | Type to represent a tile in the tile map of the game
data Tile = Tile {
    tilePosition :: Point, -- ^ The position of the bottom left corner of the tile on the screen
    isObstacle :: Bool -- ^ Whether the position of the tile is an obstacle or not
} deriving (Show)

-- | Type to represent the state of the game
data GameState = GameState {
    tileMatrix :: [[Tile]], -- ^ The tile matrix on the screen
    tankList :: [Tank], -- ^ The list of tanks in the game
    weapon :: [WeaponGraphics], -- ^ The list of weapons available
    chance :: Int, -- ^ The player whose turn it currently is
    noOfPlayers :: Int, -- ^ The number of players in the game
    isAcceptingInput :: Bool -- ^ Whether the game is still accepting input, i.e., not over yet
} deriving (Show)

-- | Type to represent the weapons in the game
data Weapon = GenericWeapon {
    currentPosition :: Point, -- ^ The current position of the weapon
    currentVelocity :: Float, -- ^ The current velocity of the weapon
    launchVelocity :: Float, -- ^ The velocity at which the weapon was launched
    launchAngle :: Float, -- ^ The angle at which the weapon was launched
    velocityMultiplyingFactor :: Float, -- ^ The multiplying factor to determine the start velocity
    currentAngle :: Float, -- ^ The current angle of the weapon to the horizontal
    impactRadius :: Float, -- ^ The impact radius of the weapon
    isLaunched :: Bool, -- ^ Whether the weapon is currently mid air
    hasImpacted :: Bool, -- ^ Whether the weapon has impacted yet or not
    launchDirection :: Direction -- ^ The direction of launch of the weapon
} deriving (Show)

-- | Type to represent the graphics properties of a weapon
data WeaponGraphics = WeaponGraphics {
    weaponPhysics :: Weapon, -- ^ The state of the weapon
    bulletColor :: Graphics.UI.GLUT.Color4 Float, -- ^ The colour of the weapon
    turretColor :: Graphics.UI.GLUT.Color4 Float, -- ^ The colour of the turret when this weapon is launched
    bulletRotation :: Graphics.UI.GLUT.Vector3 Float, -- ^ Bullets Current Rotation Angle For Display 
    turretThickness :: Float, -- ^ The thickness of the turret when this weapon is launched
    lengthOfTurret :: Float -- ^ The length of the turret when this weapon is launched
} deriving (Show)

-- | Number of rows in the tile matrix
tileMatrixRowSize :: Int
tileMatrixRowSize = 201

-- | Number of columns in the tile matrix
tileMatrixColumnSize :: Int
tileMatrixColumnSize = 401

-- | Width of a tile in the tile matrix
widthOfTile :: Float
widthOfTile = 0.01

-- | Height of a tile in the tile matrix
heightOfTile :: Float
heightOfTile = 0.01

-- | Width of the tank in terms of the number of tiles
widthOfTank :: Integer
widthOfTank = 9

-- | Height of the tank in terms of the number of tiles
heightOfTank :: Integer
heightOfTank = 6

-- | The amount by which the power changes per key press
powerIncrement :: Float
powerIncrement = 1

-- | The amount by which the angle, in radians, changes per key press
angleIncrement :: Float
angleIncrement = 0.1

-- | Function to check if the current index is in the range of the tile map or not
isIndexInRange :: [a] -> Int -> Bool
isIndexInRange list index  = if (index < 0 || index >= length list) then False else True