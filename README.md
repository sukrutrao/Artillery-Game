# Artillery-Game

A turn based Artillery Game in Haskell

### Installation Instructions

* Install Haskell Platform on your system. Installation instructions can be found [here](https://www.haskell.org/platform/)
* Clone the repository, using

 ```
 $ git clone https://github.com/GoodDeeds/Artillery-Game.git
 ```

* Change the working directory to that of the cloned repository

```
$ cd Artillery-Game
```

* Install using Cabal

```
$ cabal install
```

### Playing the Game

* The game is a multi-player turn based game. Currently, the number of players supported is three.
* Each player has one tank. Tanks can shoot weapons on opponent tanks.
* Each tank starts with a certain health. Being hit by weapons decreases the health. If it reaches zero, the tank is eliminated from the game.
* Each player is given three weapon types. The quantities of each are 1000, 10, and 2 respectively.
* Each weapon type has an associated damage and impact radius. In lexicographic order, the number of weapons of the type reduces, the damage increases, and the impact radius decreases.
* The objective of each player is to eliminate all other players' tanks. The last tank standing, if any, is the winner.
* To triangle marker denotes whose turn it is. The bar on the top of a tank denotes its remaining health.
* To move the tank, press the left and right arrow keys.
* To change the power of launch of weapon, use the + and - keys.
* To change the angle of the turret, use the A and D keys.
* To launch the weapon, use the Space key.