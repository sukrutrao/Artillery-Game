# Artillery Game

A three-player turn based Artillery Game written in Haskell. Each player controls a tank, with a set of weapons, and the objective is to destroy the tanks of the other players.

## Installation
### Prerequisites

* Haskell Platform. Installation instructions can be found [here](https://www.haskell.org/platform/)

### Building the project
* Clone the repository

 ```bash
 $ git clone https://github.com/GoodDeeds/Artillery-Game.git
 $ cd Artillery-Game
 ```

* Install using Cabal

```bash
$ cabal install
```

* Copy the file `Level1.txt` from `src` folder to `dist/build/Artillery-Game` folder
```bash
$ cp src/Level1.txt dist/build/Artillery-Game/
```

### Starting the game

Use

```bash
$ ./Artillery-Game
```


## Playing the Game

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

## Documentation
Code documentation can be found [here](https://gooddeeds.github.io/Artillery-Game).

## Authors
* [Sukrut Rao](https://github.com/GoodDeeds)
* [Harsh Agarwal](https://github.com/sipian)
* [Atul Raj](https://github.com/atulraj297)


## License

This project is provided under the [MIT License](LICENSE).
