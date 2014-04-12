# HIB
The Haskell Interactive Battleship game. Originally a programming project for
the course 'Programming Paradigm' in the University of Antwerp.

* 10x10 playing field.
* 2 pirates.
* 4 ships for each pirate (of length 2, 3, 4 and 5), arranged either
  horizontally or vertically.
* Player's goal is to sink the opponent's fleet by calling out each of the
  cells belonging to the ships in that fleet.
* On each turn, a player can fire as many shots as he has ships that aren't
  sunk.

## Try it..
There are two ways to try the game:

### Interactive
```
$ cd src/
$ runhaskell Main.hs
```

An example of inputs:

```
1: Enter yer name > Alice
Alice: Ahoy!
2: Enter yer name > Bob
Bob: Ahoy!
Alice: Enter yer ship [2 points] > (0,0);(1,0)
Alice: Enter yer ship [3 points] > (0,0);(0,1);(0,2)
Belay there! Overlaps with yer fleet.
Alice: Enter yer ship [3 points] > (0,1);(0,2);(0,3)
Alice: Enter yer ship [4 points] > (4,1);(4,2);(4,3);(4,4)
Alice: Enter yer ship [5 points] > (5,5);(6,5);(7,5);(8,5);(9,5)
Bob: Enter yer ship [2 points] > (9,9);(8,9)
Bob: Enter yer ship [3 points] > (2,1);(3,1);(4,1)
Bob: Enter yer ship [4 points] > (5,1);(5,2);(5,3);(5,4)
Bob: Enter yer ship [5 points] > (8,0);(8,1);(8,2);(8,3);(8,4)
Alice: Gimme chart!
----------
----------
----------
----------
----------
----------
----------
----------
----------
----------
Alice: Fire in the hole!
Alice: Gun ready at > (0,0)
Alice: FIRE!
Blimey! Miss!
Alice: Gimme chart!
o---------
----------
----------
----------
----------
----------
----------
----------
----------
----------
Alice: Fire in the hole!
Alice: Gun ready at > (8,0)
Alice: FIRE!
Yo-ho-ho! Hit!
Alice: Gimme chart!
o-------x-
----------
----------
----------
----------
----------
----------
----------
----------
----------
...
```

### From file
With `runhaskell` you can also pipeline inputs from file. In that case, the
game will abort when EOF has reached (so not necessarilly the end of the game).
There's no possibility to read from file and then continue the game in
interactive mode.

## .. love it!
If you liked the game, you can decide to install the game into your computer.

### Building
A `Makefile` is provided, so building the executable should be as easy as 3.14:

```sh
$ make
```

This will compile and link the files using `ghc` (Glasgow Haskell Compiler) by
default. Otherwise you can set the compiler to be used by overriding the
`HCOMPILER` variable:

```sh
$ make HCOMPILER="<command to build the executable>"
```

> In the future, a Cabal setup script will be provided instead of a Makefile.

Assuming you have used the default settings, an executable can be found in the
`bin/` directory: `hib` you can just run the game locally from there.

### Installing
If you liked HIB, you can also install it into your computer:

```sh
$ make install
```

The executable `bin/hib` will be then put in the `/usr/local/bin/` directory by
default. However, if you've renamed the executable or want another target
directory, you can override the variables `EXE` and `BIN` respectively:

```sh
$ make install EXE="battleship.exe" BIN="/usr/games/bin/"
```

Of course, you can revert it:

```sh
$ make uninstall [variables]
```

## Contributing
Yarr!! Send a pull request, post an issue, ... Ye be welcome aboard!
