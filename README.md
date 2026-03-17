# DNES

A NES simulator for learning purpose.

## Reference

- [OneLoneCoder/olcNES](https://github.com/OneLoneCoder/olcNES)

  There is a lot of code referencing this repository and OneLoneCoder's Youtube video.

## Frontend

Basically, I split NES core simulation module and interactive module(named 'frontend'). So that I can conveniently transplant this NES simulator to other platform by implementing a new 'frontend' if needed in the future.

Implemented frontends:

- SDL3

## Tested ROM

- battle city mapper 000
- donky kong mapper 000
- nestest (wihtout illegal code test) mapper 000
- final fantasy mapper 001
- tetris mapper 001
- contra mapper 002
- metal gear mapper 002
- bump 'n' jump mapper 003
- super mario bros. mapper 004
- super mario bros. 2 (USA) mapper 004
- super mario bros. 3 mapper 004
- the hyrule fantasy: the legend of zelda (sounds not so good) mapper 004

## How to

### Pull source code

```sh
git clone https://github.com/dangjinghao/dnes
git submodule init
git submodule update
```

### Build

The build system of this project is `CMake`, so you can execute those command to build:

```sh
cmake -S . -B build/ && cmake --build build/
```

### Run

```sh
./build/dnes <rom-path>
```

#### Environment variables

- `DNES_SAV_PATH`: specify the path of game save data file to save or load game data.

  But it is supported only in the `mapper_001`(MMC1) mapper. And only final fantasy is tested.

## Features

- A completely controllable visiual debug panel
  - palette display and switch
  - pattern display
  - PC address dis-assembler
  - register display
  - OAM display
  - forward to next instruction / next frame / pause / reset
- Some Bug fixes and new features(compared to olcNES)
- game sav data dump/load (only mapper 001 is supported)
- registrable bus

  This is an interesting idea that helps me to detect some address overlay problems.

## Vendor

By default, DNES uses the vendored SDL as a git submodule. So it's not necessary for us to install SDL dependency.

But if necessary, you can link the system SDL library in building stage:

```sh
cmake -S . -B build/ -D DNES_VENDORED=off && cmake --build build/
```
