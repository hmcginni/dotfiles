# hrmutils

Custom productivity utilities for Ubuntu 16.04 LTS.

These are mostly command-line utilities written in `bash` that accomplish various small tasks for a Ubuntu 16.04 workstation. The aim is to keep the tools compatible with Ubuntu 18.04 as well, but beyond that you are on your own!


## Features

README files get out of date. This repository changes too often to keep a manually-written text consistent with the implemented functionality, but until a README autogenerator is created, here are some highlights:

-   `finder` - Application launcher and file opener based on `gnome-terminal` and `fzf`
-   `sc` - Command-line path shortcut (alias) manager
-   `tile` - Tile windows on the current monitor (requires `xdotool` and `wmctrl`)
-   `slockd` - Daemon that, when the user unlocks the screen, runs all scripts in `unlock.d/`


## Installation and Usage

1.  Clone this repository
2.  `$ sudo apt install xdotool wmctrl`
3.  Run `/install/install_utils`
