# HLP 2021 Project
This is the HLP group project for team 5. The code is made to act as the GUI library for the Issie application. 
It's aim is to be able to replace the currently used library. The code allows you to add a different range of symbols to your canvas and connect them together using wires.
The symbol creation is handled by the [symbol.fs](./src/Render/symbol.fs) while the wire creation is handled by [buswire.fs] (./src/Render/buswire.fs). The sheet module written in [sheet.fs] (./src/Render/sheet.fs) is used for the overall control of the system with function such as copy, past, drag, and select etc.



## Running the Demo

#### First time running
1. In the root directory, open up the command prompt
2. Run the following code
    - Windows: ```build```
    - Linux: ```build.sh```
3. If the program fails to run, go to the original skeleton code, retrieve the file ```build.fsx```, and paste it into the root directory, then repeat step 2 again.

#### Subsequent times
1. In the root directory, open up the command prompt
2. Run the following code
    - ```npm run dev```

## File structure
    <root>
    |- src // Contains the source code of the project
    \- docs // Contains the documents explaining the layout of the code.
       |- buswire.md
       |- common_types.md
       |- notes.md
       |- sheet.md
       \- symbol.md
       
## Interface Definition
All interface used in the project are defined in the interface.md file found in `docs/`, a file for each module. There are also two other files, `common_types.md` and `notes.md`. `common_types.md` contains all the new type definitions we had to agree on, and `notes.md` contains notes on our design decisions.

## Features
To see an exhaustive list of features, refer to [feature_summary.md](./docs/feature_summary.md).

## Project allocation
###### Symbol
  * Chizu
  * Marcus
###### BusWire
  * Wei Loon
  * Sofia Jegnell
###### Sheet
  * Ole
  * Ben
