# HLP 2021 Project
This is the HLP group project for team 5. The code is made to act as the GUI library for the Issie application. Issie is an "Interactive Schematic Simulator with Integrated", for more information about issie see the [Issie Repo](https://github.com/tomcl/ISSIE) . 
The aim is to be able to replace the currently used drawing library. 

The code allows you to add a different range (note that for demo purpose ```alt-n``` creates new symbol of random type) of component symbol displayed on the canvas and connect them together using the wires. The code is set up three modules ```sheet```, ```buswire```, and ```symbol``` where they are complied as ```symbol -> buswire-> sheet ```, i.e. sheet acts as the perent module.
The symbol element creation is handled by the [symbol.fs](./src/Render/symbol.fs) while the wire creation is handled by [buswire.fs](./src/Render/buswire.fs). The sheet module written in [sheet.fs](./src/Render/sheet.fs) is used for the overall control of the system with function such as copy, past, drag, and select etc.



## Running the Demo
The code is setup so that the some symbols are generated on intialisation which can then be used to test all the different features.
The code which handles the initalisation in the code are the ```init```  dummy functions in ```symbol``` and ```buswire```. For production these function would most likely not contain the intialization of any symbols or wires.

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
       |- issie_interface.md
       |- feature_summary.md
       \- symbol.md
       
       
## Interface Definition
All interface used in the project are defined in the interface.md file found in [docs/](./docs), a file for each module. There are also two other files, [common_types.md](./docs/common_types.md) and [notes.md](./docs/notes.md). [common_types.md](./docs/common_types.md) contains all the new type definitions we had to agree on, and [notes.md](/.docs/notes.md) contains notes on our design decisions. There is also a file named [issie_interface.md](./docs/issie_interface.md) which contains information about the function which exsist in the code for potential interface with issie.

## Features
To see an exhaustive list of features, refer to [feature_summary.md](./docs/feature_summary.md).

## Project allocation
This was the inital allocation used under the group phase. In the project phase the division has become less rigid as everyone has worked together to put all the modules together.
###### Symbol
  * Chizu
  * Marcus
###### BusWire
  * Wei Loon
  * Sofia 
###### Sheet
  * Ole
  * Ben
