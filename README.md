# Compact control micro program

[![Generic badge](https://img.shields.io/badge/Company-AitinTech-blue.svg)](http://AitinTech.ir/)
[!["Contributors"](https://img.shields.io/github/contributors/saeeddiscovery/CompactControl.svg)](https://github.com/saeeddiscovery/CompactControl/graphs/contributors)

## IDE
- Use BASCOM-AVR to edit and compile the code (.bas).
- Use Proteus to edit and run the simulation project (.pdsprj).
- To show line numbers in BASCOM-AVR
    - Optins (menu) -> Environment -> Line Numbers

## Compile the code for Proteus
- Comment ```$crystal = 11059200``` (Line 2).
- Uncomment ```$crystal = 8000000``` (Line 3).
- Uncomment these lines (under the _Read_positions_ label, lines 687-692).
    ```basic
    Gant_cofin = -58618
    Collim_cofin = 28017
    X1_co = 2048
    X2_co = 2048
    Y1_co = 2048
    Y2_co = 2048
    ```

## Proteus Project
- Double-click on the COM port (P1) and set the proper "Physical port" and "BaudRate" (both physical and virtual).
- Double-click on the micro (U1) and load the compiled .hex file from the "Program File" field.

## Virtual Serial Port
- Download and install "Virtual Serial Port Emulator (VSPE)"
http://www.eterlogic.com/Products.VSPE.html
- Open VSPE and create a virtual port:
   - Device (menu) -> Connector -> Next -> COM port -> Finish
- Use the same COM port in the port properties in Proteus
- Minimize VSPE and run the CompactControl application and the Proteus project.
