# sprockets

## Description

Command-line program for doing bicycle gearing calculations.

It prompts the user to enter the number of gears in the front and rear
cassettes of the bicycle gearing. After that, the number of teeth on each gear
is prompted.

The program then prints the resulting gear ratios, gear steps and velocities for
all different front/rear gear combinations. Wheel size is assumed to be 700 mm
in all calculations.

## Sample output

    [user@fedora sprockets]$ fpm run
    Project is up to date
     hello from project sprockets,
     a bicycle gearing calculator program. 
    Enter number of chainrings [front]: 2
    Enter number of sprockets [rear]: 5
    :: Chainrings [front] ::
     Enter size for gear #1: 34
     Enter size for gear #2: 50
     
    :: Sprockets [rear] ::
     Enter size for gear #1: 36
     Enter size for gear #2: 30
     Enter size for gear #3: 25
     Enter size for gear #4: 21
     Enter size for gear #5: 18
     
    :: Gearing overview ::
     
                  Chainrings [front]  34-50
                    Sprockets [rear]  36-30-25-21-18
              Theoretical # of gears  10
                     Max. gear ratio  1.06
                     Min. gear ratio  .36
                         Progression  294.1 %
      Chainring step % [min/avg/max]  NaN / NaN / NaN
       Sprocket step % [min/avg/max]  14.3 / 15.9 / 16.7
     
    :: Gear step tables ::
    :: Chainrings [front] ::

        1 - 2  
      -47.1 % 

    :: Sprockets [rear] :: 

        1 - 2    2 - 3    3 - 4    4 - 5  
       16.7 %   16.7 %   16.0 %   14.3 % 

    :: Gear ratio table ::
      F: Chainring [front], R: Sprocket [rear]

            R #1    R #2    R #3    R #4    R #5   
    F #1    1.06    0.88    0.74    0.62    0.53  
    F #2    0.72    0.60    0.50    0.42    0.36  
                                                  
    :: Gear vs. cadence [rpm] vs. speed [km/h] table ::

                                  cadence
    chainring      sprocket       40      60      80     100     120
    ----------------------------------------------------------------
    F #1 (34T)     R #1 (36T)     5.0     7.5    10.0    12.5    15.0
    F #1 (34T)     R #2 (30T)     6.0     9.0    12.0    15.0    17.9
    F #1 (34T)     R #3 (25T)     7.2    10.8    14.4    17.9    21.5
    F #1 (34T)     R #4 (21T)     8.5    12.8    17.1    21.4    25.6
    F #1 (34T)     R #5 (18T)    10.0    15.0    19.9    24.9    29.9
    -----------------------------------------------------------------
    F #2 (50T)     R #1 (36T)     7.3    11.0    14.7    18.3    22.0
    F #2 (50T)     R #2 (30T)     8.8    13.2    17.6    22.0    26.4
    F #2 (50T)     R #3 (25T)    10.6    15.8    21.1    26.4    31.7
    F #2 (50T)     R #4 (21T)    12.6    18.8    25.1    31.4    37.7
    F #2 (50T)     R #5 (18T)    14.7    22.0    29.3    36.7    44.0
    -----------------------------------------------------------------


## Building and running

* Install [FPM](https://github.com/fortran-lang/fpm) (Fortran Package
  Manager) and copy it into your $PATH as `fpm`.
* Clone the git repository.
* Execute the command `fpm run` inside the base directory of the 
  repository. 

This program was developed with [GFortran](https://gcc.gnu.org/fortran/) 
and packaged with [FPM](https://github.com/fortran-lang/fpm).

