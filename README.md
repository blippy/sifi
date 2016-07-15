# sifi
Text-base finance app

## Preferred method of installation

* 18-Jun-2016 cabal
* 28-Feb-2016 stack

## Cabal


Once you have downloaded the sources, you can build the project:

    cabal sandbox init
    cabal install # install the dependenices
    cabal build # create the executable: dist/build/sifi/sifi

To play around with the project as a developer:

    cabal repl
    :l Etb
    ceb
    


To rebuild the modules:
cabal build

Generate documentation: cabal haddock


## Stack

Here's how you build the project:

    stack init
    stack build

If you want a repl for development:

    stack ghci
    

# Profiling

https://nikita-volkov.github.io/profiling-cabal-projects/
http://stackoverflow.com/questions/32123475/profiling-builds-with-stack

stack exec my-exec -- +RTS -p

## WIN7/CYGWIN

Ensure the following is in your PATH:


	C:\Users\mcarter\AppData\Roaming\local\bin\



## Notes

In order to use the "browse" option on Linux, "firefox" needs to be installed. sifi does not check for this.

See sifi.cfg: you will need to install a runcom (rc) file

