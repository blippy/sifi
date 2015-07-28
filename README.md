# sifi
Text-base finance app


Once you have downloaded the sources, you can build the project:

    cabal sandbox init
    cabal install # install the dependenices
    cabal build # create the executable: dist/build/sifi/sifi

To play around with the project as a developer:

    cabal repl
    :l Etb
    createEtb
    


To rebuild the modules:
cabal build

Generate documentation: cabal haddock


See sifi.cfg: you will need to install a runcom (rc) file 

## Notes

In order to use the "browse" option on Linux, "firefox" needs to be installed. sifi does not check for this.