#!/bin/sh

ghc --make AddPIObject
ghc --make AddPILink
ghc --make CreateSpimRepo
ghc --make InjectEvent
ghc --make MIMEDir_test
./MIMEDir_test
./test.sh
