#!/bin/sh

rm -rf testrepo
./CreateSpimRepo testrepo
./AddPIObject testrepo testvcards/1
./AddPIObject testrepo testvcards/2
./AddPILink testrepo Friends 5111627 519615
