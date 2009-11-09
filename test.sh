#!/bin/sh

rm -rf testrepo
./CreateSpimRepo testrepo
./AddPIObject testrepo testvcards/1
./AddPIObject testrepo testvcards/2
