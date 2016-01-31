#!/bin/bash

mkdir coverage

echo
echo
echo
echo Initiating Code details
echo
echo

lcov -c -i -b `pwd` -d `pwd`/.obj -o coverage/baseline.info

echo
echo
echo
echo Running Software
echo
echo

./openDynamics

echo
echo
echo
echo Scanning data
echo
echo

lcov -c -b `pwd` -d `pwd`/.obj -o coverage/testNumber.info

echo
echo
echo
echo Compiling Report
echo
echo

lcov -a coverage/baseline.info -a coverage/testNumber.info -o coverage/compiled.info

echo
echo
echo
echo Creating HTML
echo
echo

genhtml coverage/compiled.info -o coverage