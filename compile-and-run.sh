#!/bin/sh

SCALA_HOME=~/scala/2.11.8
DAY=$1
echo Compiling day $DAY...
$SCALA_HOME/bin/scalac -d bin day${DAY}.scala
echo Running day $DAY...
$SCALA_HOME/bin/scala -classpath bin advent.day${DAY}.Main day${DAY}-input.txt
