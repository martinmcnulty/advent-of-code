@echo off

SET SCALA_HOME=E:\scala\2.11.7
SET DAY=%1%
ECHO Compiling day %DAY%...
CALL %SCALA_HOME%\bin\scalac -d bin day%DAY%.scala
ECHO Running day %DAY%...
%SCALA_HOME%\bin\scala -classpath bin advent.day%DAY%.Main day%DAY%-input.txt
