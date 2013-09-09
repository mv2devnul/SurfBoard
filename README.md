Copyright (c) 2013, Mark VandenBrink. All rights reserved.

# Introduction

A Lisp library to monitor a Motorola SB6121 cable modem.  Creates a CSV
of all relevant data from the modem's HTTP interface.


**Mostly complete.  Your mileage may vary.**

# Dependencies

All avalailable via quicklisp

* optima
* drakma
* closure-html/closure-common
* fare-csv

# Objectives

I was having some issues with my cable modem rebooting, so I whipped together this set of tools to monitor it.

For the SB6121, I read from the following URLs:

*__http://192.168.100.1/indexData.htm__: to get the modem's status
*__http://192.168.100.1/cmSignalData.htm__: to get the modem's stats like power-levels, etc
*__"http://192.168.100.1/cmLogsData.htm__: to read the modem's log

## High-Level Design

### The Files

*__surfboard.lisp__: opens HTTP requests to modem, collects data, then writes a CSV fie
*__watch-modem.sh__: a shell script that runs the watcher
*__report.lisp__: template for creating modem reports.  The only one instantiated here is creating a data file to gnuplot that graphs power-levels
*__power.gp__: a shell script that invokes gnuplot to plot the power levels

