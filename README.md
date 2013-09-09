Copyright (c) 2013, Mark VandenBrink. All rights reserved.

# Introduction

A Lisp library to monitor a Motorola SB6121 cable modem.  Creates a CSV
of all relevant data from the modem's HTTP interface.


**Mostly complete.  Your mileage may vary.**

# Dependencies

All avalailable via quicklisp

* optima
* cl-ppcre
* drakma
* closure-html/closure-common
* fare-csv

# Objectives

I was having some issues with my cable modem rebooting, so I whipped together this set of tools to monitor it.

For the SB6121, I read from the following URLs:

* __http://192.168.100.1/indexData.htm:__ to get the modem's status
* __http://192.168.100.1/cmSignalData.htm:__ to get the modem's stats like power-levels, etc
* __http://192.168.100.1/cmLogsData.htm:__ to read the modem's log

## High-Level Design

None. No, really. Just a hack.

### The Files

* __surfboard.lisp:__ opens HTTP requests to modem, collects data, then writes a CSV file. Uses optima and cl-ppcre to hack up the HTML. Fugly code.
  BTW, the process-modem-log is most probably buggy.
* __watch-modem.sh:__ a shell script that runs the watcher. Use this to run the program from the command line to collect data.
* __report.lisp:__ template for creating modem reports.  The only one instantiated here is creating a data file to gnuplot that graphs power-levels.
* __power.gp:__ a shell script that invokes gnuplot to plot the power& SNR levels.
* __plot-power.sh:__ quick script to run the modem log and gnuplot the results.

### Sample Invocation

Assuming you have quicklisp set up and use CCL, you can launch watch-modem.sh to start the data collection.  After some period of time, you
can load up report.lisp and run report-levels to create a data file that can be fed to power.gp.

Here is a [sample of gnuplot ouput](https://github.com/mv2devnul/SurfBoard/blob/master/SampleOutput.png)

See [this page](http://www.dslreports.com/faq/3412) for info on how to interpret the plot.

