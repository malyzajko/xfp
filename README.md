
The xfp project
==

xfp is a tool that can synthesize rewrites of an arithmetic expressions which
are more accurate when implemented in fixed-point arithmetic. The search is performed
with genetic programming, for which we use ECJ (A Java-based Evolutionary Computation 
Research System). More details can be found in the paper
Synthesis of Fixed-Point Programs, E. Darulova, V. Kuncak, R. Majumdar, I. Saha, EMSOFT'13.xf 






This project consists of two parts:
- an analysis tool for estimating worst-case errors of fixed-point code
- genetic programming, implemented in the ECJ framework for
synthesizing more precise fixed-point expressions.

Examples are located in the inputs/ directory.

Analysis tool
----

The analysis_tool/ directory contains the source files and
a build file for sbt (simple-build-tool).

An easy way to compile, run and package the tool is using sbt.
You can of course use any other tool.

Start sbt in interactive mode in the analysis_tool directory.
- to compile: type 'compile'
- to package: type 'package'  (the jar file will be placed into the target/ directory)
- to run: type 'run' to see the options of the analysis tool.

Note: if you want to make changes visible for GP,
you need to replace the xfp.jar in the lib/ directory.

GP
----
The xfp specific source code is in the ec/app/fixed/ directory
and the source code for the expression parser from parser.jar is located
in ec/utils/.

Settings (gp specific): ec/app/fixedpoint/fixedpoint.params

To build: make

To run the one example specified in fixedpoint.params: run.sh
Results are written to the results.txt file.

To change what is printed in the results.txt output file, modify
ec.simple.SimpleStatistics (check the end of the file).
