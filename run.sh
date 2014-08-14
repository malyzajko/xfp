#!/bin/bash
java -cp lib/xfp.jar:lib/parser.jar:lib/scala-library.jar:. ec.Evolve \
  -file ec/app/fixedpoint/fixedpoint.params
