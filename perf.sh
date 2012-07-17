#!/bin/sh
cp target/oscar.jar perf/oscar.jar
cd perf
D1=$(date +"%m-%d-%y")
D2=$(date +%s)
C=`hg id -i`
for f in `ls -1 *.scala`; do
  echo "File -> $f"
  SECONDS=0; scala  -cp oscar.jar  -P:continuations:enable $f ; echo "that took approximately $SECONDS seconds"
  echo $f $SECONDS $D2 $D1 $C >> ../perfresults.txt
  echo $f $SECONDS $D $C
done
cd ..
scala analyze


