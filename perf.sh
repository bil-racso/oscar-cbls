#!/bin/sh
echo $PATH
#/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
cp target/oscar.jar perf/oscar.jar
cd perf
D1=$(date +"%m-%d-%y")
D2=$(date +%s)
C=`hg id -i`
for f in `ls -1 *.scala`; do
  echo "File -> $f"
  f2=${f%%??????}
  scalac  -cp oscar.jar  -P:continuations:enable $f
  SECONDS=0; scala  -cp oscar.jar  -P:continuations:enable $f2 ; echo "that took approximately $SECONDS seconds"
  echo $f $SECONDS $D2 $D1 $C >> ../perfresults.txt
  echo $f $SECONDS $D $C
done
cd ..
scala analyze


