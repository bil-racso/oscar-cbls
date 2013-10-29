#!/bin/sh
#author       : Debroux Léonard : leonard.debroux@gmail.com 
#description  : Outputs an html version of the diff 
#                     of the two files in arg
#date         : 30/08/2013
#===========================================================

cd test/flatzinc
for f in `ls *.fzn`
do
	echo "<h3>Test of $f:<h3>" >> test_result.html
	echo "File -> $f"
	name=${f%%.*}
	java -cp /home/inekar/Documents/hg/oscar/target/oscar.jar oscar.cp.minizinc.FlatZinc2OscaR -a $f > ../output/${name}.output
	echo "Output created for $f"
done
