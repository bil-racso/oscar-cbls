#!/bin/sh
cd test/flatzinc
for f in `ls *.fzn`
do
	echo "<h3>Test of $f:<h3>" >> test_result.html
	echo "File -> $f"
	name=${f%%.*}
	java -cp /home/inekar/Documents/hg/oscar/target/oscar.jar oscar.cp.minizinc.FlatZinc2OscaR -a $f > ../output/${name}.output
	echo "Output created for $f"
done
