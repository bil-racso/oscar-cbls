#!/bin/sh
cd test/flatzinc
result="../../test_result.html"
touch $result
echo "<html>" > $result
echo "<title>Test of flatzinc models</title>" >> $result
echo "<body>" >> $result
for f in `ls *.fzn`
do
	echo "<h3>Test of $f:<h3>" >> $result
	echo "File -> $f"
	name=${f%%.*}
	#¢all solver on $f > res
	java -cp /home/inekar/Documents/hg/oscar/target/oscar.jar oscar.cp.minizinc.FlatZinc2OscaR -a $f > res.tmp
	../../diff2html ../output/${name}.output res.tmp >> $result
done
rm res.tmp
# rm diff_result
echo "</html>" >> $result
echo "</body>" >> $result