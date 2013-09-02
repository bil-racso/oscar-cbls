#!/bin/sh
#author       : Debroux Léonard : leonard.debroux@gmail.com 
#description  : Outputs an html version of the diff 
#                     of the two files in arg
#date         : 30/08/2013
#===========================================================

cd test/flatzinc
result="../../test_result.html"
touch $result
echo "<html>" > $result
echo "<head>" >> $result
echo "<title>Test of flatzinc models</title>" >> $result
echo "<style type="text/css"> " >> $result
echo "<!--" >> $result
echo "BODY { font-family: Versana,Arial,Helvetica }" >> $result
echo "TH { font-family : Verdana, Arial, Helvetica;font-size : 8pt;color : White; }" >> $result
echo "TD { font-family : Verdana, Arial, Helvetica;font-size : 7pt;color : Black; }" >> $result
echo "-->" >> $result
echo "</style>" >> $result
echo "</head>" >> $result
echo "<title>Test of flatzinc models</title>" >> $result
echo "<body>" >> $result
for f in `ls *.fzn`
do
	echo "<h3>Test of $f:<h3>" >> $result
	echo "File -> $f"
	name=${f%%.*}
	#¢all solver on $f > res
	java -cp /home/inekar/Documents/hg/oscar/target/oscar.jar oscar.cp.minizinc.FlatZinc2OscaR -a $f > res.tmp
	../../diff2html ../output/${name}.output res.tmp $f >> $result
done
rm res.tmp
# rm diff_result
echo "</html>" >> $result
echo "</body>" >> $result