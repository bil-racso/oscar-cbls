var 2..3: x;
var 1..4: y;

solve satisfy;

constraint
	x <= y
;

output [show([x,y])];