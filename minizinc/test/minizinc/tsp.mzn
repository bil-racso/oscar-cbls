%
% TSP with circuit predicate in Minizinc.
%
% Model created by Krzysztof Kuchcinski
%

include "circuit.mzn";

% Data

int: n; % number of nodes

array[1..n] of var 1..n: x;             % the circuit
array[1..n, 1..n] of int: distances;  % distance matrix
array[1..n] of var int: d;
var int: distance;

solve :: int_search(x, first_fail, indomain_min, complete) minimize distance;
%solve :: int_search(d, max_regret, indomain_min, complete) minimize distance;

constraint

    circuit(x)
    /\ 
    forall(i in 1..n) (
    let {
         array[1..n] of int: l = [ distances[i,j] | j in 1..n ]
	}
	in
	 l[x[i]] = d[i]
    )
    /\ 
   distance = sum(i in 1..n) (d[i])
;

% 
% data
% 

n = 12;
distances = 
array2d(1..n, 1..n,
[
0, 509, 501, 312, 1019, 736, 656, 60, 1039, 726, 2314, 479,
509, 0, 126, 474, 1526, 1226, 1133, 532, 1449, 1122, 2789, 958,
501, 126, 0, 541, 1516, 1184, 1084, 536, 1371, 1045, 2728, 913,
312, 474, 541, 0, 1157, 980, 919, 271, 1333, 1029, 2553, 751,
1019, 1526, 1516, 1157, 0, 478, 583, 996, 858, 855, 1504, 677,
736, 1226, 1184, 980, 478, 0, 115, 740, 470, 379, 1581, 271,
656, 1133, 1084, 919, 583, 115, 0, 667, 455, 288, 1661, 177,
60, 532, 536, 271, 996, 740, 667, 0, 1066, 759, 2320, 493,
1039, 1449, 1371, 1333, 858, 470, 455, 1066, 0, 328, 1387, 591,
726, 1122, 1045, 1029, 855, 379, 288, 759, 328, 0, 1697, 333,
2314, 2789, 2728, 2553, 1504, 1581, 1661, 2320, 1387, 1697, 0, 1838,
479, 958, 913, 751, 677, 271, 177, 493, 591, 333, 1838, 0
]);


output [
       "x: ", show(x),"\n",
       show(distance)
]
