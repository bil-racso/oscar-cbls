% 
% Simple knapsack problem in Minizinc.
%  
% ( Cf http://www.hakank.org/constraints/Knapsack.java )

%
% This Minizinc program is written by Hakan Kjellerstrand and is commented in 
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%

%
% This is a slightly modified version from the one from http://www.hakank.org/
%

% Define a predicate for generality
predicate knapsack(array[int] of var int: Weights, array[int] of var int: Take, var int: Wtmax) =
           sum(i in index_set(Weights)) ( Weights[i] * Take[i]) <= Wtmax
;


int: n; % number of objects
int: weight_max;  % maximum weight allowed (the capacity of the knapsack)
array[1..n] of int: values;
array[1..n] of int: weights;
array[1..n] of var int: take; % 1 if we take item i; 0 otherwise

var int: profit = sum(i in 1..n) (take[i] * values[i]);

solve maximize profit;

constraint
   % all elements in take must be >= 0
   forall(i in 1..n) (  take[i] >= 0  )
   /\
   % and then use the knapsack predicate
   knapsack(weights, take, weight_max)
;


% data
n = 5;
weight_max = 35;

% Gold, Silver, Bronze        
values = [14, 12, 7, 3, 1];
weights = [5, 4, 3, 2, 1];

output [
   show(take), "\n"

];