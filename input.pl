% read_number(+LowerBound, +UpperBound, -Number)
% used to read inputs between the Lower and Upper Bounds
read_number(LowerBound, UpperBound, Number):-
  read(Number),
  Number =< UpperBound, Number >= LowerBound, skip_line.
