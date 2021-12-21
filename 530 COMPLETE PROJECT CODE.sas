*------------------------------------------------------------

STAT 530
Experimental Design & Analysis
Project

Factors:
A = Condensation Temperature
B = Amount of Material 1
C = Solvent Volume
D = Condensation Time
E = Amount of Material 2 on yield

Response:
Y = Planning Experiments to Increase Research Efficiency

*-------------------------------------------------------------;


* Design Matrix with Data;
data layout;
  do C = -1 to 1 by 2; 
  do B = -1 to 1 by 2; 
  do A = -1 to 1 by 2; 
    E=A*C;
	D=B*E;
    input Run$ y @@;
    output;
  end; end; end;
cards;
e 23.2 ad 16.9 bde 16.8 ab 15.5 cd 23.8 ace 23.4 bc 16.2 abcde 18.1
;
run;

* Define Interaction Terms;
data inter;
set layout;
  AB = A*B; 
  AD = A*D;
run;

proc sql; 
  create table data as 
    select A, B, C, D, E, Run, y 
    from inter; 
quit; 

title 'One-Half Fractional Factorial Design for 5 Factors';
title2 'Defining Relation: I = ACE, I = BDE and I = ABCD';
proc print data=data;
run;

* ANOVA and Estimate Effects;
title 'Analysis of Variance Table and Estimating Effects';
proc glm data=data;
  class A B C D E;
  model y = A B C D E;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
	estimate 'C' C -1 1; 
    estimate 'D' D -1 1;
	estimate 'E' E -1 1; 
run; quit;

* Normal Probability Plot of the Residuals;
ods graphics on;
proc glm data=data plots=all;
  model y = A B C D E;
  run;
  ods graphics off;
