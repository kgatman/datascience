/*creating the data*/

data example1;
    input bench $ variety $ height @@;
    cards;
1 1 19.8 1 2 21.9 1 3 16.4 1 4 14.7
2 1 16.7 2 2 19.8 2 3 15.4 2 4 13.5
3 1 17.7 3 2 21.0 3 3 14.8 3 4 12.8
4 1 18.2 4 2 21.4 4 3 15.6 4 4 13.7
5 1 20.3 5 2 22.1 5 3 16.4 5 4 14.6
6 1 15.5 6 2 20.8 6 3 14.6 6 4 12.9
;

/* Data Exploration*/

/*1.*/
proc means data=example1 min mean median std max;
    class bench;
    var height;
run;

/*2.*/
ods graphics on;
proc sgplot data=example1;
    scatter x=variety y=height / group=bench;
    xaxis type=discrete;
run;

/*3.*/
ods graphics on;
proc sgplot data=example1;
    vbox height / category=variety;
run;

/*4.*/
ods graphics on;
proc univariate data=example1;
    var height;
    histogram;
run;

proc mixed data=example1 covtest;
    class bench variety;
    model height=variety /s htype=3 e;
    random bench /s;
    lsmeans variety /adjust=bon diff=all e;
    contrast "variety type 3"
        variety 1 0 0 -1;
        variety 0 1 0 -1;
        variety 0 0 1 -1;
    contrast "diff variety 1&2" variety 1 -1 0 0;
    contrast "diff variety 2&4" variety 0 1 0 -1;
    estimate "diff variety 1&2" variety 1 -1 0 0;
    estimate "diff variety 2&4" variety 0 1 0 -1;
run;