
x "mkdir ./data";
filename data1 "./data/donnees1.csv";
filename data2 "./data/donnees2.csv";
filename data3 "./data/donnees3.csv";
filename data4 "./donnees4.csv";

/*Scrapping data*/
proc http
url='https://query1.finance.yahoo.com/v7/finance/download/TSLA?period1=1277769600&period2=1683244800&interval=1d&events=history&includeAdjustedClose=true'
method="get" out=data1;
run;

proc http
url='https://query1.finance.yahoo.com/v7/finance/download/HYU.SG?period1=1198800000&period2=1683244800&interval=1d&events=history&includeAdjustedClose=true'
method="get" out=data2;
run;


proc http 
url='https://query1.finance.yahoo.com/v7/finance/download/OR.PA?period1=946857600&period2=1683676800&interval=1d&events=history&includeAdjustedClose=true'
method="GET" out=data3;
run;

/*Import data*/
proc import file=data1 out=donnees1 dbms=csv replace;
delimiter=',';
run;

proc import file=data2 out=donnees2 dbms=csv replace;
delimiter=',';
run;

proc import file=data3 out=donnees3 dbms=csv replace;
delimiter=',';
run;

quit;
proc import file=data4 out=donnees4(keep=Date Change) dbms=csv replace;
delimiter=',';
format Date MMDDYY10.;
informat Date Date9.;
run;

proc sort data=donnees4;
by Date;
run;
