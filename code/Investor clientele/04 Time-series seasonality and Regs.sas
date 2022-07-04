/** F test for monthly, daily and intraday returns ***/
/*************************Monthly return***************************/

data ret_a;
	set draft.msf;	
	month = month(date);
	if substr(stkcd,1,3) not in ("200","900");
run;

data ret_b;
	set draft.msf;	
	month = month(date);
	if substr(stkcd,1,3) in ("200","900");
run;

%let table = ret_a;
%let table = ret_b;
 
proc sort data = &table; by month;quit;
proc means data = &table noprint;
	by month;
	var ret;
	output out = month mean = m std = std;
quit;

data month1;
set month;
	m = m*100;
	m = round(m,0.001);
	r = catt(m,"%(", round(std,0.01), ")");
	keep month r;
run;

/* H0:u1=u2=...=u12 */
PROC ANOVA DATA=&table;
	CLASS month;
	MODEL ret=month;
	RUN; 
quit;

/* Regression for AB shares */
proc sql;
	create table msf 
	as select a.*, b.compname as compnameA, c.compname as compnameB
	from draft.msf as a left join china.ab_cross as b
	on a.stkcd = b.stkcdA
	left join china.ab_cross as c
	on a.stkcd = c.stkcdB;
quit;

data msf;
	set msf;
	B = 0;
	if compnameB ne "" then B = 1;
	jan = 0;
	if month(date) = 1 then jan = 1; 
	feb = 0;
	if month(date) = 2 then feb = 1;
	mar = 0;
	if month(date) = 3 then mar = 1;
	apr = 0;
	if month(date) = 4 then apr = 1;
	may = 0;
	if month(date) = 5 then may = 1;
	jun = 0;
	if month(date) = 6 then jun = 1;
	jul = 0;
	if month(date) = 7 then jul = 1;
	aug = 0;
	if month(date) = 8 then aug = 1;
	sep = 0;
	if month(date) = 9 then sep = 1;
	oct = 0;
	if month(date) = 10 then oct = 1;
	nov = 0;
	if month(date) = 11 then nov = 1;
	dec = 0;
	if month(date) = 12 then dec = 1;
	jd = 0;
	if month(date) in (6,12) then jd = 1;
	feb_b = feb*b;
	jun_b = jun*b;
	dec_b = dec*b;
	jd_b = jd*b;
	year = year(date);
run; 

data msf1;
	set msf;
	if compnameB ne "" or compnameA ne "";
run;

%let tablein = msf;
%let tablein = msf1; 

proc sort data = &tablein; by year; quit; 
proc reg data = &tablein(where = (substr(stkcd,1,3) not in ("200","900"))) outest = est_a noprint;
	by year;
	model ret = feb mar apr may jun jul aug sep oct nov dec;
quit;

proc reg data = &tablein(where = (substr(stkcd,1,3) in ("200","900"))) outest = est_b noprint;
	by year;
	model ret = feb mar apr may jun jul aug sep oct nov dec;
quit;


proc sort data = &tablein; by year; quit; 
proc reg data = &tablein outest = est_all noprint;
	by year;
	model ret = feb mar apr may jul aug sep oct nov feb_b jd jd_b;
quit;

proc means data = est_all mean t; quit;

%let lag = 12;
%let list = jan feb mar apr may jun jul aug sep oct nov dec;
%macro newey_west_m(table = ,coefout= ,tstatout = );
data &coefout; run;
data &tstatout; run;

data temptable;
	set &table;
	where year > 1999;
	rename intercept = jan;
	feb = intercept + feb;
	mar = intercept + mar;
	apr = intercept + apr;
	may = intercept + may;
	jun = intercept + jun;
	jul = intercept + jul;
	aug = intercept + aug;
	sep = intercept + sep;
	oct = intercept + oct;
	nov = intercept + nov;
	dec = intercept + dec;
run;

%do i = 1 %to %sysfunc(countw(&list));
%let depvar = %scan(&list,&i);
proc model data=temptable;
         endo &depvar; 
         instruments / intonly;
         parms b0;
         &depvar =b0;
         fit &depvar / gmm kernel=(bart,%eval(&lag + 1),0) vardef=n;
		 ods output parameterestimates = temp;
quit; 

data temp2 (rename = (estimate = alpha));
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	Estimate = Estimate;
	keep model dependent Estimate;
run;

data &coefout;
	set &coefout temp2;
	if model ne "";
run;

data temp2;
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	tvalue2 = strip(put(tvalue,10.2));
	keep model dependent tvalue2;
run;

data &tstatout;
	set &tstatout temp2;
	if model ne "";
run;
%end;
%mend;
%newey_west_m(table = est_a, coefout = coef_a, tstatout = tstat_a);
%newey_west_m(table = est_b, coefout = coef_b, tstatout = tstat_b);


%let lag = 12;
%let list = feb feb_b jd jd_b;
%newey_west_m(table = est_all, coefout = coef_all, tstatout = tstat_all);

/*************************Daily return***************************/

data dret_a;
	set draft.dsf;	
	weekday = weekday(date)-1; 
	if substr(stkcd,1,3) not in ("200","900");
run;

data dret_b;
	set draft.dsf;	
	weekday = weekday(date)-1; 
	if substr(stkcd,1,3) in ("200","900");
run;

%let tabled = dret_a;
%let tabled = dret_b;

proc sort data = &tabled; by weekday;quit;
proc means data = &tabled(where = (year(date) > 2002)) noprint;
	by weekday;
	var ret;
	output out = day mean = m t = tstat;
quit;

data day1;
set day;
	m = m*100;
	m = round(m,0.001);
	r = catt(m,"%(", round(tstat,0.01), ")");
	keep weekday r;
run;

PROC ANOVA DATA=&tabled;
	CLASS weekday;
	MODEL ret = weekday;
	RUN; 
quit;

/* Regression */

proc sql;
	create table dsf 
	as select a.*, b.compname as compnameA, c.compname as compnameB
	from draft.dsf as a left join china.ab_cross as b
	on a.stkcd = b.stkcdA
	left join china.ab_cross as c
	on a.stkcd = c.stkcdB;
quit;

data dsf;
	set dsf;
	B = 0;
	if compnameB ne "" then B = 1;
	mon = 0;
	if weekday(date) - 1 = 1 then mon = 1;
	tue = 0;
	if weekday(date) - 1 = 2 then tue = 1;
	wed = 0;
	if weekday(date) - 1 = 3 then wed = 1;
	thu = 0;
	if weekday(date) - 1 = 4 then thu = 1;
	fri = 0;
	if weekday(date) - 1 = 5 then fri = 1;
	early1 = 0;
	if weekday(date) - 1 in (1,2) then early1 = 1;
	early2 = 0;
	if weekday(date) - 1 in (1,2,3) then early2 = 1;
	late1 = 0;
	if weekday(date) - 1 in (4,5) then late1 = 1;

	mon_b = mon*b;
	tue_b = tue*b;
	wed_b = wed*b;
	thu_b = thu*b;
	fri_b = fri*b;
	early1_b = early1*b;
	early2_b = early2*b;
	late1_b = late1*b;
	if year(date) > 2005;
	week = intck("week","01jan1999"d,date);
run; 

data dsf1;
	set dsf;
	if compnameB ne "" or compnameA ne "";
run;

%let tablein = dsf;
%let tablein = dsf1;

proc sort data = &tablein; by week; quit;
proc reg data = &tablein(where = (substr(stkcd,1,3) not in ("200","900"))) outest = est_da noprint;
	by week;
	model ret = tue wed thu fri;
quit;

proc reg data = &tablein(where = (substr(stkcd,1,3) in ("200","900"))) outest = est_db noprint;
	by week;
	model ret = tue wed thu fri;
quit;


proc sort data = &tablein; by week; quit;
proc reg data = &tablein outest = est_dall noprint;
	by week;
	model ret = early1 late1 early1_b late1_b;
quit;

proc means data = est_dall mean t ;quit;

%let lag = 5;
%let list = mon tue wed thu fri;
%macro newey_west_d(table = ,coefout= ,tstatout = );
data &coefout; run;
data &tstatout; run;

data temptable;
	set &table;
	rename intercept = mon;
	tue = intercept + tue;
	web = intercept + web;
	thu = intercept + thu;
	fri = intercept + fri;
run;

%do i = 1 %to %sysfunc(countw(&list));
%let depvar = %scan(&list,&i);
proc model data=temptable;
         endo &depvar; 
         instruments / intonly;
         parms b0;
         &depvar =b0;
         fit &depvar / gmm kernel=(bart,%eval(&lag + 1),0) vardef=n;
		 ods output parameterestimates = temp;
quit; 

data temp2 (rename = (estimate = alpha));
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	Estimate = Estimate;
	keep model dependent Estimate;
run;

data &coefout;
	set &coefout temp2;
	if model ne "";
run;

data temp2;
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	tvalue2 = strip(put(tvalue,10.2));
	keep model dependent tvalue2;
run;

data &tstatout;
	set &tstatout temp2;
	if model ne "";
run;
%end;
%mend;
%newey_west_d(table = est_da, coefout = coef_da, tstatout = tstat_da);
%newey_west_d(table = est_db, coefout = coef_db, tstatout = tstat_db);


%let lag = 5;
%let list = early1_b late1_b early1 late1;
%macro newey_west_d(table = ,coefout= ,tstatout = );
data &coefout; run;
data &tstatout; run;

data temptable;
	set &table;
	rename intercept = wed;
	early1 = intercept + early1;
	late1 = intercept + late1;
run;

%do i = 1 %to %sysfunc(countw(&list));
%let depvar = %scan(&list,&i);
proc model data=temptable;
         endo &depvar; 
         instruments / intonly;
         parms b0;
         &depvar =b0;
         fit &depvar / gmm kernel=(bart,%eval(&lag + 1),0) vardef=n;
		 ods output parameterestimates = temp;
quit; 

data temp2 (rename = (estimate = alpha));
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	Estimate = Estimate;
	keep model dependent Estimate;
run;

data &coefout;
	set &coefout temp2;
	if model ne "";
run;

data temp2;
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	tvalue2 = strip(put(tvalue,10.2));
	keep model dependent tvalue2;
run;

data &tstatout;
	set &tstatout temp2;
	if model ne "";
run;
%end;
%mend;

%newey_west_d(table = est_dall, coefout = coef_dall, tstatout = tstat_dall);

/************************intraday return***************************/

data pret_a;
	set draft.isf;	
	if substr(stkcd,1,3) not in ("200","900");
run;

data pret_b;
	set draft.isf;	
	if substr(stkcd,1,3) in ("200","900");
run;

%let tablep = pret_a;
%let tablep = pret_b;
proc sort data = &tablep; by period;quit;
proc means data = &tablep noprint;
	by period;
	var ret;
	output out = period mean = m t = tstat;
quit;

data period1;
set period;
	m = m*100;
	m = round(m,0.001);
	r = catt(m,"%(", round(tstat,0.01), ")");
	keep period r;
run;

PROC ANOVA DATA=&tablep;
	CLASS period;
	MODEL ret = period;
	RUN; 
quit;

/* Regression */

proc sql;
	create table isf 
	as select a.*, b.compname as compnameA, c.compname as compnameB
	from draft.isf as a left join china.ab_cross as b
	on a.stkcd = b.stkcdA
	left join china.ab_cross as c
	on a.stkcd = c.stkcdB;
quit;

data isf;
	set isf;
	B = 0;
	if compnameB ne "" then B = 1;
	p0 = 0;
	if period = 0 then p0 = 1;
	p1 = 0;
	if period = 1 then p1 = 1;
	p2 = 0;
	if period = 2 then p2 = 1;
	p3 = 0;
	if period = 3 then p3 = 1;
	p4 = 0;
	if period = 4 then p4 = 1;
	p4_5 = 0;
	if period = 4.5 then p4_5 = 1;
	p5 = 0;
	if period = 5 then p5 = 1;
	p6 = 0;
	if period = 6 then p6 = 1;
	p7 = 0;
	if period = 7 then p7 = 1;
	p8 = 0;
	if period = 8 then p8 = 1;
	p0_b = p0*b;
	p1_b = p1*b;
	p5_b = p5*b;
	p8_b = p8*b;
run; 

data isf1;
	set isf;
	if compnameB ne "" or compnameA ne "";
run;

/*
proc reg data = isf1(where = (substr(stkcd,1,3) not in ("200","900"))) outest = est_a tableout noprint;
	model ret = p0 p1 p8;
quit;

proc reg data = isf1(where = (substr(stkcd,1,3) in ("200","900"))) outest = est_b tableout noprint;
	model ret = p0 p1 p8;
quit;

proc reg data = isf1 outest = est_all tableout noprint;
	model ret = p0 p1 p8 p0_b p1_b p8_b;
quit;*/


%let tablein = isf;
%let tablein = isf1;

proc sort data = &tablein; by date; quit;
proc reg data = &tablein(where = (substr(stkcd,1,3) not in ("200","900"))) outest = est_ia noprint;
	by date;
	model ret = p1 p2 p3 p4 p4_5 p5 p6 p7 p8;
quit;

proc reg data = &tablein(where = (substr(stkcd,1,3) in ("200","900"))) outest = est_ib noprint;
	by date;
	model ret = p1 p2 p3 p4 p4_5 p5 p6 p7 p8;
quit;


proc sort data = &tablein; by date; quit;
proc reg data = &tablein outest = est_iall tableout noprint;
	by date;
	model ret = p0 p1 p3 p4 p4_5 p5 p6 p7 p8 p0_b p1_b;
quit;

proc means data = est_iall mean t; quit; 

%let lag = 10;
%let list = p0_b p1_b p0 p1;
%macro newey_west_i(table = ,coefout= ,tstatout = );
data &coefout; run;
data &tstatout; run;

data temptable;
	set &table;
	rename intercept = p2;
	p0 = intercept + p0;
	p1 = intercept + p1;
run;

%do i = 1 %to %sysfunc(countw(&list));
%let depvar = %scan(&list,&i);
proc model data=temptable;
         endo &depvar; 
         instruments / intonly;
         parms b0;
         &depvar =b0;
         fit &depvar / gmm kernel=(bart,%eval(&lag + 1),0) vardef=n;
		 ods output parameterestimates = temp;
quit; 

data temp2 (rename = (estimate = alpha));
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	Estimate = Estimate;
	keep model dependent Estimate;
run;

data &coefout;
	set &coefout temp2;
	if model ne "";
run;

data temp2;
	retain model dependent;
	set temp;
	if parameter = 'b0';
	model = 'Avg';
	dependent = "&depvar.";
	tvalue2 = strip(put(tvalue,10.2));
	keep model dependent tvalue2;
run;

data &tstatout;
	set &tstatout temp2;
	if model ne "";
run;
%end;
%mend;
%newey_west_i(table = est_iall, coefout = coef_iall, tstatout = tstat_iall);

