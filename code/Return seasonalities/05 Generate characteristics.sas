/* Generate characteristics for both type of stocks */

/* Monthly */
data msf_a; 
	set draft.monthly_lags_a;
	keep stkcd date ym ret same_1_120 other_13_120 dif_1_120;
run; 

data chars;
	set china.selected_chars;
	ym = year(date)*12 + month(date);
	keep stkcd date ym me bmq roe roa tur tv ivff;
run;

proc sql;
	create table chars1
	as select a.*, b.* , c.pps, c.isff, d.*, e.totalproportion as io
	from msf_a as a left join chars as b
	on a.stkcd = b.stkcd and a.ym = b.ym
	left join china.trading_frictions as c 
	on a.stkcd = c.stkcd and a.ym = c.ym
	left join cdraft.loa_m as d
	on a.stkcd = d.stkcd and a.ym = d.ym
    left join china.io_aggregate as e
    on a.stkcd = e.stkcd and intnx("qtr",a.date,-1,"E") = intnx("qtr",e.date,0,"E");
quit;

data msf_b; 
	set draft.monthly_lags_b;
	keep stkcd date ym ret same_1_120 other_13_120 dif_1_120;
run; 

proc sql;
	create table chars2
	as select a.*, b.*, d.*, e.totalproportion as io
	from msf_b as a left join china.b_char(rename = (lnme = me)) as b
	on a.stkcd = b.stkcd and a.ym = b.ym 
	left join cdraft.loa_m as d
	on a.stkcd = d.stkcd and a.ym = d.ym
    left join china.io_aggregate as e
    on a.stkcd = e.stkcd and intnx("qtr",a.date,-1,"E") = intnx("qtr",e.date,0,"E");
quit;

data chars2;
set chars1 chars2;
run;

proc sql;
	create table chars3
	as select distinct a.*, exp(sum(log(b.ret+1))) - 1 as ret_2_12
	from chars2 as a left join china.msf as b 
	on a.stkcd = b.stkcd and 2 <= a.ym - b.ym <= 12
	group by a.stkcd, a.ym
	order by a.stkcd, a.ym;
quit;
  
proc sql;
	create table chars4
	as select distinct a.*, exp(sum(log(b.ret+1))) - 1 as ret_13_60
	from chars3 as a left join china.msf as b 
	on a.stkcd = b.stkcd and 13 <= a.ym - b.ym <= 60
	group by a.stkcd, a.ym;
quit;
  
proc sql;
	create table chars5
	as select distinct a.*, b.ret as ret_1
	from chars4 as a left join china.msf as b
	on a.stkcd = b.stkcd and a.ym - b.ym = 1;
quit;

/*Merge Btm, ROA, ROE seperately */
proc sql;
	create table chars6
	as select distinct a.*, b.F050201B as roa1, b.F050202B as roa2, b.F050203B as roa3,
							b.F050501B as roe1, b.F050502B as roe2, b.F050503B as roe3,
							c.F101001A as bmq1, c.F101002A as bmq2, log(c.F100801A) as me1,
							log(c.F100802A) as me2
	from chars5 as a left join china.roa (where = (Typrep = "A")) as b
	on a.stkcd = b.stkcd and intnx("qtr",a.date,-1,"E") = intnx("qtr",b.date,0,"E")
	left join china.btm as c
	on a.stkcd = c.stkcd and intnx("qtr",a.date,-1,"E") = intnx("qtr",c.date,0,"E");
quit;

/* For dual - share firms, use the same ME BtM ROA ROE */
proc sql;
	create table a_dual(where = (compnameA ne "") )
	as select a.*, b.compname as compnameA
	from chars6 as a left join china.ab_cross as b
	on a.stkcd = b.stkcdA;
quit;

proc sql;
	create table chars7
	as select distinct a.*, c.compname as compnameB
	from chars6 as a left join china.ab_cross as c
	on a.stkcd = c.stkcdb;
quit;

proc sql;
	create table chars8
	as select distinct a.*, b.me1 as me1_a, b.bmq1 as bmq1_a, b.roa1 as roa1_a, b.roe1 as roe1_a
	from chars7 as a left join a_dual as b
	on a.compnameB = b.compnameA and a.ym = b.ym;
quit;

data chars9;
	set chars8;
	if compnameB ne "" then do;
	me1 = me1_a;
	bmq1 = bmq1_a;
	roa1 = roa1_a;
	roe1 = roe1_a;
	end;
	drop me1_a bmq1_a roa1_a roe1_a compnameA compnameB;
run;

data draft.monthly_chars;set chars9; run;
 
data test; set chars2; if compnameB ne "";run;

data test; set draft.monthly_chars;run;
proc sort data = test nodupkey; by stkcd ym;quit;

/* Daily */
/*Need dsf lags*/
data dsf;
	set cdraft.daily_lags
		cdraft.daily_lags_b;
	ym = year(date)*12 + month(date);
	keep stkcd date ym ret same_1_120 other_13_120 dif_1_120;
run;

proc sql;
	create table draft.daily_chars
	as select distinct a.*, b.*
	from dsf as a left join draft.monthly_chars as b
	on a.stkcd = b.stkcd and a.ym = b.ym;
quit;

/* Intraday */
data isf;
	set cdraft.intraday_lags
		cdraft.intraday_lags_b;
	ym = year(date)*12 + month(date);
	keep stkcd date period ym ret same_1_120 other_13_120 dif_1_120;
run;

data monthly_chars;
	set draft.monthly_chars;
	keep stkcd ym me bmq bmq1 bmq2 roa roa1 roa2 roa3 roe roe1 roe2 roe3 nan io tur tv ivff pps;
run;

proc sql;
	create table intraday_chars
	as select distinct a.*, b.*
	from isf as a left join monthly_chars as b
	on a.stkcd = b.stkcd and a.ym = b.ym;
quit;

data draft.intraday_chars; set intraday_chars;run;

/*Descriptive analysis*/
proc sql;
	create table msf 
	as select a.*, b.compname as compnameA, c.compname as compnameB, d.*
	from draft.monthly_chars as a left join china.ab_cross as b
	on a.stkcd = b.stkcdA
	left join china.ab_cross as c
	on a.stkcd = c.stkcdB
	left join draft.monthly_chars as d
	on a.stkcd = d.stkcd and a.ym = d.ym;
quit;

data a;
	set msf;
	if substr(stkcd,1,3) not in ("200","900");
	keep stkcd ym me me1 bmq roa roe bmq1 roa1 roe1 tv ivff pps;
run;

data a_dual;
	set msf;
	if compnameA ne "";
	keep stkcd ym me me1 bmq roa roe bmq1 roa1 roe1 tv ivff pps;
run;

data b;
	set msf;
	if substr(stkcd,1,3) in ("200","900");
	keep stkcd ym me me1 bmq roa roe bmq1 roa1 roe1 tv ivff pps;
run;

data b_dual;
	set msf;
	if compnameB ne "";
	keep stkcd ym me me1 bmq roa roe bmq1 roa1 roe1 tv ivff pps;
run;

proc means data = a mean p50; var me1 bmq1 roa1 roe1 tv ivff pps;quit;
proc means data = b mean p50; var me1 bmq1 roa1 roe1 tv ivff pps;quit;
proc means data = a_dual mean p50; var me1 bmq1 roa1 roe1 tv ivff pps;quit;
proc means data = b_dual mean p50; var me1 bmq1 roa1 roe1 tv ivff pps;quit;
