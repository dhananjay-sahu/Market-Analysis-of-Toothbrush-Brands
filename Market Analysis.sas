/*Importing prod_tooth.xls file and sorting by UPC*/
PROC IMPORT DATAFILE = 'h:\prod_tooth.xls' DBMS = XLS OUT = prod;
GETNAMES = YES;
RUN;

/*L5 L9 UPC BRISTLE SIZE USER_INFO TYPE_OF_BRUSH SHAPE*/
DATA prod (KEEP = L5 UPC);
SET prod;
RUN;

PROC PRINT DATA = prod (obs=10);
RUN;

PROC SORT DATA = prod;
BY UPC;
RUN;

/*Reading toothbr_groc_1114_1165 file, combining SY, GE, VEND and ITEM to get the UPC and sorting by UPC*/
DATA groc (drop = groc_SY groc_GE groc_VEND groc_ITEM padded_SY padded_GE padded_VEND padded_ITEM upc1 F D);
INFILE 'h:\toothbr_groc_1114_1165' firstobs=2;
INPUT IRI_KEY WEEK groc_SY groc_GE groc_VEND groc_ITEM UNITS DOLLARS F $ D PR;
padded_SY=put(groc_SY,z2.);
padded_GE=put(groc_GE,z2.);
padded_VEND=put(groc_VEND,z5.);
padded_ITEM=put(groc_ITEM,z5.);
UPC = catx('-', padded_SY, padded_GE, padded_VEND, padded_ITEM);
upc1 = cat(TRIM(LEFT(groc_SY)), TRIM(LEFT(groc_GE)), padded_VEND, padded_ITEM);
COLUPC = input(upc1,best15.);
IF F = 'A' THEN F_A = 1; ELSE F_A = 0;
IF F = 'A+' THEN F_APlus = 1; ELSE F_APlus = 0;
IF F = 'B' THEN F_B = 1; ELSE F_B = 0;
IF F = 'C' THEN F_C = 1; ELSE F_C = 0;
IF D = 1 THEN D_Minor = 1; ELSE D_Minor = 0;
IF D = 2 THEN D_Major = 1; ELSE D_Major = 0;
RUN;

PROC PRINT DATA = groc (obs=10);
RUN;

PROC SORT DATA = groc;
BY UPC;
RUN;

/*Merging the prod and groc files and sorting by IRI_KEY*/
DATA prod_groc;
MERGE prod (IN = In1) groc (IN = In2);
BY UPC;
IF In1 & In2;
RUN;

/*Top 6 Brands*/
PROC SQL;
CREATE TABLE A AS
  SELECT L5 AS A_L5, SUM(DOLLARS) AS total
    FROM prod_groc
      GROUP BY L5
        ORDER BY total descending
  ;
QUIT;

PROC SQL OUTOBS=6;                                                                                          
SELECT *,(total/SUM(total)) AS Percent format=6.2      
FROM A;
QUIT;

/*Reading PANEL_GR file*/
DATA panel_gr (DROP = OUTLET);
INFILE 'h:\toothbr_PANEL_GR_1114_1165.dat' firstobs=2;
INPUT PANID	WEEK UNITS OUTLET $ DOLLARS IRI_KEY COLUPC;
RENAME DOLLARS = Panel_Dollars;
RENAME UNITS = Panel_Units;
RUN;

/*Merging PANEL_GR with Prod_groc*/
PROC SQL;
CREATE TABLE merged AS
SELECT *
FROM prod_groc, panel_gr
WHERE prod_groc.COLUPC = panel_gr.COLUPC
AND prod_groc.week = panel_gr.week
AND prod_groc.IRI_KEY = panel_gr.IRI_KEY;
QUIT;

PROC SQL;
CREATE TABLE A AS
  SELECT L5 AS A_L5, SUM(DOLLARS) AS total
    FROM merged
      GROUP BY L5
        ORDER BY total descending
  ;
QUIT;

PROC SQL OUTOBS=6;                                                                                          
SELECT *,(total/SUM(total)) AS Percent format=6.2      
FROM A;
QUIT;

/*Reading Demographic file*/
PROC IMPORT DATAFILE = 'h:\demoup.csv' DBMS = CSV OUT = demo;
GETNAMES = YES;
RUN;

DATA demo (DROP = VAR1);
SET demo;
RENAME Panelist_ID = PANID;
RUN;

/*PROC PANEL*/
DATA top_brand (keep = UPC IRI_KEY Week Units Dollars PR F_A F_APlus F_B F_C D_Minor D_Major);
SET prod_groc;
IF L5 = 'ORAL B ADVANTAGE';
RUN;

PROC SQL;
CREATE TABLE total_UPC AS
SELECT UPC, SUM(Units) AS totalByUPC
FROM top_brand
GROUP BY UPC;
QUIT;

PROC SQL;
CREATE TABLE weights AS
SELECT UPC, totalByUPC/SUM(totalByUPC) AS wgt
FROM total_UPC
;
QUIT;

PROC SQL;
CREATE TABLE reg_data AS
  SELECT IRI_KEY, WEEK, Dollars, (Dollars/Units)*wgt AS wgt_PricePerUnit, PR*wgt AS wgt_Pr, F_A*wgt AS wgt_F_A, F_APlus*wgt AS wgt_F_APlus, 
		  F_B*wgt AS wgt_F_B, F_C*wgt AS wgt_F_C, D_Minor*wgt AS wgt_D_Minor, D_Major*wgt AS wgt_D_Major
    FROM top_brand, weights
	  WHERE top_brand.UPC = weights.UPC;
QUIT;

PROC SQL;
CREATE TABLE panel_data AS
	SELECT IRI_KEY, WEEK, SUM(Dollars) AS WeeklySales, AVG(wgt_PricePerUnit) AS avg_wgt_PricePerUnit, AVG(wgt_Pr) AS avg_wgt_Pr,
		   AVG(wgt_F_A) AS avg_wgt_F_A, AVG(wgt_F_APlus) AS avg_wgt_F_APlus, AVG(wgt_F_B) AS avg_wgt_F_B, 
		   AVG(wgt_F_C) AS avg_wgt_F_C, AVG(wgt_D_Minor) AS avg_wgt_D_Minor, AVG(wgt_D_Major) AS avg_wgt_D_Major
	FROM reg_data
	GROUP BY IRI_KEY, Week;
QUIT;

DATA final_panel_data;
SET panel_data;
Price_F_A = avg_wgt_PricePerUnit * avg_wgt_F_A;
Price_F_B = avg_wgt_PricePerUnit * avg_wgt_F_B;
Price_F_C = avg_wgt_PricePerUnit * avg_wgt_F_C;
Price_D_Minor = avg_wgt_PricePerUnit * avg_wgt_D_Minor;
Price_D_Major = avg_wgt_PricePerUnit * avg_wgt_D_Major;
RUN;

PROC PRINT DATA = final_panel_data (obs = 10);
RUN;

/*To run the Hausman test*/
PROC PANEL DATA = final_panel_data;
ID IRI_KEY Week;
MODEL WeeklySales = avg_wgt_PricePerUnit avg_wgt_Pr avg_wgt_F_A avg_wgt_F_APlus avg_wgt_F_B avg_wgt_F_C avg_wgt_D_Minor 
					avg_wgt_D_Major Price_F_A Price_F_B Price_F_C Price_D_Minor Price_D_Major 
					/ ranone;
RUN;

/*Since Hausman test rejects the NULL hypothesis, run only fixed effects model.*/
PROC PANEL DATA = final_panel_data;
ID IRI_KEY Week;
MODEL WeeklySales = avg_wgt_PricePerUnit avg_wgt_Pr avg_wgt_F_A avg_wgt_F_APlus avg_wgt_F_B avg_wgt_F_C avg_wgt_D_Minor 
					avg_wgt_D_Major Price_F_A Price_F_B Price_F_C Price_D_Minor Price_D_Major 
					/ fixone fixtwo;
RUN;

/*RFM*/
PROC SQL;
CREATE TABLE RFM_data AS
	SELECT PANID, max(WEEK) AS Recency, count(*) AS Frequency, AVG(Panel_Dollars) AS Monetary
	FROM merged
	WHERE L5 = 'ORAL B ADVANTAGE'
	GROUP BY PANID;
QUIT;

PROC CORR DATA = RFM_data;
VAR Recency Frequency Monetary;
RUN;

PROC RANK DATA = RFM_data OUT = rank_R TIES = low GROUPS = 5;
VAR Recency;
RANKS R;
RUN;

PROC RANK DATA = RFM_data OUT = rank_F TIES = low GROUPS = 5;
VAR Frequency;
RANKS F;
RUN;

PROC RANK DATA = RFM_data OUT = rank_M TIES = low GROUPS = 5;
VAR Monetary;
RANKS M;
RUN;

PROC SQL;
	CREATE TABLE rank AS
	SELECT * 
	FROM rank_R, rank_F, rank_M
	WHERE rank_R.PANID = rank_F.PANID
	AND rank_R.PANID = rank_M.PANID
	;
QUIT;

DATA RFM (DROP = R F M);
set rank;
R+1;
F+1;
M+1;
RFM = cats(of R F M) + 0;
run;

PROC CLUSTER DATA = RFM METHOD=average ccc pseudo outtree=RFM_Clus_Tree;
VAR RFM;
ID PANID;
run;

PROC FASTCLUS DATA = RFM MAXC = 3 MAXITER = 10 OUT = clus;
VAR RFM;
ID PANID;
RUN;

PROC SORT DATA = clus;
BY PANID;
RUN;

PROC SORT DATA = demo;
BY PANID;
RUN;

DATA clus_demo;
MERGE clus (IN = In1) demo;
BY PANID;
IF In1;
RUN;

PROC SQL;
	CREATE TABLE RFM_Demo_Cluster AS
	SELECT * 
	FROM clus_demo
	ORDER BY Cluster;
QUIT;

/*PROC MDC*/
DATA brand_num (DROP = F_A F_APlus F_B F_C D_Minor D_Major Units Dollars Pr);
SET merged;
IF L5 = 'MENTADENT PROCARE' THEN Choice = 1;
ELSE IF L5 = 'ORAL B ADVANTAGE' THEN Choice = 2;
ELSE IF L5 = 'BUTLER PROTECT' THEN Choice = 3;
ELSE IF L5 = 'ORAL B INDICATOR' THEN Choice = 4;
/*Feature = F_A + F_APlus + F_B + F_C;
Display = D_Minor + D_Major;*/
RUN;

DATA brand_num;
SET brand_num;
WHERE Choice ne .;
RUN;

DATA mdc_prod_groc (DROP = F_A F_APlus F_B F_C D_Minor D_Major);
SET prod_groc;
Feature = F_A + F_APlus + F_B + F_C;
Display = D_Minor + D_Major;
RUN;

PROC SQL;
	CREATE TABLE subset_prod_groc AS
	SELECT *
	FROM mdc_prod_groc
	WHERE UPC IN (SELECT DISTINCT(UPC) from brand_num)
	;
QUIT;

PROC SQL;
	CREATE TABLE  subset2 AS
	SELECT *
	FROM subset_prod_groc
	WHERE IRI_KEY IN (SELECT DISTINCT(IRI_KEY) from brand_num)
	;
QUIT;

PROC SQL;
	CREATE TABLE mdc_total_UPC AS
	SELECT L5, IRI_KEY, Week, SUM(Units) AS totalByUPC
	FROM subset2
	GROUP BY L5, IRI_KEY, Week;
QUIT;

PROC SQL;
	CREATE TABLE mdc_weights AS
	SELECT mdc_total_UPC.L5, UPC, mdc_total_UPC.IRI_KEY, mdc_total_UPC.Week, Units/totalByUPC AS wgt
	FROM mdc_total_UPC, subset2
	WHERE mdc_total_UPC.L5 = subset2.L5
	AND mdc_total_UPC.IRI_KEY = subset2.IRI_KEY
	AND mdc_total_UPC.Week = subset2.Week
	;
QUIT;

PROC SQL;
CREATE TABLE mdc_data AS
  SELECT subset2.L5, subset2.IRI_KEY, subset2.WEEK, (Dollars/Units)*wgt AS wgt_PricePerUnit, PR*wgt AS wgt_Pr, Feature*wgt AS wgt_F, 
		 Display*wgt AS wgt_D
    FROM subset2, mdc_weights
	  WHERE subset2.L5 = mdc_weights.L5
	  AND subset2.UPC = mdc_weights.UPC
	  AND subset2.IRI_KEY = mdc_weights.IRI_KEY
	  AND subset2.Week = mdc_weights.Week;
QUIT;

PROC SQL;
CREATE TABLE mdc_groc_wt AS
	SELECT L5, IRI_KEY, WEEK, AVG(wgt_PricePerUnit) AS avg_wgt_PricePerUnit, AVG(wgt_Pr) AS avg_wgt_Pr,
		   AVG(wgt_F) AS avg_wgt_F, AVG(wgt_D) AS avg_wgt_D
	FROM mdc_data
	GROUP BY l5, IRI_KEY, Week;
QUIT;

PROC SORT DATA = mdc_groc_wt;
BY L5 IRI_KEY Week;
RUN;

DATA mdc_groc_wt (Drop = L5);
SET mdc_groc_wt;
IF L5 = 'MENTADENT PROCARE' THEN Choice = 1;
ELSE IF L5 = 'ORAL B ADVANTAGE' THEN Choice = 2;
ELSE IF L5 = 'BUTLER PROTECT' THEN Choice = 3;
ELSE IF L5 = 'ORAL B INDICATOR' THEN Choice = 4;
RUN;

PROC SORT DATA = mdc_groc_wt;
BY IRI_KEY Week;
RUN;

proc transpose data=mdc_groc_wt out=wide_price prefix=P;
    by IRI_KEY Week;
    id Choice;
    var avg_wgt_PricePerUnit;
run;

proc transpose data=mdc_groc_wt out=wide_pr prefix=Pr;
    by IRI_KEY Week;
    id Choice;
    var avg_wgt_Pr;
run;

proc transpose data=mdc_groc_wt out=wide_F prefix=F;
    by IRI_KEY Week;
    id Choice;
    var avg_wgt_F;
run;

proc transpose data=mdc_groc_wt out=wide_D prefix=D;
    by IRI_KEY Week;
    id Choice;
    var avg_wgt_D;
run;

DATA mdc_groc_final;
MERGE wide_Price wide_Pr wide_F wide_D;
BY IRI_KEY Week;
RUN;

PROC SORT DATA = brand_num;
BY IRI_KEY Week;
RUN;

DATA mdc (DROP = L5 COLUPC _NAME_);
MERGE brand_num (IN = in1) mdc_groc_final;
BY IRI_KEY Week;
IF in1;
RUN;

PROC SQL;
	CREATE TABLE mdc_panel_UPC AS
	SELECT Choice, IRI_KEY, Week, SUM(Panel_Units) AS totalByUPC
	FROM mdc
	GROUP BY Choice, IRI_KEY, Week;
QUIT;

PROC SQL;
	CREATE TABLE mdc_panel_weights AS
	SELECT mdc_panel_UPC.Choice, UPC, mdc_panel_UPC.IRI_KEY, mdc_panel_UPC.Week, Panel_Units/totalByUPC AS wgt
	FROM mdc_panel_UPC, mdc
	WHERE mdc_panel_UPC.Choice = mdc.Choice
	AND mdc_panel_UPC.IRI_KEY = mdc.IRI_KEY
	AND mdc_panel_UPC.Week = mdc.Week
	;
QUIT;

PROC SQL;
CREATE TABLE mdc_final AS
  SELECT mdc.*, (Panel_Dollars/Panel_Units)*wgt AS Panel_Price
    FROM mdc, mdc_panel_weights
	  WHERE mdc.UPC = mdc_panel_weights.UPC
	  AND mdc.Choice = mdc_panel_weights.Choice
	  AND mdc.IRI_KEY = mdc_panel_weights.IRI_KEY
	  AND mdc.Week = mdc_panel_weights.Week;
QUIT;

PROC SQL;
CREATE TABLE PanelPrice AS
	SELECT IRI_KEY, WEEK, AVG(Panel_Price) AS avg_wgt_PanelPrice
	FROM mdc_final
	GROUP BY IRI_KEY, Week;
QUIT;

DATA FINAL;
MERGE mdc_final PanelPrice;
BY IRI_KEY Week;
RUN;

DATA FINAL (DROP = UPC Panel_Units Panel_Dollars Panel_Price avg_wgt_PanelPrice);
SET FINAL;
IF Choice = 1 THEN P1 = avg_wgt_PanelPrice;
ELSE IF Choice = 2 THEN P2 = avg_wgt_PanelPrice;
ELSE IF Choice = 3 THEN P3 = avg_wgt_PanelPrice;
ELSE IF Choice = 4 THEN P4 = avg_wgt_PanelPrice;
RUN;

data FINAL;
set FINAL;
array change _numeric_;
do over change;
	if change=. then change=0;
end;
run;

DATA demo_subset (KEEP = PANID Income Family_Size);
SET demo;
RENAME Combined_Pre_Tax_Income_of_HH = Income;
RUN;

PROC SORT DATA = demo_subset;
BY PANID;
RUN;

PROC SORT DATA = FINAL;
BY PANID;
RUN;

DATA final_data;
MERGE FINAL (IN = in1) demo_subset;
BY PANID;
IF in1;
RUN;

data newdata(keep = pid decision mode c1 c2 c3 p pr d f inc1 inc2 inc3 fam1 fam2 fam3); 
   set final_data; 
   array pvec{4} p1 - p4; 
   array prvec{4} pr1 - pr4;
   array dvec{4} d1 - d4; 
   array fvec{4} f1 - f4; 

   array s1{4}(1 0 0 0); 
   array s2{4}(0 1 0 0);
   array s3{4}(0 0 1 0);

   retain pid 0; 
   pid + 1; 
   
   do i = 1 to 4; 
      mode = i; 
      p = pvec{i}; 
	  pr = prvec{i};
	  d = dvec{i}; 
	  f = fvec{i};

	  c1 = s1{i}; /*c1 is the intercept for brand1*/
	  c2 = s2{i};
	  c3 = s3{i};

	  inc1 = c1 * income;
	  inc2 = c2 * income;
	  inc3 = c3 * income;

	  fam1 = c1 * family_size;
	  fam2 = c2 * family_size;
	  fam3 = c3 * family_size;
	  
      decision = ( Choice = i ); 
      output; 
   end; 
run; 
 
 
proc print data=newdata(obs=9); 
run; 
 
proc mdc data=newdata; 
   model decision = c1 c2 c3 p pr d f inc1 inc2 inc3 fam1 fam2 fam3 / 
            type=clogit 
            nchoice=4 
            optmethod=qn 
            covest=hess; 
   id pid; 
   output out=probdata pred=prob;
run; 

data probdata1;
set probdata;
own_price_elasticity = ((1-prob)*(-10.9139)*p);
cross_elasticity = ((-prob)*(-10.9139)*p);
run;

proc means data=probdata1 mean; 
var own_price_elasticity cross_elasticity; 
Class mode;
run;
