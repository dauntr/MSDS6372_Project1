proc import datafile="/home/sarellano0/dataSets/hybrid_reg.csv"
          dbms=dlm out=Hybrid replace;
     delimeter=',';
     getnames=yes;
     
run;

/* accelrate ~ carid vehicle year msrp mpg mpgmpge carclass carclass_id */

/* bunch of data transformations */
data Hybrid2;
set Hybrid;
logaccel = log(accelrate);
logMPG = log(mpg);
logYear = log(year);
GP100 = 100/mpg;
logGP100 = log(100/mpg);
logMSRP = log(msrp);
sqrtMPG = sqrt(mpg);
sqrtMSRP = sqrt(msrp);
MPG2 = (mpg*mpg);
MSRP2 = (msrp*msrp);
run; 


/* EDA of Statisitical Significance of year as Categorical Variable */
proc GLM data=Hybrid2;
	class year;
	model accelrate=year;
	run;

/*EDA of Statisitical Significance of carclass_id as Categorical Variable */
proc GLM data=Hybrid2;
	class carclass_id;
	model accelrate=carclass_id; 
	run;

/* EDA of Continous Variables */
proc sgscatter data=Hybrid2;
  title "Hybrid Car Data";
  matrix accelrate carid year msrp mpg mpgmpge / group=carclass diagonal=(histogram kernel);
run;
  title;

/* EDA Correlation of Continous Variables*/
ods graphics on;
title 'Hybrid Car Data';
proc corr data=Hybrid2 nomiss plots=matrix(histogram);
   var accelrate year msrp mpg mpgmpge;
 run;
ods graphics off;

/* EDA Correlation of MGP Transformations*/
ods graphics on;
title 'Hybrid Car Data';
proc corr data=Hybrid2 nomiss plots=matrix(histogram);
   var accelrate mpg logmpg sqrtMPG MPG2 ;
 run;
ods graphics off;

/* EDA Correlation of MGP Transformations on logaccel*/
ods graphics on;
title 'Hybrid Car Data';
proc corr data=Hybrid2 nomiss plots=matrix(histogram);
   var logaccel mpg logmpg sqrtMPG MPG2 ;
 run;
ods graphics off;

/* EDA examining data distribution of mpg */
proc univariate data = Hybrid2;
var mpg;
histogram;
run;

/* Linear Modeling Assumptions Continous Variables with VIF */
proc reg data = Hybrid2 corr plots(label) = all;
	model accelrate = year msrp mpg mpgmpge/ VIF /*CLB*/;  
	title 'Hybrid Car Data';
	run; quit;

/* Linear Modeling Assumptions All Variables */
proc glm data = Hybrid2 plots=all;
  	class carclass_id;
  	model accelrate = year msrp mpg mpgmpge carclass_id; 
	run; quit;

/* Model Selection simple glmselect with no interaction variables Stepwise no CV*/	
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id / selection =STEPWISE;
	run; quit; /* year msrp mpgmpge 
				RMSE 1.968 Adj R-Sq	0.552 AICC 366.474 SBC 223.187 */

/* Model Selection simple glmselect with no interaction variables LARS no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id / selection =LAR;
	run; quit; /* msrp 
				RMSE	2.286 Adj R-Sq	0.395 AICC 410.205 SBC 261.105 */
	
/* Model Selection simple glmselect with no interaction variables LASSO no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = year msrp mpg mpgmpge carclass_id / selection =LASSO;
	run; quit; /* msrp 
				RMSE	2.286 Adj R-Sq	0.395 AICC 410.205 SBC 261.105 */

/* Model Selection simple glmselect with no interaction variables ELASTICNET no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = year msrp mpg mpgmpge carclass_id / selection =ELASTICNET;
	run; quit; /* year msrp mpg mpgmpge carclass_id_1 carclass_id_2 carclass_id_3 carclass_id_4 carclass_id_5 
				RMSE 1.961 Adj R-Sq	.550 AICC 372.661 SBC 246.093 */
			

/* Model Selection simple glmselect with Interaction variables Stepwise no CV*/	
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection =STEPWISE;
	run; quit; /* msrp mpgmpge year*msrp 
				RMSE 1.950 Adj R-Sq 0.569 AICC 363.652 SBC 220.366 */

/* Model Selection simple glmselect with Interaction variables LAR no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection =LAR;
	run; quit; /* year*msrp 
				RMSE 2.284 Adj R-Sq	0.397 AICC 409.673 SBC 260.734 */
	
/* Model Selection simple glmselect with no interaction variables LASSO no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection =LASSO;
	run; quit; /* year*msrp 
				RMSE 2.284 Adj R-Sq	0.397 AICC 409.673 SBC 260.734 */

/* Model Selection simple glmselect with no interaction variables ELEASTICNET no CV*/
proc glmselect data = Hybrid2;
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection =ELASTICNET;
	run; quit; /* year*msrp 
				RMSE 2.284 Adj R-Sq	0.397 AICC 409.673 SBC 260.734 */
		

/* Model Selection glmselect with Interaction variables Stepwise with CV*/
proc glmselect data = Hybrid2 plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1250;
	partition fraction(test = .25 validate = .25); 
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection = stepwise( stop = CV) cvdetails;
	run; quit; /* msrp mpgmpge year*msrp 
				RMSE 1.819 Adj R-Sq 0.619 AICC 182.429 SBC 109.146 CVPRESS 269.307 */

/* Model Selection glmselect with Interaction variables LAR with CV*/
proc glmselect data = Hybrid2 plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1250;
	partition fraction(test = .25 validate = .25); 
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection = LAR( stop = CV) cvdetails;
	run; quit; /* mpg year*msrp 
				RMSE 2.041 Adj R-Sq 0.520 AICC 199.653 SBC 124.266 CVPRESS 278.518 */
	
/* Model Selection glmselect with Interaction variables LASSO with CV*/
proc glmselect data = Hybrid2 plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1250;
	partition fraction(test = .25 validate = .25); 
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection = LASSO( stop = CV) cvdetails;
	run; quit; /* mpg year*msrp 
				RMSE 2.041 Adj R-Sq 0.520 AICC 199.653 SBC 124.266 CVPRESS 278.518 */

/* Model Selection glmselect with Interaction variables ELASTICNET with CV*/
proc glmselect data = Hybrid2 plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1250;
	partition fraction(test = .25 validate = .25); 
	class carclass_id vehicle;
	model accelrate = vehicle year msrp mpg mpgmpge carclass_id msrp*year msrp*carclass_id year*carclass_id year*vehicle vehicle*carclass_id mpgmpge*carclass_id / 
	selection = ELASTICNET( stop = CV) cvdetails;
	run; quit; /* msrp mpg year*msrp 
				RMSE 2.055 Adj R-Sq 0.513 AICC 201.986 SBC 128.703 CVPRESS 269.307 */	

/* Final Model Fit Diagnostics */
data HybridReg;
set Hybrid2;
year_msrp=year*msrp;
K10= msrp/10000;
year_K10 = (year*msrp)/10000;
run;

ods graphics on;
proc ttest data =HybridReg h0=10.56 plots(showh0);
      var accelrate;
   run; 
ods graphics off;

proc glm data = HybridReg plots=all;
	model accelrate = msrp mpg year*msrp;
	run; quit;

proc reg data = HybridReg corr plots(label) = all;
	model accelrate = msrp mpg year_msrp/ VIF CLB CLI CLM;  
	title 'Hybrid Car Data Model';
	run; quit; /* F-value = 63.8, Adjusted R-Square = .561, PRESS = 611.803 */ 

proc reg data = HybridReg corr plots(label) = all;
	model accelrate = msrp mpg year_K10/ VIF CLB CLI CLM;  
	title 'Hybrid Car Data Model';
	run; quit; /* F-value = 63.8, Adjusted R-Square = .561, PRESS = 611.803 */
	
proc reg data = HybridReg corr plots(label) = all;
	model accelrate = mpg year_msrp/ VIF CLB CLI CLM;  
	title 'Hybrid Car Data Model';
	run; quit;/* F-value = 78.19, Adjusted R-Square = .504, PRESS = 670.042 */

proc reg data = HybridReg corr plots(label) = all;
	model accelrate = mpg year_K10/ VIF CLB CLI CLM;  
	title 'Hybrid Car Data Model';
	run; quit;/* F-value = 78.19, Adjusted R-Square = .504, PRESS = 670.042 */

proc reg data = HybridReg corr plots(label) = all;
	model accelrate = K10 mpg year_K10/ VIF CLB CLI CLM;  
	title 'Hybrid Car Data Model';
	run; quit; /* F-value = 63.8, Adjusted R-Square = .561, PRESS = 611.803 */




	

