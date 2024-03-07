
/************************************************************/
/*fichier de reference de sauvegarde des données*//*à modifier par l'utilisateur*/
x "mkdir ./data";
filename data1 "./data/donnees1.csv";
filename data2 "./data/donnees2.csv";
filename data3 "./data/donnees3.csv";
filename data4 "./data/donnees4.csv";


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

/*Calcul des rendements journaliers*/
%macro rentabilite();
%do i=1 %to 3;
data donnees&i; set donnees&i;
rent&i=log(close)-log(open);
format date date9.;
run;
%end;
%mend;

%rentabilite;



data RENT (rename=(rent1=RENT_TES rent2=RENT_HYU rent3=RENT_LOR Change=RENT_Gold));
merge donnees1 donnees2 donnees3 donnees4;
by date;
where date > '01JAN2017'd AND date<'01MAY2023'd  ;
keep date rent1 rent2 rent3 Change;
if cmiss(of _all_) then delete;/*suppression des jours fériés ou versement de dividendes*/
run;

/**************************************** Analyse exploratoire ******************************/
*Statistiques descriptives sur les series;
proc univariate data=RENT;
var RENT_TES RENT_HYU RENT_LOR RENT_Gold;
histogram / normal kernel;
qqplot /normal(mu=est sigma=est);
run;

/*Test de Stationnarité des séries de rendements*/
%macro stationarity_test(asset);
PROC ARIMA DATA=RENT;
IDENTIFY VAR=&asset stationarity=(adf=(2,5));
IDENTIFY VAR=&asset stationarity=(pp=6);
run;
quit;
%mend;
%stationarity_test(RENT_TES);
%stationarity_test(RENT_HYU);
%stationarity_test(RENT_LOR);
%stationarity_test(RENT_Gold);


/***************************************Matrice de corrélation **************************************/
/*Ce code a été repris du précédent mémoire sous SAS*/
ods output KendallCorr=Corr_P;
/*calcul des correlations*/
PROC CORR DATA=RENT Kendall;
    VAR RENT_TES RENT_HYU RENT_LOR RENT_Gold;
RUN;

/*tri necessaire pour les transpositions*/
proc sort data=Corr_P;
    by VARIABLE;
run;


proc transpose data=Corr_P out=CorrLong1(rename=(COL1=Correlation)) 
        name=CorrelationID;
    var RENT_TES RENT_HYU RENT_LOR RENT_Gold;
    by Variable;
run;

proc transpose data=Corr_P out=CorrLong2(rename=(COL1=p_value)) name=PvalueID;
    var PRENT_TES PRENT_HYU PRENT_LOR PRENT_Gold;
    by Variable;
run;

data CorrLong;
    merge CorrLong1 CorrLong2(drop=PvalueID );
    by Variable;
    LABEL CorrelationID="Correlations"; run;

proc sort data=CorrLong;
    by VARIABLE CorrelationID;
run;

/*representation graphique*/
ods graphics on / imagefmt=png width=4in height=4in imagename="heatmap";
proc sgplot data=CorrLong noautolegend;
heatmap x=Variable y=CorrelationID / colorresponse=Correlation name="nope1" discretex discretey x2axis colormodel=ThreeColorRamp; *Colorresponse allows discrete squares for each correlation. x2axis bring the label to the top;
text x=Variable y=CorrelationID text=correlation  / textattrs=(size=10pt) x2axis name='nope2'; /*To overlay significance, create a variable that contans that info and set text=VARIABLE */
label correlation='Kendall Correlation';
title "Matrice de corrélation";
yaxis reverse display=(nolabel);
x2axis display=(nolabel);
gradlegend;

run;
ods graphics off;

/*****************************************************************************************************************************/

/********************************             CONSTRUCTION DU PORTEFEUILLE          ****************************************/

/*Calcul des poids des actifs du portefeuille*/
proc iml;
/*Ratio de Sharpe*/
start sharpe_ratio(w);
	use RENT;
	read all var {"RENT_TES" "RENT_HYU" "RENT_LOR" "RENT_Gold"} into X[colname=varNames];
	close RENT;
		w1=w[1]/w[+];
		w2=w[2]/w[+];
		w3=w[3]/w[+];
		w4=w[4]/w[+];
		PTF=w1*X[,1]+w2*X[,2]+w3*X[,3]+w4*X[,4];
		moy_PTF=mean(PTF);
		volatility_PTF=std(PTF);
		rf=1.03**(1/365)-1;
		sharpe=(moy_PTF-rf)/volatility_PTF;
		return (sharpe);
	finish sharpe_ratio;
store module=(sharpe_ratio);


constraints={0 0 0 0 . .,
			. . . . . .,
			1 1 1 1 0 1};/*position longue sur le PTF*/

			/*Les deux premières lignes sont les contraintes sur les paramètres wi*/
			/* 3e ligne ==> contrainte sur les poids sommant à 1*/

w=j(1,4,rand("Uniform",0,1)); /*Initialisation des paramètres*/
opt={1 2 1 . 2}; /*1 pour maximiser la fonction objectif sharpe ratio et 2 pour la méthode (à voir)*/

/*Optimisation et calcul des pondérations et export en pdf*/

call nlpnra(rc,xres,"sharpe_ratio",w,opt,constraints); /*Optimisation du ratio de Sharpe par Newton-Raphson*/

use RENT;
read all var {"RENT_TES" "RENT_HYU" "RENT_LOR" "RENT_Gold"} into SERIES;
close RENT;

PTF=xres[1]*SERIES[,1]+xres[2]*SERIES[,2]+xres[3]*SERIES[,3]+xres[4]*SERIES[,4];
/*PIE CHART PART DES ACTIFS DANS LE PORTEFEUILLE*/
names = {"Tesla" "HYUNDAI" "LOREAL" "GOLD"} ;
create proportions from xres[colname=names];
append from xres;
close proportions;

data nouvelle_table (keep = ACTIF PART);
   set proportions;   
   length ACTIF $10.;
   ACTIF = "Tesla";
   PART = Tesla;
   output;
   ACTIF = "Hyundai";
   PART = Hyundai;
   output;
   ACTIF = "LOREAL";
   PART = LOREAL;
   output;
   ACTIF = "Gold";
   PART = Gold;
   output;
run;

ods graphics on / imagefmt=png width=4in height=5in imagename="pie";
title1 ls=1.5 "PART DES ACTIFS DANS LE PORTEFEUILLE";
proc gchart data=nouvelle_table;
pattern1 v=s color=CXFFAA00;
pattern2 v=s color=CX3883A8;
pattern3 v=s color=CX2A8307;
pattern4 v=s color= CXFF0000	;
pie ACTIF / type=sum sumvar=PART slice=arrow value=none percent=arrow 
			legend= legend;
run;
ods graphics off;




/*EXPORTATION DES RENDEMENTS HISTORIQUES DU PTF*/
create portfolio from PTF;
append from PTF;
close portfolio;

data RENT(rename=(COL1=RENT_PTF));
merge RENT PORTFOLIO;
run;

/******************************Stat desc et stationarité du ptf***********************************/
proc univariate data=RENT;
var RENT_PTF;
histogram / normal kernel;
qqplot /normal(mu=est sigma=est);
run;


%stationarity_test(RENT_Gold);/*test de stationnarite des rendements du portefeuille*/

/*  *************  ********************* REPRESENTATIONS GRAPHIQUES   ************ ********************* ***********/
/*Representations des series de rendements en panel (2,2)*/
proc template;
	define statgraph intro;
	begingraph ;
		entrytitle "Représentations des rendements journaliers par actif";
		layout lattice / columns=2 rows=2 columnweights=Uniform rowweights=Uniform;
		seriesplot X=Date Y=RENT_TES / name='1' legendlabel="RENT_TES" lineattrs=(color=RED);
		seriesplot X=Date Y=RENT_HYU / name='2' legendlabel="RENT_HYU" lineattrs=(color=BLUE);
		seriesplot X=Date Y=RENT_LOR / name='3' legendlabel="RENT_LOR" lineattrs=(color=GREEN);
		seriesplot X=Date Y=RENT_GOLD / name='4' legendlabel="RENT_Gold" lineattrs=(color=YELLOW);		
		sidebar / align=bottom; 
		discretelegend '1' '2' '3' '4'  / valueattrs=(size=8pt) order=columnmajor down=1 halign=center title="Actifs"; /*'5'*/
		endsidebar;
		endlayout;
	endgraph;
	end;
run;


proc sgrender data=RENT (where=(date > '01JAN2018'd)) template=intro;
run;

/*Representation des rendements du portefeuille*/
proc template;
	define statgraph intro_ptf;
	begingraph ;
		entrytitle "Représentation des rendements journaliers du portefeuille";
		layout lattice;
		seriesplot X=Date Y=RENT_PTF / name='1' legendlabel="RENT_PTF" lineattrs=(color=BLUE);
		sidebar / align=bottom; 
		discretelegend '1'  / valueattrs=(size=8pt) order=columnmajor down=1 halign=center; /*'5'*/
		endsidebar;
		endlayout;
	endgraph;
	end;
run;


proc sgrender data=RENT (where=(date > '01JAN2018'd)) template=intro_ptf;
run;


/*Module/fonction pour calculer la VaR de simulation historique d'une série (en mode moving average)*/
proc iml;

/******************  VAR DE SIMULATION HISTORIQUE (BOOTSTRAP)*******************/
start hist_VaR(serie,window,tail);
	n=nrow(serie);
	VaR=j(n,1,0);
	do i=251 to n;
		free z;
		j=i-1;
		k=j-window+1;/*OK*/
		z=serie[k:j];/*OK*/
		call sort(z);/*OK*/
		sample_quant=j(100000,1);/*Initialization of sample*/
		call randgen(sample_quant,"Uniform");
		sample=j(100000,1,0);
		do j=1 to 100000;
			rang_sample=ceil(nrow(z)*sample_quant[j]);
			sample[j]=z[rang_sample];
			end;
		call sort(sample);
		rang=ceil(nrow(sample)*tail);
		VaR[i]=sample[rang];
	end;
	return (VaR);
finish;

USE RENT;
READ all var {"RENT_TES" "RENT_HYU" "RENT_LOR" "RENT_Gold" "RENT_PTF"} into X;
close RENT;

zz=J(nrow(X),5,0);
do j=1 to 5;
	y=X[,j];
	zz[,j]= hist_VaR(Y,250,0.01);
end;

create varhist from zz;
append from zz;
close varhist;

submit;
data Global (rename=(COL1=VaR_RENT_TES COL2=VaR_RENT_HYU COL3=VaR_RENT_LOR COL4=VaR_RENT_Gold COL5=VaR_RENT_PTF));
merge Rent Varhist;
run;
data Global; set Global; 
WHERE DATE > '01JAN2018'd;
run;
endsubmit;

%macro kupiec_test(TABLE,asset);
data &TABLE; set &TABLE;
violation_&asset = (&asset < VaR_&asset);
run;
	proc iml;
		USE &TABLE;
		READ  all var {"violation_&asset"} into x;
		CLOSE &TABLE;
	T=nrow(x);
	N=X[+];/*nombre total de violations*/
	m=t-n;
	THO=n/t;/*taux de violation*/
	KUPIEC_STAT=-2*log(((1-0.01)**m)*((0.01)**n)) + 2*log(((1-tho)**m)*(tho**n));/*stat de test*/
	P_VALUE= 1 - cdf("ChiSq", KUPIEC_STAT, 1);
	KUPIEC_TEST_SUMMARY = T || N || THO || KUPIEC_STAT || P_VALUE;/*tableau résumé*/
	variables ={&asset};
	names={T N THO KUPIEC_STAT P_VALUE};
	mattrib KUPIEC_TEST_SUMMARY rowname=variables colname=names;/*putting columns and rowname*/
	print KUPIEC_TEST_SUMMARY;

%mend;

%macro BINOMIAL_TEST(table,asset);
    proc iml;
		USE &TABLE;
		READ  all var {"violation_&asset"} into x;
		CLOSE &TABLE;
    /*nombre total d'observations (T)*/
    T = nrow(X);
    N = X[+];
	S0= T-N;

	/*calcul de la stat de test*/
    BINOMIAL_STAT = (N-(T*0.01))/((T*0.01*(1-0.01))**0.5);
    
    /* Calcul de la p-value */
    P_VALUE = 2*(1 - cdf("Normal",abs(BINOMIAL_STAT)));

	BINOMIAL_TEST_SUMMARY= T || N || BINOMIAL_STAT || P_VALUE;
	variables= {&asset};
	names= {T N BINOMIAL_STAT P_VALUE};
	mattrib BINOMIAL_TEST_SUMMARY rowname=variables colname=names;/*putting columns and rowname*/
	/* Affichage des résultats */
    print BINOMIAL_TEST_SUMMARY; 
	
%mend BINOMIAL_TEST;

/*Calcul de la VaR par la methode Equally Weighted Moving Average*/
/*Sous l'hypothese que les rendements sont centrés et gaussiens, l'écart-type est calculé sur les t-1 valeurs passées : rolling windows*/
start VaR_EWMA(serie,window,tail);
	n=nrow(serie);
	VaR=j(n,1,0);
	do i=251 to n;/*251 correspond au O2JAN2018: début de la période de prev*/
		free z;
		j=i-1;
		k=j-window+1;
		z=serie[k:j];
		sigma=sqrt(Var(z));/*calcul de la volatilité*/
		VaR[i]=(probit(tail))*sigma;/*inverse réduire*/
	end;
	return (VaR);
finish;

USE RENT;
READ all var {"RENT_TES" "RENT_HYU" "RENT_LOR" "RENT_Gold" "RENT_PTF"} into X;
close RENT;
free zz;
zz=J(nrow(X),5,0);
do j=1 to 5;
	y=X[,j];
	zz[,j]= VaR_EWMA(Y,250,0.01);
end;

create varewma from zz;
append from zz;
close varewma;

submit;
data Global2 (rename=(COL1=VaR_RENT_TES COL2=VaR_RENT_HYU COL3=VaR_RENT_LOR COL4=VaR_RENT_Gold COL5=VaR_RENT_PTF));
merge Rent Varewma;
run;

data Global2; set Global2; 
WHERE DATE > '01JAN2018'd;
run;
endsubmit;

/*Representations graphiques*/
proc template;
 define statgraph dynamics;
   dynamic ASSET VARASSET METHOD;
   begingraph;
     entrytitle "Représentation de " ASSET " et de " VARASSET " calculée par la méthode " METHOD " .";
     layout overlay / yaxisopts=(label="Returns" linearopts=(viewmin=-0.20 viewmax=0.20 tickvaluelist=(-0.20 -0.15 -0.10 -0.05 0 0.05 0.10 0.15 0.20)));
      seriesplot X=Date Y=ASSET / name='1' legendlabel=Asset lineattrs=(color=BLUE);
	  seriesplot X=Date Y=VARASSET / name='2' legendlabel=VARASSET lineattrs=(color=RED);
	  discretelegend '1' '2' / border=True;
     endlayout;
   endgraph;
 end;
run;

%macro plot_var_hist(series);
	proc sgrender data=Global 
              template=dynamics;
  dynamic ASSET="&series" VARASSET="VAR_&series" METHOD="historique";
run;
%mend;

%macro plot_var_ewma(series);
	proc sgrender data=Global2 
              template=dynamics;
  dynamic ASSET="&series" VARASSET="VAR_&series" METHOD="EWMA";
run;
%mend;

/*Var historique et Kupiec,binomial*/

%plot_var_hist(RENT_TES);
%KUPIEC_TEST(GLOBAL,RENT_TES);
%BINOMIAL_TEST(GLOBAL,RENT_TES);

%plot_var_hist(RENT_HYU);
%KUPIEC_TEST(GLOBAL,RENT_HYU);
%BINOMIAL_TEST(GLOBAL,RENT_HYU);

%plot_var_hist(RENT_LOR);
%KUPIEC_TEST(GLOBAL,RENT_LOR);
%BINOMIAL_TEST(GLOBAL,RENT_LOR);

%plot_var_hist(RENT_Gold);
%KUPIEC_TEST(GLOBAL,RENT_Gold);
%BINOMIAL_TEST(GLOBAL,RENT_Gold);

%plot_var_hist(RENT_PTF);
%KUPIEC_TEST(GLOBAL,RENT_PTF);
%BINOMIAL_TEST(GLOBAL,RENT_PTF);


%plot_var_ewma(RENT_TES);
%KUPIEC_TEST(GLOBAL2,RENT_TES);
%BINOMIAL_TEST(GLOBAL2,RENT_TES);

%plot_var_ewma(RENT_HYU);
%KUPIEC_TEST(GLOBAL2,RENT_HYU);
%BINOMIAL_TEST(GLOBAL2,RENT_HYU);

%plot_var_ewma(RENT_LOR);
%KUPIEC_TEST(GLOBAL2,RENT_LOR);
%BINOMIAL_TEST(GLOBAL2,RENT_LOR);

%plot_var_ewma(RENT_Gold);
%KUPIEC_TEST(GLOBAL2,RENT_Gold);
%BINOMIAL_TEST(GLOBAL2,RENT_Gold);

%plot_var_ewma(RENT_PTF);
%KUPIEC_TEST(GLOBAL2,RENT_PTF);
%BINOMIAL_TEST(GLOBAL2,RENT_PTF);




/*Méthode de Monte Carlo avec Rolling Window*/
/*Définir une plus grande fenêtre*/
proc iml;
start monte_carlo_var(series,window);
	n=nrow(series);
	VaR=j(n,1,0);
	do i=251 to n;
		free z;
		j=i-1;
		k=j-window+1;
		z=series[k:j];
		sigma=sqrt(Var(z));
		free z;
		z=j(100000,1);
			call randgen(z,"Normal",0,sigma);
			call sort (z);
			rang = ceil(100000*0.01);
			var[i]= z[rang];
		end;
		return(var);
finish;
USE RENT;
READ all var {"RENT_TES" "RENT_HYU" "RENT_LOR" "RENT_Gold" "RENT_PTF"} into X;
close RENT;
free zz;
zz=J(nrow(X),5,0);
do j=1 to 5;
	y=X[,j];
	zz[,j]= monte_carlo_var(Y,250);
end;

create MonteCarlo from zz;
append from zz;
close MonteCarlo;

submit;
data Global3 (rename=(COL1=VaR_RENT_TES COL2=VaR_RENT_HYU COL3=VaR_RENT_LOR COL4=VaR_RENT_Gold COL5=VaR_RENT_PTF));
merge Rent MonteCarlo;
run;

data Global3; set Global3; 
WHERE DATE > '01JAN2018'd;
run;
endsubmit;

/*representations graphiques*/
%macro plot_var_mc(series);
	proc sgrender data=Global3 
              template=dynamics;
  dynamic ASSET="&series" VARASSET="VAR_&series" METHOD="Monte Carlo avec EWMA";
run;
%mend;


%plot_var_mc(RENT_TES);
%KUPIEC_TEST(GLOBAL3,RENT_TES);
%BINOMIAL_TEST(GLOBAL3,RENT_TES);

%plot_var_mc(RENT_HYU);
%KUPIEC_TEST(GLOBAL3,RENT_HYU);
%BINOMIAL_TEST(GLOBAL3,RENT_HYU);

%plot_var_mc(RENT_LOR);
%KUPIEC_TEST(GLOBAL3,RENT_LOR);
%BINOMIAL_TEST(GLOBAL3,RENT_LOR);

%plot_var_mc(RENT_Gold);
%KUPIEC_TEST(GLOBAL3,RENT_Gold);
%BINOMIAL_TEST(GLOBAL3,RENT_Gold);

%plot_var_mc(RENT_PTF);
%KUPIEC_TEST(GLOBAL3,RENT_PTF);
%BINOMIAL_TEST(GLOBAL3,RENT_PTF);


/*ESTIMATION VAR PARAMETRIQUE A EFFETS GARCH*/


/*Specification de l'ordre du Garch avec proc arima*/

/*La règle de décision est telle que le BIC de SChwartz soit minimale et tous les coeffs soient significatifs*/
/*Le BIC de Schwartz est un critère qui est justifié asymptotiqueemnt et est réputé pour sa capacité à retrouver le vrai modèle*/

%macro garch_optimal_order(series);
DATA ORD ; SET RENT;
RENT_2 = (&SERIES)**2;
WHERE DATE > '25DEC2017'd;
RUN;
proc ARIMA data=ORD;
IDENTIFY VAR=RENT_2 minic p=(1:3) q=(1:3);
run;
quit;
%mend;

%garch_optimal_order(RENT_TES);
/*Optimal Order for RENT_TES=(p=1,q=1)*/
%garch_optimal_order(RENT_HYU);

%garch_optimal_order(RENT_LOR);

%garch_optimal_order(RENT_Gold);

%garch_optimal_order(RENT_PTF);


%macro estimation_garch(series,q,p);
	PROC AUTOREG data=RENT;
		model &series= / noint garch=(q=&q,p=&p) maxiter=200 noprint;/*specification du modèle*/
		output out=g_&series cev=condvar_&series;/*out variance conditionnelle*/
		where date> '25DEC2017'd;/*3 derniers jours de 2017 pris pour la prev au 02JAN2018*/
	RUN;

	PROC IML;
	USE G_&series;
	READ ALL VAR {"condvar_&series"} into X;
	CLOSE G_&series;
	
		n=nrow(X);
		var=j(n,1,0);
		%DO i=1 %to 1319;/*1319 correspond à la taille de l'echantillon (prob d'utilisation de la variable contenant la taille); meme en macro passe pas*/
			ht_2=X[&i];/*variance conditionnelle en date t*/
		/*Monte Carlo simulation*/
		free z;
		z=j(100000,1);
			call randgen(z,"Normal",0,sqrt(ht_2));/*ehantillon*/
			call sort (z);/*tri croissant*/
			rang = ceil(100000*0.01);/**/
			var[&i]= z[rang];/*1er percentile*/
		%END;
		names=("VAR_Y");
	CREATE V_&series from VAR[colname=names];
	APPEND from VAR;
	CLOSE V_&series;

	DATA GARCH (rename=(VAR_Y=VAR_&series));
		MERGE G_&series V_&series;
	RUN;

	DATA GARCH; set GARCH;
	where date > '01JAN2018'd;
	RUN;
	/*Représentation graphique*/
	proc sgrender data=GARCH 
              template=dynamics;
  	dynamic ASSET="&series" VARASSET="VAR_&series" METHOD="Monte Carlo à erreurs GARCH";
	run;
	
%mend estimation_garch;


/*Les ordres optimaux sont trouvés par itérations successives en prenant en compte le SBC et la significativité des paramètres*/

%macro estimation_igarch(series,q,p);
	PROC AUTOREG data=RENT;
		model &series= / noint garch=(q=&q,p=&p,type=INTEGRATED) maxiter=200 noprint;/*specification du modèle*/
		output out=g_&series cev=condvar;/*out la volatilité conditionnelle*/
		where date> '25DEC2017'd;/*3 derniers jours de 2017 pris pour la prev au 02JAN2018*/
	RUN;

	PROC IML;
	USE G_&series;
	READ ALL VAR {"condvar"} into X;
	CLOSE G_&series;
	
		n=nrow(X);
		var=j(n,1,0);
		%DO i=1 %to 1319;/*1319 correspond à la taille de l'echantillon (prob d'utilisation de la variable contenant la taille); meme en macro passe pas*/
			ht_2=X[&i];/*variance conditionnelle en date t*/
		/*Monte Carlo simulation*/
		free z;
		z=j(100000,1);
			call randgen(z,"Normal",0,sqrt(ht_2));/*echantillon*/
			call sort (z);
			rang = ceil(100000*0.01);
			var[&i]= z[rang];

		%END;
		names=("VAR_Y");
	CREATE V_&series from VAR[colname=names];
	APPEND from VAR;
	CLOSE V_&series;

	DATA IGARCH (rename=(VAR_Y=VAR_&series));
		MERGE G_&series V_&series;
	RUN;

	DATA IGARCH; set IGARCH;
	where date > '01JAN2018'd;
	RUN;
	/*representation graphique*/
	proc sgrender data=IGARCH 
              template=dynamics;
  		dynamic ASSET="&series" VARASSET="VAR_&series" METHOD="Monte Carlo à erreurs IGARCH";
		run;
	/**/
%mend estimation_igarch;

/*ESTIMATION DES GARCH*/

%estimation_garch(RENT_TES,1,1);
%KUPIEC_TEST(GARCH,RENT_TES);
%BINOMIAL_TEST(GARCH,RENT_TES);

%estimation_garch(RENT_HYU,1,2);
%KUPIEC_TEST(GARCH,RENT_HYU);
%BINOMIAL_TEST(GARCH,RENT_HYU);

%estimation_garch(RENT_LOR,1,1);
%KUPIEC_TEST(GARCH,RENT_LOR);
%BINOMIAL_TEST(GARCH,RENT_LOR);

%estimation_garch(RENT_Gold,1,1);
%KUPIEC_TEST(GARCH,RENT_Gold);
%BINOMIAL_TEST(GARCH,RENT_Gold);

%estimation_garch(RENT_PTF,1,1);
%KUPIEC_TEST(GARCH,RENT_PTF);
%BINOMIAL_TEST(GARCH,RENT_PTF);

/*ESTIMATION DES IGARCH*/
%estimation_igarch(RENT_TES,1,1);
%KUPIEC_TEST(IGARCH,RENT_TES);
%BINOMIAL_TEST(IGARCH,RENT_TES);

%estimation_igarch(RENT_HYU,1,2);
%KUPIEC_TEST(IGARCH,RENT_HYU);
%BINOMIAL_TEST(IGARCH,RENT_HYU);

%estimation_igarch(RENT_LOR,1,1);
%KUPIEC_TEST(IGARCH,RENT_LOR);
%BINOMIAL_TEST(IGARCH,RENT_LOR);

%estimation_igarch(RENT_Gold,1,1);
%KUPIEC_TEST(IGARCH,RENT_Gold);
%BINOMIAL_TEST(IGARCH,RENT_Gold);

%estimation_igarch(RENT_PTF,1,1);
%KUPIEC_TEST(IGARCH,RENT_PTF);
%BINOMIAL_TEST(IGARCH,RENT_PTF);




