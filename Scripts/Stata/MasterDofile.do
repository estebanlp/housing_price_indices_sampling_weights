clear all
set more off

********
********
** Master Do-file
** Housing Price Index
** This master do-file contains all the do-files to estimate housing price indices for sales and rents in Santiago's city municipalities in three different cases: without weight (sin expansor), with weight 1 (expansor 1) and with weight 2 (expansor 2). Both weights were built based on vacant households of the 2017 Census.

** Global paths: Data and Do-files

*global path "/Users/magdalena/Dropbox/CEPR/02 Housing Price Indices/"
global pathData "${path}01 Data/"
global pathDo "${path}03 Scripts/Stata/"


** Great Santiago (City Level)

*** No sampling weight (SW)

do "${pathDo}02_GS_Sale.do"
do "${pathDo}02_GS_Rent.do"

*** SW 1
do "${pathDo}02_GS_Sale_exp_1.do"
do "${pathDo}02_GS_Rent_exp_1.do"

*** SW 2 (includes vacancies of nearby census tracts with no posting data)
do "${pathDo}02_GS_Sale_exp_2.do"
do "${pathDo}02_GS_Rent_exp_2.do"


** GS Macrozones  

*** No sampling weight (SW)

do "${pathDo}03_GSMZ_Sale.do"
do "${pathDo}03_GSMZ_Rent.do"

*** SW 1
do "${pathDo}03_GSMZ_Sale_exp_1.do"
do "${pathDo}03_GSMZ_Rent_exp_1.do"

*** SW 2 (includes vacancies of nearby census tracts with no posting data)
do "${pathDo}03_GSMZ_Sale_exp_2.do"
do "${pathDo}03_GSMZ_Rent_exp_2.do"


*global pathRes "${path}02 Results/01_Comuna/Monthly/01.3_Sin expansor/01.5 Sale/"
*include "${pathDo}master_sinexp_sale.do"
global pathRes "${path}02 Results/01_Comuna/Monthly/01.3_Sin expansor/01.6 Rent/"
include "${pathDo}master_sinexp_rent.do"


*** Expansor 1
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.1_Expansor/01.5 Sale/"
*include "${pathDo}master_exp1_sale.do"
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.1_Expansor/01.6 Rent/"
*include "${pathDo}master_exp1_rent.do"


*** Expansor 2
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.2_Expansor 2/01.5 Sale/"
*include "${pathDo}master_exp2_sale.do"
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.2_Expansor 2/01.6 Rent/"
*include "${pathDo}master_exp2_rent.do"


** Macrozones within the GS


** High-lower coverage




*** Sin Expansor
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.3_Sin expansor/01.5 Sale/"
*include "${pathDo}master_sinexp_sale.do"
global pathRes "${path}02 Results/01_Comuna/Monthly/01.3_Sin expansor/01.6 Rent/"
include "${pathDo}master_sinexp_rent.do"


*** Expansor 1
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.1_Expansor/01.5 Sale/"
*include "${pathDo}master_exp1_sale.do"
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.1_Expansor/01.6 Rent/"
*include "${pathDo}master_exp1_rent.do"


*** Expansor 2
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.2_Expansor 2/01.5 Sale/"
*include "${pathDo}master_exp2_sale.do"
*global pathRes "${path}02 Results/01_Comuna/Monthly/01.2_Expansor 2/01.6 Rent/"
*include "${pathDo}master_exp2_rent.do"

/*
Descripción Comunas:

1. Puente Alto:
	- Sales
	
2. Vitacura:
	- Sales
	- Rents

3. Maipú:
	- Sales

4. La Reina:
	- Sales
	
5. Lo Barnechea:
	- Sales
	- Rents
	
6. Las Condes:
	- Sales
	- Rents

7. Santiago:
	- Sales
	- Rents

8. Providencia:
	- Sales 
	- Rents
	
9. Colina: no hay expansores

10. Viña del Mar: no hay expansores

11. Ñuñoa:
	- Sales 
	- Rents
	
12. Peñalolén:
	- Sales
	
13. La Florida:
	- Sales
	- Rents				*/
	
