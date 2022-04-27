clear all
set more off

*global path "/Users/estebanlopezochoa/Dropbox/Documents/003 Research/02 Housing Price Indices/"
*global path "/Users/magdalena/Dropbox/02 Housing Price Indices/"
global pathData "${path}01 Data/03_Groups/"
global pathRes "${path}04 Results/03_GSMZ/Rent_exp_2/"

*global pathRes "${path}04 Results/02_GS/03.1 Sale_Houses/"
*global pathRes "${path}04 Results/02_GS/03.2 Rent_Houses/"
*global pathRes "${path}04 Results/02_GS/03.3 Sale_Apt/"
*global pathRes "${path}04 Results/02_GS/03.4 Rent_Apt/"

* 5 doesn't have obs on 2017/9
* 11 Puente Alto drops most obs on 2018/04
* 16 Concon drops most obs on 2018/04
* 18 Maipu drops most obs on 2018/04
* no sufficient obserations on Vitacura, Colina, Vina, San Miguel, Penalolen, La Reina, Puente Alto, Nunoa, Macul, Valparaiso, Antofagasta, La Florida on 2018/04

local i=1
while `i'<5{
	local y=2017
	while `y'<2021{
		local m=1
		while `m'<13{
			if (`y'==2017 & `m'<=7) local m=8
			if (`y'==2018 & `m'==4) local m=5
			if (`y'==2020 & `m'>=12) continue, break 
			*local y=2020
			*local m=1
			insheet using "${pathData}`i'_`y'_`m'.csv", comma clear
			noi di in red "`i'_`y'_`m'.csv"
			*keep if idtipooperacion==1 | idtipooperacion==7 | idtipooperacion==8 | idtipooperacion==9 // for SALE
			*keep if idtipooperacion==2 | idtipooperacion==4 | idtipooperacion==10 | idtipooperacion==11 | idtipooperacion==16 | idtipooperacion==20 // for RENT
			keep if sale ==0
			keep if macrozona==`i'
			* HPI para tipo de propiedad
			*keep if idtipopropiedad==1 // solo casas
			*keep if idtipopropiedad==2 // solo deptos
			*destring latitud,replace
			*destring longitud,replace
			* droping percentile 99 from sqft
			sum sqft, d
			keep if inrange(sqft, r(p5), r(p95))
			* getting number of control and treated before matching
			tabstat preciouf, stat(count) save, if treated==1
			matrix NumbB_T=r(StatTotal)
			tabstat preciouf, stat(count) save, if treated==0
			matrix NumbB_NT =r(StatTotal)'
			matrix NumbB=(NumbB_T, NumbB_NT)
			mat rown NumbB = "`y' `m'"
			mat coln NumbB = Before
			local c=NumbB_T[1,1]
			local z=NumbB_NT[1,1]
			mat rown NumbB_T = "`i'_`y'_`m'"
			mat rown NumbB_NT = "`i'_`y'_`m'"
			*mat2txt , m(NumbB_T) sav("${pathRes}NumbB_T.txt") append
			if (`c'<=50) continue, break
			if (`z'<=50) continue, break
			*count if mespublicacion==`m'
			*global c= r(N)
			* Median Price index
			tabstat preciouf, stat(p50) save, if treated==1
			matrix Pri_T=r(StatTotal)
			tabstat preciouf, stat(p50) save, if treated==0
			matrix Pri_NT=r(StatTotal)'
			matrix Med_Pri=Pri_T[1,1]/Pri_NT[1,1]
			mat rown Med_Pri = "`y' `m'"
			mat2txt , m(Med_Pri) sav("${pathRes}`i'_Med_Ind.txt") append
			* pscore for calculating the caliper
			quietly logit treated dormitorios banos sqft longitud latitud idtipopropiedad
			predict ps
			tabstat ps, stat(sd) save, if treated==1
			matrix stats_T=r(StatTotal)
			tabstat ps, stat(sd) save, if treated==0
			matrix stats_NT=r(StatTotal)
			matrix sdfinal = 0.2 * stats_T[1,1]/stats_NT[1,1]
			matrix list sdfinal
			local cal = sdfinal[1,1]
			
			* Caliper matching on the propensity scores (k nearest neighboors pscore + cal)
			psmatch2 treated, outcome(preciouf) pscore(ps) neighbor(1) cal(`cal') common noreplacement
			drop if _support ==0
			drop _treated _support _weight _nn _precio _pdif
			order expansor2 _id _n1, last
			
			* replacing expansor2 from the matched obs
			drop if 	expansor2==. & _n1==.
			gen 	id2=cond(missing(expansor2), _n1, _id)
			bys 	id2 (expansor2): replace expansor2=expansor2[1] if missing(expansor2)
			sort 	_id
			
			*getting number of obs by treat/control after matching
			tabstat preciouf, stat(count) save, if treated==1
			matrix NumbA_T=r(StatTotal)
			tabstat precio, stat(count) save, if treated==0
			matrix NumbA_NT =r(StatTotal)'
			matrix NumbA=( NumbA_T, NumbA_NT)
			mat rown NumbA = "`y' `m'"
			mat coln NumbA = After
			local a=NumbA_T[1,1]
			local w=NumbA_NT[1,1] 
			mat Count= (NumbB, NumbA)
			mat rown NumbA_T = "`i'_`y'_`m'"
			mat rown NumbA_NT = "`i'_`y'_`m'"
			mat2txt , m(NumbA_T) sav("${pathRes}`i'_NumbA_T.txt") append
			mat2txt , m(Count) sav("${pathRes}`i'_CountB&A.txt") append
			* mahalanobis pscore + cal
			psmatch2 treated, outcome(preciouf) mahal(dormitorios banos sqft longitud latitud idtipopropiedad)
			*drop if _support ==0
				gen clon=_weight
				drop if clon ==.
				drop clon
				tabstat _treated , stats(count sum) save
				mat TreatStat=r(StatTotal)'
				mat rown TreatStat = "`y' `m'"
				mat2txt , m(TreatStat) sav("${pathRes}`i'_TreatStatMSA.txt") append
				tabstat ps, stats(mean)save by(_treated)
				mat NT=r(Stat1)
				mat TT=r(Stat2)	
				mat PSDiff=TT-NT
				mat rown PSDiff = "`y' `m'"
				mat2txt , m(PSDiff) sav("${pathRes}`i'_PSDiffMSA.txt") append
				gsort -_treated
				gen lnCP = ln(preciouf)
			*Fisher index over One2One
			regress lnCP dormitorios banos sqft longitud latitud idtipopropiedad [pweight = expansor2] if treated==1
				est store One2OneHedReg
				est table 
				est store betR
				matrix bet=r(coef)
				tabstat dormitorios banos  sqft longitud latitud idtipopropiedad [fweight = round(expansor2)], statistics(mean) columns(variables) save, if treated==1
				matrix Mean_r=r(StatTotal)
				matrix Mean_T= Mean_r, 1
					tabstat dormitorios banos sqft longitud latitud idtipopropiedad [fweight = round(expansor2)], statistics(var) columns(variables) save, if treated==1
					matrix Var_r=r(StatTotal)
				tabstat lnCP [fweight = _weight], statistics(mean) columns(variables) save, if treated==1
				matrix nlp_r=r(StatTotal)
				matrix nlp_r= nlp_r[1,1]
			regress lnCP dormitorios banos sqft longitud latitud idtipopropiedad [pweight = expansor2] if treated==0
				est store One2OneHedRegNT
				est table 
				est store betR_NT
				matrix bet_NT=r(coef)
				tabstat dormitorios banos sqft longitud latitud idtipopropiedad [fweight = round(expansor2)], statistics(mean) columns(variables) save, if treated==0
				matrix Mean_NT=r(StatTotal)
				matrix Mean_NT2= Mean_NT, 1
					tabstat dormitorios banos sqft longitud latitud idtipopropiedad [fweight = round(expansor2)], statistics(var) columns(variables) save, if treated==0
					matrix Var_NT=r(StatTotal)
				tabstat lnCP [fweight = _weight], statistics(mean) columns(variables) save, if treated==0
				matrix nlp_NT=r(StatTotal)
				matrix nlp_NT2= nlp_NT[1,1]
			* Fisher index based on hedonic regressions over matched databases
			matrix IndP = nlp_r - (bet_NT[1...,1]'*Mean_T[1,1...]')
			matrix IndL = (bet[1...,1]'*Mean_NT2[1,1...]') - nlp_NT2
			matrix Hed= sqrt(exp(IndP[1,1])*exp(IndL[1,1]))
			mat coln Hed = "HPI"
			mat rown Hed = "`y' `m'"
			mat2txt , m(Hed) sav("${pathRes}`i'_HedMSA.txt") append
			* Fisher Index2
			foreach a in dormitorios banos sqft longitud latitud idtipopropiedad{
			quietly sum `a' [fweight = round(expansor2)] if treated==1
			scalar meanT_`a'=r(mean)
			quietly sum `a' [fweight = round(expansor2)] if treated==0
			scalar meanC_`a'=r(mean)
			}
			scalar nlp_r= nlp_r[1,1]
			scalar nlp_NT2= nlp_NT2[1,1]
			xi: regress lnCP i.treated|dormitorios i.treated|banos i.treated|sqft i.treated|longitud i.treated|latitud  i.treated|idtipopropiedad treated [pweight = expansor2] 		
			nlcom sqrt(exp(nlp_r - (_b[dormitorios]*meanT_dormitorios + _b[banos]*meanT_banos + _b[sqft]*meanT_sqft + _b[longitud]*meanT_longitud  + _b[latitud]*meanT_latitud + _b[idtipopropiedad]*meanT_idtipopropiedad+_b[_cons]*1))*exp(((_b[dormitorios]+_b[_ItreXdormi_1])*meanC_dormitorios + (_b[banos]+_b[_ItreXbanos_1])*meanC_banos +  (_b[sqft]+_b[_ItreXsqft_1])*meanC_sqft +  (_b[longitud]+_b[_ItreXlongi_1])*meanC_longitud +  (_b[latitud]+_b[_ItreXlatit_1])*meanC_latitud +  (_b[idtipopropiedad]+_b[_ItreXidtip_1])*meanC_idtipopropiedad + (_b[treated]+_b[_cons])*1)-nlp_NT2))
			mat Hed2=r(b)
			mat Sd2=r(V)
			mat Sd2=sqrt(Sd2[1,1])
			mat Hed2_Sd2=(Hed2, Sd2)
			mat coln Hed2_Sd2 = "HPI" "SD"
			mat rown Hed2_Sd2 = "`y' `m'"
			mat2txt , m(Hed2_Sd2) sav("${pathRes}`i'_HedPrice2_Sd2.txt") append
			* Standarized differences between T and NT variables used for the matching
			matrix VarP =(Var_r[1,1...]+ Var_NT[1,1...])/2
			matrix MeanP =(Mean_r - Mean_NT)
			local j=1
			matrix define Sqvar=(0,0,0,0,0,0)
			matrix define StandD_p =(0,0,0,0,0,0)
			while `j' <= 6{
				matrix Sqvar[1,`j']=sqrt(VarP[1,`j'])
				matrix StandD_p[1,`j']=(MeanP[1,`j']/Sqvar[1,`j'])
				local j=`j'+1
				}
			matrix StandD= 100*StandD_p
			mat coln StandD = bed bath sqft lon lat house
			mat rown StandD = "`y' `m'"
			mat2txt , m(StandD) sav("${pathRes}`i'_StandDMSA.txt") append
			local m=`m'+1
		}
		local y=`y'+1
	}
local i=`i'+1
}
