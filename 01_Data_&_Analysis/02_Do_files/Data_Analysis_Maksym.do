************************×*****××****××****××****××***××****××****××***××××××××××
** TITLE: Experience - Wage profiles 
** PURPOSE: Investigate how wages evolve over the life-cycle between black and white workers both in raw terms and once one accounts for differences in skills
** PROJECT: MAPSS Thesis
** AUTHOR: Maksym Kutsenko
** DATE: April 17 2024
************************×*****××****××****××****××***××****××****××***××××××××××


* Import the cleaned data 
cd "/Users/maksym/Dropbox (Compte personnel)/Mon Mac (MacBook Pro de Maksym)/Desktop/Chicago/1. Studies/Thesis/1. Final Thesis/Data/Data_files"

clear all

import delimited NLSY79_stata.csv

* AFQT trends check 
gen hgc2 = real(hgc)
keep if educ_30 == 12 & educ == 12 & (hgc2 == 12 | missing(hgc2))&sample_pref == 1&ptexp>=1

bysort age_test: egen std_afqt = std(afqt_std)
gen black_age_test = age_test*black

sort id
quietly by id:  gen dup = cond(_N==1,0,_n)
drop if dup>1
		
* Test that the afqt gap doesn't increase between the two cohorts:
reg std_afqt age_test black black_age_test if age_test<=18
eststo afqt_1

reg std_afqt age_test black black_age_test if age_test>=17&age_test<=22, cluster(id)
eststo afqt_2

esttab afqt_1 afqt_2 using "A1.tex", varwidth(50) se compress ///
star(* 0.1 ** 0.05 *** 0.01) replace label ///
mtitle("AFQT, age test <= 18" "AFQT, age test >= 17") ///
stats(r2 N, fmt( %4.2f %12.0fc))

eststo clear
* The gap might be increasing with age, but most of it happens for those who are below 18, and then it stabilizes for ages 18+. Add some overlap to avoid having unaddressed jumps between 18 and 19 in the AFQT gap for example. 


************************×*****××****××****××****××***××****××****××***××××××××××
* Further cleaning
************************×*****××****××****××****××***××****××****××***××××××××××

clear all

import delimited NLSY79_stata.csv

* make education numeric, and then subset the relevant sample
gen hgc2 = real(hgc)
keep if educ_30 == 12 & educ == 12 & (hgc2 == 12 | missing(hgc2))&sample_pref == 1&ptexp>=1

** Generate relevant variables **

* Re-standardize AFQT scores among those who are high school graduates:
bysort age_test: egen std_afqt = std(afqt_std)
gen post = 0
replace post = 1 if age_test>18

* The logic is that those who took the test after 18 are in a pool of individuals where some are in a university already. These people will have much lower AFQT than if they were compared at younger ages as higher education affects skills development. Thus, to make AFQT standardized for the high-schoolers, I re-standardize the AFQT scores 


* Wages and controls  
gen lnwage = real(logwage)
gen urban2 = real(urban)
gen region2 = real(region)
gen emp2 = real(emp)

* wages for those employed, and "." otherwise 
gen lnwage2 = .
replace lnwage2 = lnwage if emp2==1
replace lnwage2 = . if emp2==0

* Skills measures
gen soc_nlsy2_std2 = real(soc_nlsy2_std)
gen noncog_std2 = real(noncog_std)
gen std_afqt2 = std_afqt*std_afqt

* Experience and interactions 
gen ptexp2 = ptexp*ptexp
gen ptexp_afqt = ptexp*std_afqt
gen ptexp_black = ptexp*black
gen ptexp_afqt2 = ptexp*std_afqt2
gen ptexp2_black = ptexp2*black
gen ptexp2_afqt = ptexp2*std_afqt
gen ptexp2_afqt2 = ptexp2*std_afqt2
gen black_age_test = age_test*black

* Create a lag of employment 
sort id year 
by id: gen lag_emp2 = emp2[_n-1]

* Vizualize what's up with employment and afqt:

twoway (scatter emp2 black) (lfit emp2 black)
 (scatter le year) (lfit le year)
 
* Raw gap estimation
areg lnwage2 black if ptexp==10, absorb(year) cluster(id)

* By age brackets
areg lnwage2 black if age_test<=18&ptexp==11, absorb(year) cluster(id)
areg lnwage2 black if age_test>18&ptexp==11, absorb(year) cluster(id)
* Wage gap is wider for cohorts born earlier when evaluated at the same time in their potential experience - wage cycle. 

* areg lnwage2 afqt_std black if year==91|year==90, absorb(year) cluster(id)

/*
However, if wwe run the analysis at face value, we will have an erronated estimation of the coefficient on black, afqt, and other variables, as those employed and thus for whom we observe wages are likely to differ from those who we do not observe and thus for whom we do not observe wages
*/

* Those with higher afqt and non-cognitive skills have higher employment rates, blacks have lower employment rates, 
areg std_afqt emp2 if ptexp==11, absorb(year) cluster(id)
areg black emp2 if ptexp==11, absorb(year) cluster(id)
areg noncog_std2 emp2 if ptexp==11, absorb(year) cluster(id)
areg soc_nlsy2_std2 emp2 if ptexp==11, absorb(year) cluster(id)

* Let's do the first stage probit by hand: 
probit emp2 black std_afqt noncog_std2 if ptexp==20, cluster(id)


*** Assessing the wage gaps between Blacks and Whites

* Ordinary model - raw: 
areg lnwage2 black if ptexp==10&soc_nlsy2_std2!=.&lag_emp2!=., absorb(year) cluster(id)
eststo lm1
** Heckman model - raw - control for different things **

heckman lnwage2 black i.year if ptexp==10, select(emp2=black lag_emp2) twostep
eststo lm2
 
heckman lnwage2 black i.year if ptexp==10, select(emp2=black std_afqt lag_emp2) twostep 
eststo lm3

esttab lm1 lm2 lm3 using "Table_1.tex", varwidth(50) se compress ///
star(* 0.1 ** 0.05 *** 0.01) replace label ///
mtitle("OLS" "Heckman" "Heckman") drop(8* 9* 1*) ///
stats(r2 N, fmt( %4.2f %12.0fc))

eststo clear 

* Ordinary model - AFQT - controlled: 
areg lnwage2 black std_afqt if ptexp==10&soc_nlsy2_std2!=.&lag_emp2!=., absorb(year) cluster(id)
eststo lm1
** Heckman model - raw - control for different things **

heckman lnwage2 black std_afqt i.year if ptexp==10, select(emp2=black std_afqt lag_emp2) twostep 
eststo lm2

esttab lm1 lm2 using "Table_2.tex", varwidth(50) se compress ///
star(* 0.1 ** 0.05 *** 0.01) replace label ///
mtitle("OLS" "Heckman") drop(8* 9* 1*) ///
stats(r2 N, fmt( %4.2f %12.0fc))

eststo clear

areg lnwage black ptexp ptexp2 ptexp_black ptexp2_black if soc_nlsy2_std2!=.&noncog_std2!=., cluster(id) absorb(year)
eststo lm1

areg lnwage black ptexp ptexp2 ptexp_black ptexp2_black std_afqt std_afqt2 soc_nlsy2_std2 noncog_std2 ptexp_afqt ptexp2_afqt, cluster(id) absorb(year)
eststo lm2

areg lnwage black ptexp ptexp2 ptexp_black ptexp2_black std_afqt std_afqt2 soc_nlsy2_std2 noncog_std2 ptexp_afqt ptexp2_afqt ptexp_afqt2 ptexp2_afqt2, cluster(id) absorb(year)
eststo lm3


esttab lm1 lm2 lm3 using "Table_3.tex", varwidth(50) se compress ///
star(* 0.1 ** 0.05 *** 0.01) replace label ///
mtitle("log wages" "log wages" "log wages") ///
stats(r2 N, fmt( %4.2f %12.0fc))



*** Heckman Tables 
heckman lnwage2 black ptexp_black ptexp2_black std_afqt afqt_std2 soc_nlsy2_std2 noncog_std2 ptexp ptexp2 ptexp_afqt ptexp2_afqt i.year, select(emp2=black ptexp_black ptexp2_black std_afqt afqt_std2 soc_nlsy2_std2 noncog_std2 ptexp ptexp2 ptexp_afqt ptexp_afqt2 ptexp2_afqt lag_emp2 i.year i.urban2 i.region2) cluster(id) 

* Ptexp interacted with afqt^2
areg lnwage black ptexp_black ptexp2_black afqt_std afqt_std2 soc_nlsy2_std2 noncog_std2 ptexp ptexp2 ptexp_afqt ptexp_afqt_post ptexp2_afqt_post ptexp2_afqt ptexp_afqt2 ptexp2_afqt2, cluster(id) absorb(year)
heckman lnwage black ptexp_black ptexp2_black afqt_std afqt_std2 soc_nlsy2_std2 noncog_std2 ptexp ptexp2 ptexp_afqt ptexp2_afqt ptexp_afqt2 ptexp2_afqt2 i.year, select(emp2=black ptex fp_black ptexp2_black afqt_std afqt_std2 soc_nlsy2_std2 noncog_std2 ptexp ptexp2 ptexp_afqt ptexp_afqt2 ptexp2_afqt lag_emp2 i.year i.urban2 i.region2) cluster(id) 
