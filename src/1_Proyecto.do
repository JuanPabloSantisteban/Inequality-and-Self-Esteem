

************************
**** 1.Load Data *******
************************
cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Inequality-and-Self-Esteem-src"

** 1.1 Import GDP Data**

import delimited GDP_Data.csv, clear
drop in 1/3
keep v1 v2 v64
rename v1 country
rename v2 ISO3C
rename v64 GDP_PC

save GDP_Data.dta, replace


** 1.2 Import WID Data **



** Load data **
// wid, indicators(shweal sptinc ) perc(p90p100 p99p100 p0p50 ) years(2019) ages(992) pop(j) clear
// save WID_DATA.dta


use  WID_DATA.dta, clear
egen new_var = concat(variable percentile)
drop variable percentile year age pop
reshape wide value, i(country) j(new_var) string

** Rename variable names **
rename valuesptinc992jp99p100 income_1
rename valuesptinc992jp90p100 income_10
rename valuesptinc992jp0p50 income_bottom50
rename valueshweal992jp99p100 wealth_1
rename valueshweal992jp90p100 wealth_10
rename valueshweal992jp0p50 wealth_bottom50

** Multiply by 100 the variable names **
replace income_1 = income_1 * 100
replace income_10 = income_10 * 100
replace income_bottom50 = income_bottom50 * 100

replace wealth_1 = wealth_1 * 100
replace wealth_10 = wealth_10 * 100
replace wealth_bottom50 = wealth_bottom50 * 100

** Change country to the format name **
kountry country, from(iso2c) to(iso3c)
rename _ISO3C_ ISO3C

save WID_DATA_proccesed.dta, replace





** 1.3 Import SWIID Inequality Data ***

import delimited swiid9_8_summary.csv, clear
kountry country, from(other) stuck marker
rename _ISO3N_ ISO3N
kountry ISO3N, from(iso3n) to(iso3c)
rename _ISO3C_ ISO3C
keep if year == 2017
keep if MARKER == 1

save SWIID.dta, replace

** 1.4 Import VDem Data**
use  "Vdem.dta", clear
keep v2x_libdem v2x_polyarchy COWcode year
keep if year == 2019
kountry COWcode, from(cown) to(iso3c)
rename _ISO3C_ ISO3C

save VDEM_NEW.dta, replace

** 1.5 Import WVS data**

use "WVS_Cross-National_Wave_7_stata_v6_0 2.dta", clear 
rename B_COUNTRY_ALPHA ISO3C

** 1.6 Merge Data**

joinby ISO3C using WID_DATA.dta, unmatched(master)
drop _merge
joinby ISO3C using GDP_Data.dta, unmatched(master)
drop _merge
joinby ISO3C using SWIID.dta, unmatched(master)
drop _merge
joinby ISO3C using VDEM_NEW.dta, unmatched(master)


************************
**** 2.Clean Data ******
************************

** 2.1 Keep observations that actually responded **

keep if Q48 >= 0 & Q275 >= 0 & Q288 >= 0


** 2.2 Change the original variable names to understandable names**

quietly{
rename Q48 freedom_choice
rename Q275 nivel_educacion
rename Q275R nivel_educacion_class // recoded into 3 groups: Lower, Middle, Upper
rename Q288 subjective_income
rename Q288R subjective_income_class  // recoded into 3 groups: Lower, Middle, Upper
}


** 2.3 Create variables for persons who correctly identify themselves in the social-class scale**
quietly{
generate class_placement = "true" if nivel_educacion_class == subjective_income_class
//replace class_placement = "false" if nivel_educacion_class != subjective_income_class 
replace class_placement = "optimist" if nivel_educacion_class < subjective_income_class 
replace class_placement = "pessimist" if nivel_educacion_class > subjective_income_class 

}


**2.4 Change variable label **
quietly{
label variable nivel_educacion "Social class"
}

**2.5 Log and devide GPD per gapita**

generate lnGDPpercap1 = ln(GDPpercap1)
generate GDPpercap1_10k = (GDPpercap1)/10000


** 2.6 Add Latin America **

	   
gen byte latin_america =                                      ///
    inlist(ISO3C, "ARG","BOL","BRA","CHL","COL","CRI","CUB","DOM") ///
  | inlist(ISO3C, "ECU","SLV","GTM","HND","HTI","MEX","NIC","PAN") ///
  | inlist(ISO3C, "PRY","PER","URY","VEN")


gen mobility = nivel_educacion - Q278 if Q262 >= 30
  
*****************************
**** 3. Use Treated Data ****
*****************************

save master.dta, replace
use master.dta, clear


binsreg Q49 freedom_choice if Q49>0 & freedom_choice>0, ///
    nbins(10)                                  /// 10 bins
    dots(0) ci(0) cb(0)                        /// omit dots, CIs and band
    polyreg(1)                                 /// global linear fit
    polyregplotopt(lwidth(medium) lpattern(dash))  /// style the fit
    ///
    title("Higher self-esteem is correlated with high life satisfaction")      /// main title
    subtitle("Global linear fit through binscatter")  /// optional subtitle
    xtitle("Self-Esteem Score ")               /// x-axis label
    ytitle("Life Satisfaction (Wellness)")               /// y-axis label
    xlabel(1(1)10, grid angle(45))                  /// ticks at 1–10 with gridlines
    ylabel(4(1)10, grid)                            /// same for y
    legend(off)                                    /// turn off legend if you like
	note("Note: Graph elaborated by the author based on WVS data. N = 92,689 ", ///
         size(small) position(6))

  
  


*****************************
**** 4. Descriptive Data ****
*****************************

** 4.1 Sankey Diagram 3 class to 3 class**

preserve

contract nivel_educacion_class subjective_income_class
list, sepby(nivel_educacion_class)

sankey _freq , from(nivel_educacion_class) to(subjective_income_class)  ctitles("Social-Class" "Subjective Social Class") cts(2.5) ctg(900) ctpos(bot) offset(10) title("People around the world tend to missplace themselves as middle class", size(3)) showtot 

restore





preserve

quietly{
gen pct_true = class_placement == "true"
gen pct_pess = class_placement == "pessimist"
gen pct_opti = class_placement == "optimist"
	
collapse (mean) pct_true pct_pess pct_opti, by(B_COUNTRY)
format pct_true pct_pess pct_opt %6.2f
}
* List of countries that are more "Class Aware"
gsort -pct_true
list B_COUNTRY pct_true pct_pess pct_opt, noobs




** Scatterplot of Polyarchy Index vs Gini Index
/*twoway (scatter v2x_polyarchy gini_disp if v2x_polyarchy > 0 & gini_disp > 0, ///
        mlabel(ISO3C) mlabsize(small)) ///
       (lfit v2x_polyarchy gini_disp if v2x_polyarchy > 0 & gini_disp > 0), ///
       title("Scatterplot with Regression Line") ///
       xtitle("Gini Index (gini_disp)") ///
       ytitle("Polyarchy Index (v2x_polyarchy)")
graph save PolyArch_vs_gini_disp.gph
*/
graph use PolyArch_vs_gini_disp.gph


	   
* Scatterplot of Liberal Democracy Index vs Gini Index
/*twoway (scatter v2x_libdem gini_disp if v2x_libdem > 0 & gini_disp > 0, ///
        mlabel(ISO3C) mlabsize(small)) ///
       (lfit v2x_libdem gini_disp if v2x_libdem > 0 & gini_disp > 0), ///
       title("Scatterplot with Regression Line") ///
       xtitle("Gini Index (gini_disp)") ///
       ytitle("Liberal Democracy Index (v2x_libdem)")
graph save LibDem_vs_gini_disp.gph
*/
graph use LibDem_vs_gini_disp.gph


	
	
* Scatterplot of Liberal Democracy Index vs Top 10% Income Share	  
/*twoway (scatter v2x_libdem income_10 if v2x_libdem > 0 & income_10 > 0, ///
        mlabel(ISO3C) mlabsize(small)) ///
       (lfit v2x_libdem income_10 if v2x_libdem > 0 & income_10 > 0), ///
       title("Scatterplot: Liberal Democracy vs Top 10% Income Share") ///
       xtitle("Top 10% Income Share (income_10)") ///
       ytitle("Liberal Democracy Index (v2x_libdem)")
graph save democracy_vs_income10.gph
*/

graph use democracy_vs_income10.gph


/* Prueba
regress freedom_choice nivel_educacion_class if ISO3C=="USA"
scalar slope_USA = _b[nivel_educacion_class]

summarize gini_disp if ISO3C=="USA", meanonly
scalar gini_USA = r(mean)


regress freedom_choice nivel_educacion_class if ISO3C=="MEX"
scalar slope_MEX = _b[nivel_educacion_class]

summarize gini_disp if ISO3C=="MEX", meanonly
scalar gini_MEX = r(mean)
*/







*****************************
**** 5.Linear Models ********
*****************************


** 5.1 Personal variables simple regression **


keep if = 

quietly{
regress freedom_choice nivel_educacion_class if class_placement == "true" 
estimates store ClassAwareness

regress freedom_choice nivel_educacion_class if class_placement == "optimist" 
estimates store optimists

regress freedom_choice nivel_educacion_class if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice nivel_educacion_class 
estimates store WithoutDistinction
}

etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal Variables ") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )


** 5.1.2 Other personal variables simple regression **

quietly{
regress freedom_choice nivel_educacion_class subjective_income_class if class_placement == "true" 
estimates store ClassAwareness

regress freedom_choice nivel_educacion_class subjective_income_class if class_placement == "optimist" 
estimates store optimists

regress freedom_choice nivel_educacion_class subjective_income_class if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice subjective_income_class nivel_educacion_class
estimates store WithoutDistinction
}


etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal Variables ") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )

** 5.2 Country & Personal variables simple regression **

quietly{
regress freedom_choice nivel_educacion_class lnGDPpercap1 income_10 wealth_10 gini_disp gini_mkt if class_placement == "true"
estimates store ClassAwareness

regress freedom_choice nivel_educacion_class lnGDPpercap1 income_10 wealth_10 gini_disp gini_mkt if class_placement == "optimist"
estimates store optimists

regress freedom_choice nivel_educacion_class lnGDPpercap1 income_10 wealth_10 gini_disp gini_mkt if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice nivel_educacion_class lnGDPpercap1 income_10 wealth_10 gini_disp gini_mkt
estimates store WithoutDistinction
}
etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal & Personal Variables") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )


encode ISO3C, gen(country_id)
regress freedom_choice  subjective_income_class i.country_id, vce(cluster country_id)

regress freedom_choice subjective_income_class i.country_id if class , vce(cluster country_id)

** 5.2 Country & Personal variables simple regression **

quietly{
regress freedom_choice subjective_income_class i.country_id if class_placement == "true"
estimates store ClassAwareness

regress freedom_choice subjective_income_class i.country_id if class_placement == "optimist"
estimates store optimists

regress freedom_choice subjective_income_class i.country_id if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice subjective_income_class i.country_id
estimates store WithoutDistinction
}
etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal & Personal Variables") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )


quietly{
regress freedom_choice subjective_income_class if class_placement == "true"
estimates store ClassAwareness

regress freedom_choice subjective_income_class if class_placement == "optimist"
estimates store optimists

regress freedom_choice subjective_income_class if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice subjective_income_class nivel_educacion_class
estimates store WithoutDistinction
}
etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal & Personal Variables") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )







quietly{
regress freedom_choice subjective_income_class humanineqiality if class_placement == "true"
estimates store ClassAwareness

regress freedom_choice subjective_income_class lnGDPpercap1 gini_disp if class_placement == "optimist"
estimates store optimists

regress freedom_choice subjective_income_class lnGDPpercap1 gini_disp if class_placement == "pessimist"
estimates store pessimists

regress freedom_choice subjective_income_class lnGDPpercap1 gini_disp
estimates store WithoutDistinction
}
etable, estimates(ClassAwareness optimists pessimists WithoutDistinction) showstars showstarsnote  title("Control over your lives - Personal & Personal Variables") novarlabel column(estimates) cstat(_r_b) cstat(_r_p, )

regress nivel_educacion_class lnGDPpercap1 income_10 if class_placement == "true"


replace humanineqiality = . if humanineqiality == -9999


mixed freedom_choice c.subjective_income##c.gini_disp gini_disp || B_COUNTRY:
margins, dydx(subjective_income) at(gini_disp=(22 32 42 52))
marginsplot


mixed freedom_choice c.subjective_income##c.humanineqiality humanineqiality || B_COUNTRY:
margins, dydx(subjective_income) at(humanineqiality=(4 8 12 16 20 24 28 32 36 40 44 48 52 56))
marginsplot






** 5.3 MIXED EFFECTS - Class and Gini Disp **
mixed freedom_choice c.subjective_income##c.lnGDPpercap1 lnGDPpercap1 || B_COUNTRY: if class_placement == "true"
margins, dydx(subjective_income) at(lnGDPpercap1=(8.6 9.11 9.50 9.65 10.0 10.13))
marginsplot

** 5.3 MIXED EFFECTS - Class and Gini Disp **
mixed freedom_choice i.nivel_educacion_class##c.gini_disp || B_COUNTRY: if class_placement == "true"
margins, at(gini_disp=(22 32 42 52))
marginsplot

** 5.4 MIXED EFFECTS - Class and Income_1
mixed freedom_choice i.nivel_educacion_class##c.humanineqiality || B_COUNTRY:
margins, at(humanineqiality=(4 8 12 16 20 24 28 32 36 40 44 48 52 56))
marginsplot

** 5.5 MIXED EFFECTS - Class and Income_10
mixed freedom_choice i.nivel_educacion_class##c.income_1 || B_COUNTRY: if class_placement == "true"
margins, at(income_1=(22 32 42 52))
marginsplot

** 5.6 MIXED EFFECTS - Class and Income_Bottom50
mixed freedom_choice i.nivel_educacion_class##c.income_bottom50 || B_COUNTRY: if class_placement == "true"
margins, at(income_bottom50=(0 20 40 60))
marginsplot



***********************************
******* 7. Primeras Graficas ******
***********************************


*— 1. Subjective income

levelsof ISO3C, local(countries)

tempname handle
tempfile results
postfile `handle' ///
    str32 ISO3C ///
    double coef se lnGDPpercap1 ///
    using "`results'", replace

foreach c of local countries {
    preserve
        keep if ISO3C == "`c'"
        quietly regress freedom_choice subjective_income
        
        * grab slope and its SE
        local b = _b[subjective_income]
        local s = _se[subjective_income]
        * grab the country's gini_disp (assumes one value per country)
        local g = lnGDPpercap1[1]
        
        post `handle' ("`c'") (`b') (`s') (`g')
    restore
}


postclose `handle'


use "`results'", clear

generate beta_ub = coef + 1.96 * se
generate beta_lb = coef - 1.96 * se

rename coef beta_subj_inc
  
  

twoway ///
  (rcap  beta_ub    beta_lb    lnGDPpercap1)                                    ///  // 95% CI bars
  (scatter beta_subj_inc lnGDPpercap1, ///
       msymbol(circle) msize(medium) ///
       mlabel(ISO3C) mlabposition(0) mlabsize(small) mlabcolor(black))                     ///  // point estimates + labels
  (lfit    beta_subj_inc    lnGDPpercap1)                                       ///  // fitted regression line
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("GDP per Cap") ///
  ytitle("Slope on Subjective Income") ///
  title("Subjective-Income Effect vs. Inequality (with Country Labels)")
  
  
**--



levelsof ISO3C, local(codes)

* 1.2 Prepare a temp "postfile" for results
tempname handle
tempfile results
postfile `handle' ///
    str3   ISO3C   ///  <-- change here
    double coef se gini_disp ///
    using "`results'", replace

* 1.3 Loop over each ISO3C
foreach c of local codes {
    preserve
        keep if ISO3C == "`c'"
        quietly regress freedom_choice subjective_income_class

        * capture coefficient & SE for nivel_educacion_class
        local b = _b[subjective_income_class]
        local s = _se[subjective_income_class]
        * assume one gini_disp per ISO3C
        local g = gini_disp[1]

        post `handle' ("`c'") (`b') (`s') (`g')
    restore
}

postclose `handle'

* 1.4 Load the assembled results
use "`results'", clear

* 1.5 (Optional) rename for clarity
rename coef    beta_nivel
rename se      se_nivel

***************
* 2. Build 95% CI bounds
***************
generate nivel_ub = beta_nivel + 1.96 * se_nivel
generate nivel_lb = beta_nivel - 1.96 * se_nivel

***************
* 3. Scatter + error bars + fit line + ISO3C labels
***************
twoway ///
  (scatter beta_nivel gini_disp, ///
       msymbol(circle) msize(medium) ///
       mlabel(ISO3C) mlabposition(0) mlabsize(small) mlabcolor(black))       ///
  (lfit beta_nivel gini_disp)                                           ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Gini Index") ///
  ytitle("Slope on Education Class") ///
  title("Social-Class Effect vs. Inequality (with ISO3C Labels)")




****** THIRD TRY *****


* 1.1 Get list of ISO3C codes
levelsof ISO3C, local(codes)

* 1.2 Prepare a temp "postfile" for results
tempname handle
tempfile results
postfile `handle' ///
    str3   ISO3C   ///
    double coef se income_10 ///
    using "`results'", replace

* 1.3 Loop over each ISO3C
foreach c of local codes {
    preserve
        keep if ISO3C == "`c'"
        quietly regress freedom_choice nivel_educacion_class

        * capture coefficient & SE for nivel_educacion_class
        local b = _b[nivel_educacion_class]
        local s = _se[nivel_educacion_class]
        * capture that country's income_10 (assumed constant)
        local inc = income_10[1]

        post `handle' ("`c'") (`b') (`s') (`inc')
    restore
}

postclose `handle'

* 1.4 Load the assembled results
use "`results'", clear

* 1.5 (Optional) rename for clarity
rename coef    beta_nivel
rename se      se_nivel
rename income_10 country_income

***************
* 2. Build 95% CI bounds for the slope
***************
generate nivel_ub = beta_nivel + 1.96 * se_nivel
generate nivel_lb = beta_nivel - 1.96 * se_nivel

***************
* 3. Scatter + error bars + fit line + ISO3C labels
***************
twoway ///
  (rcap  nivel_ub    nivel_lb    country_income)                             ///  // 95% CI bars
  (scatter beta_nivel country_income, ///
       msymbol(circle) msize(medium) ///
       mlabel(ISO3C) mlabposition(0) mlabsize(small) mlabcolor(black))     ///  // points + ISO3C labels
  (lfit    beta_nivel    country_income)                                     ///  // fitted regression line
, ///
  xlabel(, grid) ylabel(, grid) ///
xtitle("Income Decile (income_10)")
  ytitle("Slope on Education Class") ///
  title("Education-Class Effect vs. Income (with ISO3C Labels)")  
  
  
*****************************
******* 8.Grafica ***********
*****************************

bysort nivel_educacion_class: egen mean_true  = mean(cond(class_placement=="true",  freedom_choice, .))
bysort nivel_educacion_class: egen mean_false = mean(cond(class_placement=="false", freedom_choice, .))

sort nivel_educacion_class

twoway ///
  (line    mean_true  nivel_educacion_class, lwidth(medium) lpattern(solid)) ///
  (line    mean_false nivel_educacion_class, lwidth(medium) lpattern(dash))  ///
  (scatter mean_true  nivel_educacion_class, msymbol(circle)  msize(medium)) ///
  (scatter mean_false nivel_educacion_class, msymbol(diamond) msize(medium)), ///
  xlabel(1 2 3, valuelabel) ///
  legend(order(1 "Correctly Placed" 2 "Incorrectly Placed") ///
         ring(0) pos(6)) ///
  xtitle("Social Class Perception") ///
  ytitle("Mean Freedom of Choice") ///
  title("Mean Freedom of Choice by Class and Placement")
  

**Trust more their subjective placement**
  
bysort subjective_income: egen mean_true1  = mean(cond(class_placement=="true",  freedom_choice, .))
bysort subjective_income: egen mean_false2 = mean(cond(class_placement=="false", freedom_choice, .))

sort subjective_income

twoway ///
  (line    mean_true1  subjective_income, lwidth(medium) lpattern(solid)) ///
  (line    mean_false2 subjective_income, lwidth(medium) lpattern(dash))  ///
  (scatter mean_true1  subjective_income, msymbol(circle)  msize(medium)) ///
  (scatter mean_false2 subjective_income, msymbol(diamond) msize(medium)), ///
  legend(order(1 "Correctly Placed" 2 "Incorrectly Placed") ///
         ring(0) pos(6)) ///
  xtitle("Social Class Perception") ///
  ytitle("Mean Freedom of Choice") ///
  title("Mean Freedom of Choice by Class and Placement")


