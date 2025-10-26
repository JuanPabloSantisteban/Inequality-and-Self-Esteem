

************************
**** 1.Load Directory ****
************************
cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"


*****************************
**** 2. Load Data ****
*****************************

use master.dta, clear

encode ISO3C, gen(country_id)

*****************************
****3. Graphs  **************
*****************************
		 
** First graph **

binsreg Q49 freedom_choice if Q49>0 & freedom_choice>0, ///
    nbins(10) dots(0) ci(0) cb(0) polyreg(1) ///
    polyregplotopt(lwidth(medium) lpattern(dash)) ///
    title("Higher self-esteem is correlated with higher life satisfaction", size(2.8)) ///
    subtitle("Linear fit through binscatter", size(1.5)) ///
    xtitle("Self-Esteem Score") ///
    ytitle("Life Satisfaction") ///
    xlabel(1(1)10, grid angle(45) labsize(2.5)) ///
    ylabel(4(1)10, grid) ///
    legend(off) ///
    note("Note: Graph elaborated by the author based on WVS data. N = 92,689", ///
         size(1.2) position(6)) ///
    aspectratio(1) xsize(6) ysize(6) ///
    name(bins1, replace)
	
scatter Q49 freedom_choice if Q49>0 & freedom_choice>0


** Second graph **

regress freedom_choice nivel_educacion_class subjective_income_class i.country_id, vce(cluster country_id)

coefplot, ///
    keep(nivel_educacion_class subjective_income_class) ///
    rename( ///
        nivel_educacion_class   = "Objective Social Class" ///
        subjective_income_class = "Subjective Social Class" ///
    ) ///
    yscale(reverse) ///
    xlabel(, grid) ///
    ylabel(, angle(horizontal) labsize(small)) /// shrink labels
    xtitle("β̂ on Self-Esteem") /// <— here's your x‐axis label
    title("Effect of Objective and Subjective Class on Self‐Esteem", justification(left) margin(l-30) ) ///
    subtitle("With country fixed effects", justification(left) margin(l-30)) ///
    legend(off) ///
    scheme(s2color) ///
    graphregion(margin(l-5)) ///
	note("Note: Graph elaborated by author based on WVS data. N = 92,892", size(1.5) position(6)) ///
	plotregion(color(white)) ///
    graphregion(color(white))



************************************************************
******** 4. Subjective-GINI Coefficient Effects ************
************************************************************

cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear

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
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black))       ///
  (lfit beta_nivel gini_disp)                                           ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Gini Index (Inequality)") ///
  ytitle("β̂ on Self-Esteem") ///
  title("Subjective Social-Class effect on Self-Esteem is less salient on more unequal countries ", ///
        size(3.3) margin(l-3.5)) ///
  graphregion(color(white)) ///
  plotregion(color(white)) ///
  legend(off)
  cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Imagenes Tesina"
  graph export  Subjective-GINI Coefficient Effects", width(2000) replace

  
  
************************************************************
******** 4.1 Subjective-GINI Coefficient Effects WITH MOBILITY ************
************************************************************

cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear

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
        quietly regress freedom_choice subjective_income_class mobility

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
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black))       ///
  (lfit beta_nivel gini_disp)                                           ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Gini Index (Inequality)") ///
  ytitle("β̂ on Self-Esteem") ///
  title("Subjective Social-Class effect on Self-Esteem is less salient on more unequal countries ", ///
        size(3.3) margin(l-3.5)) ///
  graphregion(color(white)) ///
  plotregion(color(white)) ///
  legend(off)
  
  
  
**************************************************************
******** 5. Subjective-Income_10 Coefficient Effects *********
**************************************************************

cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear  

levelsof ISO3C, local(codes)

* 1.2 Prepare a temp "postfile" for results
tempname handle
tempfile results
postfile `handle' ///
    str3   ISO3C   ///  <-- change here
    double coef se income_10 ///
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
        local g = income_10[1]

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
  (scatter beta_nivel income_10, ///
       msymbol(circle) msize(medium) ///
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black)) ///
  (lfit beta_nivel income_10, ///
       lpattern(solid) lwidth(medium)) ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Top 10% income Share") ///
  ytitle("β̂ on Self-Esteem") ///
  title(  ///
    "Subjective Social-Class effect on Self-Esteem is less salient on more unequal countries",  ///
    size(3.3) margin(l-5.5)                              ///
  ) ///
  legend(off) ///
  graphregion(color(white)) ///
  plotregion(color(white))
  cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Imagenes Tesina"
  graph export "Subjective-Income_10 Coefficient Effects.png", width(2000) replace

  
  
  
  
****************************************************************
******** 6. Subjective-humanineqiality Coefficient Effects *****
****************************************************************
  
cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear
keep if humanineqiality > 0


levelsof ISO3C, local(codes)

* 1.2 Prepare a temp "postfile" for results
tempname handle
tempfile results
postfile `handle' ///
    str3   ISO3C   ///  <-- change here
    double coef se humanineqiality ///
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
        local g = humanineqiality[1]

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
  (scatter beta_nivel humanineqiality, ///
       msymbol(circle) msize(medium) ///
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black)) ///
  (lfit beta_nivel humanineqiality, ///
       lpattern(solid) lwidth(medium)) ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Coefficient of human inequality [UNDP, 2016-2018] ") ///
  ytitle("β̂ on Self-Esteem") ///
  title(  ///
    "Subjective Social-Class effect on Self-Esteem is less salient on more unequal countries",  ///
    size(3) margin(l-6.5)                              ///
  ) ///
  legend(off) ///
  graphregion(color(white)) ///
  plotregion(color(white))
  cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Imagenes Tesina"
  graph export "Subjective-humanineqiality Coefficient Effects.png", width(2000) replace

  
  
*******************************************************************************
******** 7. Subjective-Gini_Disp Coefficient Effects in Free Countries ********
*******************************************************************************

cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear
keep if polregfh > 1

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
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black))       ///
  (lfit beta_nivel gini_disp)                                           ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Gini Index (Inequality)") ///
  ytitle("β̂ on Self-Esteem") ///
  title("Effect on Self-Esteem (Only Free and Partly Free Countries) ", ///
        size(3.3) margin(l-3.5)) ///
  graphregion(color(white)) ///
  plotregion(color(white)) ///
  legend(off)
  cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Imagenes Tesina"
  graph export "Subjective-Gini_Disp Coefficient Effects in Free Countries.png", width(2000) replace

  

  
*****************************************************************************************
******** 8. Subjective-Gini_Disp Coefficient Effects in Latin American Countries ********
*****************************************************************************************

cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Tesina Stata"
use master.dta, clear
keep if latin_america == 1



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
       mlabel(ISO3C) mlabposition(2) mlabsize(vsmall) mlabcolor(black))       ///
  (lfit beta_nivel gini_disp)                                           ///
, ///
  xlabel(, grid) ylabel(, grid) ///
  xtitle("Gini Index (Inequality)") ///
  ytitle("β̂ on Self-Esteem") ///
  title("Effect on Self-Esteem (Latin American Countries) ", ///
        size(3.3) margin(l-3.5)) ///
  graphregion(color(white)) ///
  plotregion(color(white)) ///
  legend(off)
  cd "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Imagenes Tesina"
  graph export "Subjective-Gini_Disp Coefficient Effects in Latin American Countries.png", width(2000) replace
  






*******************************************************************************************************

** 5.3 MIXED EFFECTS - Class and Gini Disp **
mixed freedom_choice i.subjective_income_class##c.gini_disp || B_COUNTRY:
margins, dydx(subjective_income_class) at(gini_disp=(20 30 40 50))
marginsplot


mixed freedom_choice subjective_income_class gini_disp ln_GDp || B_COUNTRY: gini_disp
margins, dydx(subjective_income_class) at(gini_disp=(20 30 40 50))
marginsplot




* 1. Re–estimate (if needed)
mixed freedom_choice c.subjective_income_class##c.gini_disp || B_COUNTRY:

* 2. Pick your "low/med/high" Gini
summarize gini_disp, detail
local g10 = r(p10)
local g50 = r(p50)
local g90 = r(p90)

* 3. margins at those values over income class
margins , at(gini_disp=(`g10' `g50' `g90')) over(subjective_income_class) predict(xb)

* 4. marginsplot with custom legend labels via plotdimension()
marginsplot , ///
  xdimension(subjective_income_class) ///
  plotdimension( ///
    gini_disp, ///
    labels( ///
      "Gini=`=round(`g10',.1)'" ///
      "Gini=`=round(`g50',.1)'" ///
      "Gini=`=round(`g90',.1)'" ///
    ) ///
  ) ///
  title("Predicted Freedom of Choice by Income Class") ///
  xtitle("Subjective Income Class") ///
  ytitle("Predicted freedom_choice") ///
  legend(rows(1) position(6) ring(0))

  

  
  




****


regress freedom_choice subjective_income_class Q6 if Q6 > 0
