****************************************************
* requirements.do
* Records environment for reproducibility
****************************************************

display "Replication environment: Stata 17.0, macOS"

set more off
set matsize 10000

* Install required packages if missing
capture which kountry
if _rc ssc install kountry, replace

capture which binsreg
if _rc ssc install binsreg, replace

capture which binsreg
if _rc ssc install binsreg, replace

sankey _freq , from(nivel_educacion_class) to(subjective_income_class) ...




