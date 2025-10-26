****************************************************
* run_all.do
* Master pipeline for "Inequality and Self-Esteem"
****************************************************

version 17.0
clear all
set more off

* 1. Set project directory (edit this line to your real path)
global PROJECT "/Users/gabrielsantisteban/Documents/Escuela/10mo Semestre/Inequality and Self-Esteem"

* 2. Define subdirectories
global RAW        "$PROJECT/data/raw"
global PROC       "$PROJECT/data/processed"
global CODE       "$PROJECT/src"
global OUT_FIGS   "$PROJECT/outputs/figures"
global OUT_TABS   "$PROJECT/outputs/tables"

* 3. Create outputs if missing
capture noisily mkdir "$PROC"
capture noisily mkdir "$OUT_FIGS"
capture noisily mkdir "$OUT_TABS"

* 4. Run your actual scripts
do "$CODE/1_Proyecto.do"
do "$CODE/2_Notebook_Experimental.do"

display "Pipeline complete! Processed data in $PROC, outputs in outputs/"
