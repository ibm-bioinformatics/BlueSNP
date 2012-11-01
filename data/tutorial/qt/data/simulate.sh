#!/bin/sh

# quantitative trait
plink --noweb --simulate-qt settings_qt --simulate-n 1000 --make-bed --out simulated_qt
plink --noweb --bfile simulated_qt --transpose --recode --out simulated_qt
