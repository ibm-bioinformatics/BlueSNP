#!/bin/sh

# case-control
plink --noweb --simulate settings_cc --simulate-ncases 500 --simulate-ncontrols 500 --make-bed --out simulated_cc
plink --noweb --bfile simulated_cc --transpose --recode --out simulated_cc
# encoding of cases and controls is: cases=2, controls=1

