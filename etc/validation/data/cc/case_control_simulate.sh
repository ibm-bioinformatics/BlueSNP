#!/bin/sh

name="case_control"

# simulate
plink --simulate settings.sim --simulate-ncases 1000 --make-bed --out $name

# recode as 0,1,2 format 
plink --bfile $name --recodeA --out $name

