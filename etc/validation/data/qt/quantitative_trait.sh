name="quantitative_trait"

## simulate
plink --simulate-qt settings.sim --simulate-ncases 1000 --make-bed --out $name

# recode as 0,1,2 format 
plink --bfile $name --recodeA --out $name

# linear regression
plink --bfile $name --assoc --out linreg

# maxT permutation
plink --bfile $name --linear --tdt --mperm 100 --out maxT
