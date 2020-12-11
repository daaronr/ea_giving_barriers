clear all
cd "C:\Users\Janek\Desktop\Novemer_Pilot\Main_analysis"
graph set window fontface "Times New Roman"
import excel "main study_December 3_patched.xlsx", firstrow 
keep ID Charity_A_Control-Charity_B_Impact Effectiveness_A-Most_effective facts help_many-compare_impact Money_pledge Impact_pledge


* var cleaning
foreach var in Charity_A_Control Charity_B_Control Charity_A_Impact	Charity_B_Impact Charity_A_Money Charity_B_Money
facts Money_pledge Impact_pledge {
destring `var',replace force
}

foreach var in Effectiveness_A Effectiveness_B {
gen temp_`var' = subinstr(`var'," days","",.)
destring temp_`var',replace force
drop `var'
rename temp_`var' `var'
}

foreach var in BB BC BD{
replace `var'  = substr(`var',-2,1)
destring `var',replace force
}

replace AE = "1" if AE=="Charity A"
replace AE = "2" if AE=="Charity B"
replace AE = "3" if AE=="I do not know / no opinion"
replace AE = "4" if AE=="Both are equally cost-effective"
destring AE,replace force


destring Normalized,replace force
rename Normalized impactpledged_conv
rename MoneyPledge moneypledged
replace risk = subinstr(risk,"Fully willing to take risk 10","10",.)
replace risk = subinstr(risk,"Unwilling to take risk 1","1",.)
destring risk, replace force

replace household_income = subinstr(household_income,"under 20,000","1",.)
replace household_income = subinstr(household_income,"20,000-39,999","2",.)
replace household_income = subinstr(household_income,"40,000-59,999","3",.)
replace household_income = subinstr(household_income,"60,000-99,999","4",.)
replace household_income = subinstr(household_income,"100,000-149,999","5",.)
replace household_income = subinstr(household_income,"150,000-249,999","6",.)
replace household_income = subinstr(household_income,"+250,000","7",.)
destring household_income, replace force


* analysis
hist moneypledged, name(ac, replace)
hist impactpledged_conv, name(ab, replace)
graph combine ac ab, name(abc, replace) plotregion(fcolor(white)) graphregion(fcolor(white)) graphregion(color(white) lwidth(large))
graph save abc "hist.png",replace

sum moneypledged impactpledged_conv,det
graph box moneypledged impactpledged_conv, name(gerpge,replace)
graph save gerpge "boxt.png",replace


log using "R1.smcl",replace

sum
reg impactpledged_conv detla_happy detla_strong detla_upset detla_active detla_proud detla_enthusiastic detla_guilty detla_ashamed risk household_income,r

reg moneypledged detla_happy detla_strong detla_upset detla_active detla_proud detla_enthusiastic detla_guilty detla_ashamed risk household_income,r

gen pledge = moneypledged
replace pledge = impactpledged_conv if pledge==.

gen type = 1 if moneypledged!=.
replace type = 2 if moneypledged==.

reg pledge i.type##c.detla_happy i.type##c.detla_strong i.type##c.detla_upset i.type##c.detla_active i.type##c.detla_proud i.type##c.detla_enthusiastic i.type##c.detla_guilty i.type##c.detla_ashamed risk household_income,r

log close
translate "R1.smcl" "R1.pdf",replace

export excel using "NEW_R1.xls", replace
