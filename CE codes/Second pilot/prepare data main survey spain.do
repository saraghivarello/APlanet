cd "C:\Users\ash\Transportøkonomisk institutt\A-PLANET - General\WP 2\"


*****************************************************************************

*** BACKGROUND VARIABLES (without full social norms part) ***

import delimited "C:\Users\ash\Transportøkonomisk institutt\A-PLANET - General\WP 2\Data\Main survey Spain\Part3.csv", clear
*drop playeralternative1-GJ
renpfix player
sort participantcode participantid subsessionround_number
keep if subsessionround_number==4

save "Data\Main survey Spain\Part3_1line.dta", replace

import delimited "C:\Users\ash\Transportøkonomisk institutt\A-PLANET - General\WP 2\Data\Main survey Spain\Part1.csv", clear
renpfix player

merge 1:1 participantcode using "Data\Main survey Spain\Part3_1line.dta"
drop _merge

save "Data\Main survey Spain\backgroundvars.dta", replace

*****************************************************************************

*** CHOICE EXPERIMENT DATA ***

import delimited "C:\Users\ash\Transportøkonomisk institutt\A-PLANET - General\WP 2\Data\Main survey Spain\Part2.csv", clear

* Drop superfluous variables *
*drop participantlabel-playerlanguage playerpayoff groupid_in_subsession sessioncode-sessionis_demo

sort participantcode participantid_in_session subsessionround_number

renpfix player

drop if policy_choice==.|policy_choice==0

* Reshaping to three alternatives per choice, adding opt-out option *
reshape long price_ur price_un price_o ev revenue, i(participantcode participantid_in_session subsessionround_number) j(alternative) string

expand 2 if alternative=="_b", gen(dupli)

gen alt=1
replace alt=2 if alternative=="_b"&dupli==0
replace alt=3 if alternative=="_b"&dupli==1
drop alternative dupli
sort participantid_in_session subsessionround_number alt

foreach var in price_ur price_un price_o ev revenue {
	replace `var'=0 if alt==3
}

* Generating variables with attibute values (not levels) *
* Note that prices are somewhat higher than they were supposed to be *
foreach var in price_ur price_un price_o ev revenue {
	gen level_`var'=`var'
}

replace price_ur=0.05 if level_price_ur==1
replace price_ur=0.1 if level_price_ur==2
replace price_ur=0.3 if level_price_ur==3
replace price_ur=0.5 if level_price_ur==4

replace price_un=0.05 if level_price_un==1
replace price_un=0.1 if level_price_un==2
replace price_un=0.2 if level_price_un==3
replace price_un=0.3 if level_price_un==4

replace price_o=0.01 if level_price_o==1
replace price_o=0.02 if level_price_o==2
replace price_o=0.03 if level_price_o==3
replace price_o=0.05 if level_price_o==4

replace ev=0 if level_ev==1
replace ev=0.25 if level_ev==2
replace ev=0.5 if level_ev==3
replace ev=1 if level_ev==4

lab def revenue 1 "General budget" 2 "Equal cash transfer" 3 "Low income citizens" 4 "Investments in roads" 5 "Public transport, walking and cycling", replace
lab val revenue revenue

* Binary outcome variable *
gen choice=policy_choice==alt

* Dummy for choosing one of the two policy alternatives *
gen choice_policy=policy_choice!=3

* Constant term for the two policy alternatives *
gen policy=alt!=3
gen policy1=alt==1
gen policy2=alt==2

* Constant term for left(?) policy alternative *
gen left=alt==1

* Dummy variables for revenue attribute *
tab revenue if revenue>0, gen(d_rev)
recode d_rev* (.=0)

order participantcode participantid_in_session subsessionround_number policy_choice alt choice policy, before(price_ur)

* Exporting dataset *
export delimited using "Data\Main survey Spain\CE policy restructured.csv", delimiter(tab) replace
export excel using "Data\Main survey Spain\CE policy restructured.xls", firstrow(var) replace

egen id_num=group(participantcode)

* Declaring panel structure of dataset *
cmset id_num subsessionround_number alt

*****************************************************************************

*** COMBINED DATA SET ***

merge m:1 participantid_in_session participantcode using "Data\Main survey Spain\backgroundvars.dta", keep(match master)

save "Data\Main survey Spain\CE policy and backgroundvars.dta", replace
