cd "C:\Users\ash\Transportøkonomisk institutt\A-PLANET - General\WP 2\"


*****************************************************************************

*** BACKGROUND VARIABLES ***

import excel "Data\Pilot Spain\all_parts.xlsx", sheet("Sheet1") firstrow clear

drop playeralternative1-GJ

save "Data\Pilot Spain\backgroundvars.dta", replace


*****************************************************************************

*** CHOICE EXPERIMENT DATA ***

import excel "Data\Pilot Spain\Aplanet_Part2.xlsx", sheet("Sheet1") firstrow clear

* Drop superfluous variables *
drop participantlabel-playerlanguage playerpayoff groupid_in_subsession sessioncode-sessionis_demo

sort participantid_in_session subsessionround_number

renpfix player

drop if policy_choice==.

* Reshaping to three alternatives per choice, adding opt-out option *
reshape long Price_UR Price_UN Price_O EV Revenue, i(participantid_in_session subsessionround_number) j(alternative) string

expand 2 if alternative=="_B", gen(dupli)

gen alt=1
replace alt=2 if alternative=="_B"&dupli==0
replace alt=3 if alternative=="_B"&dupli==1
drop alternative dupli
sort participantid_in_session subsessionround_number alt

foreach var in Price_UR Price_UN Price_O EV Revenue {
	replace `var'=0 if alt==3
}

* Generating variables with attibute values (not levels) *
* Note that prices are somewhat higher than they were supposed to be *
foreach var in Price_UR Price_UN Price_O EV Revenue {
	gen Level_`var'=`var'
}

replace Price_UR=0.1 if Level_Price_UR==1
replace Price_UR=0.2 if Level_Price_UR==2
replace Price_UR=0.4 if Level_Price_UR==3
replace Price_UR=0.5 if Level_Price_UR==4

replace Price_UN=0.1 if Level_Price_UN==1
replace Price_UN=0.2 if Level_Price_UN==2
replace Price_UN=0.3 if Level_Price_UN==3
replace Price_UN=0.4 if Level_Price_UN==4

replace Price_O=0.01 if Level_Price_O==1
replace Price_O=0.02 if Level_Price_O==2
replace Price_O=0.03 if Level_Price_O==3
replace Price_O=0.05 if Level_Price_O==4

replace EV=0 if Level_EV==1
replace EV=0.25 if Level_EV==2
replace EV=0.5 if Level_EV==3
replace EV=1 if Level_EV==4

lab def Revenue 1 "General budget" 2 "Equal cash transfer" 3 "Low income citizens" 4 "Investments in roads" 5 "Public transport, walking and cycling", replace
lab val Revenue Revenue

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
tab Revenue if Revenue>0, gen(d_Rev)
recode d_Rev* (.=0)

order participantid_in_session participantcode subsessionround_number policy_choice alt choice policy, before(Price_UR)

* Exporting dataset *
export delimited using "Data\Pilot Spain\CE policy restructured.csv", delimiter(tab) replace
export excel using "Data\Pilot Spain\CE policy restructured.xls", firstrow(var) replace

* Declaring panel structure of dataset *
cmset participantid_in_session subsessionround_number alt

*****************************************************************************

*** COMBINED DATA SET ***

merge m:1 participantid_in_session participantcode using "Data\Pilot Spain\backgroundvars.dta"

renpfix player

save "Data\Pilot Spain\CE policy and backgroundvars.dta", replace