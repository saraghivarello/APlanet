cd "/home/sara/Documents/A-Planet/Askill stata codes/First pilot/clogit"


*****************************************************************************

use "/home/sara/Documents/A-Planet/Askill stata codes/First pilot/CE policy and backgroundvars.dta", clear

gen age=2022-year_born
gen woman=gender==0
gen car=trip_mode<2
gen carowner=access_to_car==0 /* This was coded wrong before */
gen caraccess=access_to_car==1
replace Madrid="Madrid" if Madrid=="Yes"
replace Madrid="Outside Madrid" if Madrid=="No"
gen d_madrid=Madrid=="Madrid"
gen concern_fairness=income_distribution==3|income_distribution==4

lab var car "Car user"
lab var carowner "Car owner"
lab var caraccess "Car access"
lab var age "Age"
lab var woman "Woman"
lab var concern_fairness "Fairness concern"

lab def mode 0 "Private car (driver)" 1 "Private car (passenger)" 2 "Bus" 3 "Metro/tram/light rail" 4 "Train" 5 "Walk" 6 "Bike" 7 "E-scooter" 8 "Motorcycle" 10 "Other"
lab val trip_mode mode


*****************************************************************************

*cd Analysis/Askill

*** DESCRIPTIVES ***

tab Madrid if subsessionround==1&alt==1
tab trip_mode if subsessionround==1&alt==1
tab car if subsessionround==1&alt==1

graph bar (percent) if subsessionround==1&alt==1, over(trip_mode) by(Madrid, graphregion(color(white))) asyvars  xsize(8)
*graph export "Figures/CE policy/mode_city.svg", replace

corr Price_UR Price_UN Price_O EV d_Rev* if alt!=3
foreach var in Price_UR Price_UN Price_O EV d_Rev1 d_Rev2 d_Rev3 d_Rev4 d_Rev5 {
gen diff_`var'=`var'-`var'[_n-1] if alt==2
}
corr diff_*

* TREATMENTS AND COVARIATE BALANCE

preserve
keep if subsessionround==1&alt==1

eststo clear
sort mod

by mod: eststo: quietly estpost sum d_madrid age woman car carowner caraccess

esttab using "Tables/Descriptive/treatment_covar.rtf", replace cells(mean(fmt(2))) label mtitles("Control" "Info 1" "Info 2" "Info 3") collabels(, none) fonttbl(\f0\fnil Arial; )

prtest d_madrid if (mod==1|mod==4), by(mod)

restore


* FIGURE: CHOICE OF ALTERNATIVE *

lab def alt 0 "No" 1 "Alt. 1" 2 "Alt. 2" 3 "Alt. 3", replace
lab val policy_choice alt

graph bar if alt==1, over(policy_choice) bargap(10) ytitle("") subtitle("Share choosing each alternative") legend(cols(3)) graphregion(color(white)) xsize(8)
*graph save "Figures/CE policy/gph/bar_choice.gph", replace
preserve
gen always=0
keep if alt==1
forval i=1/3 {
egen count_alt`i'=total(policy_choice==`i'), by(participantcode)
replace always=`i' if count_alt`i'==6
}
lab val always alt
keep if subsessionround==1
graph bar if alt==1, over(always) bargap(10) ytitle("") subtitle("Share always choosing same alternative") legend(cols(3)) graphregion(color(white)) xsize(8)
*graph save "Figures/CE policy/gph/bar_lexi.gph", replace
restore

graph combine "Figures/CE policy/gph/bar_choice.gph" "Figures/CE policy/gph/bar_lexi.gph",  graphregion(color(white)) xsize(10) iscale(1.3)
graph export "Figures/CE policy/bar_choice_lexi.svg", replace


* FIGURE: CHOICE OF ALTERNATIVE BY PRICE LEVEL *

cd "Figures/CE policy/gph"

foreach cat in UR UN O {
graph bar (mean) choice if alt!=3, over(Price_`cat') ytitle("") subtitle("Choice, by Price `cat'") graphregion(color(white))
graph save choice_Price_`cat'.gph, replace
}
graph combine choice_Price_UR.gph choice_Price_UN.gph choice_Price_O.gph, cols(3) xsize(10) graphregion(color(white)) iscale(1.3)
cd ..
*graph export "bar_choice_price.svg", replace

cd gph
foreach cat in UR UN O {
graph bar (mean) choice if alt!=3&d_madrid==1, over(Price_`cat') ytitle("") subtitle("Choice, by Price `cat'") graphregion(color(white))
*graph save choice_Price_`cat'.gph, replace
}
graph combine choice_Price_UR.gph choice_Price_UN.gph choice_Price_O.gph, cols(3) xsize(10) graphregion(color(white)) iscale(1.3)
cd ..
*graph export "bar_choice_price_madrid.svg", replace

cd ../..



*****************************************************************************

*** POWER ANALYSIS ***

tabstat choice_policy if alt==1&d_madrid==1, by(mod) stat(mean sd count)

prtest choice_policy if alt==1&d_madrid==1&(mod==1|mod==2), by(mod) cluster(participantcode) rho(0.5)


*****************************************************************************

*** ESTIMATING CHOICE MODELS ***

*drop d_Rev1

replace Price_O=Price_O*10

gen Price_U_avg=(Price_UR+Price_UN)/2
gen EV_Price_U=Price_U_avg*(1-EV)
gen EV_Price_O=Price_O*(1-EV)

lab var policy "Alt. 1 or 2"
lab var policy2 "Alt. 2"
lab var Price_UR "Price, urban peak-hour"
lab var Price_UN "Price, urban off-peak"
lab var Price_O "Price, non-urban"
lab var EV "EV discount"
lab var d_Rev1 "General budget"
lab var d_Rev2 "Equal cash transfer"
lab var d_Rev3 "Low-income citizens"
lab var d_Rev4 "Road investments"
lab var d_Rev5 "PT/active travel"
lab var Price_U_avg "Price, urban average"
lab var EV_Price_U "EV price, urban"
lab var EV_Price_O "EV price, non-urban"

replace mod=mod-1
gen treatment=mod*(alt<3)
tab treatment, gen(d_treatment)

lab var d_treatment2 "Info: Pollution/congestion"
lab var d_treatment3 "Info: Public revenues"
lab var d_treatment4 "Info: Redistribution"


* 1: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES *

eststo clear 
eststo: clogit choice policy policy2 Price_UR Price_UN Price_O EV d_Rev2-d_Rev5, group(mod)
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5, group(mod)
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5, group(mod)
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 if d_madrid==1, group(mod)
eststo: clogit choice policy Price_U_avg Price_O EV_Price_O EV_Price_U d_Rev2-d_Rev5, group(mod)
eststo: clogit choice policy Price_U_avg Price_O EV_Price_O EV_Price_U d_Rev2-d_Rev5 if d_madrid==1, group(mod)

esttab using "Tables/CE policy/logit_simple_nocovar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 Price_UR Price_UN Price_U_avg Price_O EV EV_Price_U EV_Price_O d_Rev2 d_Rev3 d_Rev4 d_Rev5) ///
mtitles("All" "All" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 if d_madrid==1
*margins, at(Price_U_avg=generate(Price_U_avg)) at(Price_U_avg=generate(Price_U_avg+0.01)) contrast(at(r)) outcome(1) alt(1)
margins, dydx(Price_U_avg) outcome(1) alt(1)


* 1b: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES, control group *

preserve
keep if mod==0
eststo clear 
eststo: clogit choice policy policy2 Price_UR Price_UN Price_O EV d_Rev2-d_Rev5, nocons
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 if d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg Price_O EV_Price_O EV_Price_U d_Rev2-d_Rev5, nocons
eststo: clogit choice policy Price_U_avg Price_O EV_Price_O EV_Price_U d_Rev2-d_Rev5 if d_madrid==1, nocons
restore

esttab using "Tables/CE policy/logit_simple_nocovar_controlgroup.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 Price_UR Price_UN Price_U_avg Price_O EV EV_Price_U EV_Price_O d_Rev2 d_Rev3 d_Rev4 d_Rev5) ///
mtitles("All" "All" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 2: EFFECT OF THE CONGESTION AND POLLUTION TREATMENT *
eststo clear
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment2 if mod<2, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment2 if mod<2&d_madrid==1, nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment2 Price_O EV d_Rev2-d_Rev5 if mod<2, nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment2 Price_O EV d_Rev2-d_Rev5 if mod<2&d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg Price_O c.EV##c.d_treatment2 d_Rev2-d_Rev5 if mod<2, nocons
eststo: clogit choice policy Price_U_avg Price_O c.EV##c.d_treatment2 d_Rev2-d_Rev5 if mod<2&d_madrid==1, nocons
 
esttab using "Tables/CE policy/logit_treatment_pollcong.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "Madrid" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment2 if mod<2&d_madrid==1, nocons
margins, dydx(d_treatment2) outcome(1) alt(1) post
*margins, dydx(d_treatment2) outcome(2) alt(2) post
qui clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment2 if mod<2&d_madrid==1, nocons
margins, at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1) post

clogit choice policy c.Price_U_avg##c.d_treatment2 Price_O EV d_Rev2-d_Rev5 if mod<2&d_madrid==1, nocons
margins, dydx(Price_U_avg) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1)

clogit choice policy Price_U_avg Price_O c.EV##c.d_treatment2 d_Rev2-d_Rev5 if mod<2&d_madrid==1, nocons
margins, dydx(EV) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1)
margins, dydx(EV) at(d_treatment2=0) outcome(1) alt(1)

esttab


* 3: EFFECT OF THE PUBLIC REVENUE TREATMENT *
eststo clear
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment3 if (mod==0|mod==2), nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment3 if (mod==0|mod==2)&d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment3 c.d_Rev1#c.d_treatment3 if (mod==0|mod==2), nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment3 c.d_Rev1#c.d_treatment3 if (mod==0|mod==2)&d_madrid==1, nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment3 Price_O EV c.EV#c.d_treatment3 d_Rev2-d_Rev5 c.d_Rev1#c.d_treatment3 if (mod==0|mod==2), nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment3 Price_O EV c.EV#c.d_treatment3 d_Rev2-d_Rev5 c.d_Rev1#c.d_treatment3 if (mod==0|mod==2)&d_madrid==1, nocons

esttab using "Tables/CE policy/logit_treatment_revenue.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "Madrid" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 3: EFFECT OF THE REDISTRIBUTION TREATMENT *
eststo clear
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment4 if (mod==0|mod==3), nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment4 if (mod==0|mod==3)&d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment4 c.d_Rev3#c.d_treatment4 if (mod==0|mod==3), nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 d_treatment4 c.d_Rev3#c.d_treatment4 if (mod==0|mod==3)&d_madrid==1, nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment4 Price_O EV c.EV#c.d_treatment4 d_Rev2-d_Rev5 c.d_Rev3#c.d_treatment4 if (mod==0|mod==3), nocons
eststo: clogit choice policy c.Price_U_avg##c.d_treatment4 Price_O EV c.EV#c.d_treatment4 d_Rev2-d_Rev5 c.d_Rev3#c.d_treatment4 if (mod==0|mod==3)&d_madrid==1, nocons

esttab using "Tables/CE policy/logit_treatment_redistr.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "Madrid" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 4: ASSOCIATION WITH CAR OWNERSHIP *
eststo clear
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car c.Price_U_avg#c.car c.EV#c.car, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car c.d_Rev4#c.car c.d_Rev5#c.car, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car if d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car c.d_Rev4#c.car c.d_Rev5#c.car if d_madrid==1, nocons
*eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman if d_madrid==1, nocons

esttab using "Tables/CE policy/logit_car_nocar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "All" "Madrid" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 5. ASSOCIATION WITH FAIRNESS CONCERNS *
eststo clear
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.concern_fairness, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.concern_fairness c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.concern_fairness c.d_Rev3#c.concern_fairness, nocons
eststo: clogit choice policy Price_U_avg Price_O EV d_Rev2-d_Rev5 c.policy#c.concern_fairness c.d_Rev3#c.concern_fairness if d_madrid==1, nocons
eststo: clogit choice policy Price_U_avg c.Price_U_avg#c.concern_fairness Price_O EV c.EV#c.concern_fairness d_Rev2-d_Rev5 c.d_Rev3#c.concern_fairness, nocons
eststo: clogit choice policy Price_U_avg c.Price_U_avg#c.concern_fairness Price_O EV c.EV#c.concern_fairness d_Rev2-d_Rev5 c.d_Rev3#c.concern_fairness if (mod==0|mod==3)&d_madrid==1, nocons

esttab using "Tables/CE policy/logit_concern_fairness.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

/*

eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==1, nocons
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==2, nocons
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==3, nocons
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==4, nocons

esttab using "Tables/CE policy/logit_simple_treatment.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("1" "2" "3" "4") nonote stats(N r2_p, fmt(0 2) labels("Observations" "Rho-squared"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

eststo clear 
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==1, nocons constraints(1)
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==2, nocons constraints(1)
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==3, nocons constraints(1)
eststo: clogit choice policy Price_UR Price_UN Price_O EV d_Rev2-d_Rev5 if mod==4, nocons constraints(1)

esttab using "Tables/CE policy/logit_simple_treatment_constr.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("1" "2" "3" "4") nonote stats(N r2_p, fmt(0 2) labels("Observations" "Rho-squared"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

* Interaction effects of treatment *
clogit choice policy Price_UR Price_UN Price_O d_Rev* EV c.EV#mod c.d_Rev*#mod i.treatment, nocons

