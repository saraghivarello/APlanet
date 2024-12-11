cd "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/cmclogit"


*****************************************************************************

use "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/CE policy and backgroundvars.dta", clear

gen age=2022-year_born
gen woman=gender==0
gen car=trip_mode<2
gen carowner=access_to_car==0 /* This was coded wrong before */
gen caraccess=access_to_car==1
*replace Madrid="Madrid" if Madrid=="Yes"
*replace Madrid="Outside Madrid" if Madrid=="No"
*gen d_madrid=Madrid=="Madrid"
gen concern_fairness=income_distribution==3|income_distribution==4

lab var car "Car user"
lab var carowner "Car owner"
lab var caraccess "Car access"
lab var age "Age"
lab var woman "Woman"
lab var concern_fairness "Fairness concern"

lab def mode 0 "Private car (driver)" 1 "Private car (passenger)" 2 "Bus" 3 "Metro/tram/light rail" 4 "Train" 5 "Walk" 6 "Bike" 7 "E-scooter" 8 "Motorcycle" 9 "Other"
lab val trip_mode mode

*****************************************************************************

*cd home/sara/Documents/A-Planet/Askill stata codes/Second pilot

*** DESCRIPTIVES ***

*tab Madrid if subsessionround==1&alt==1
tab trip_mode if subsessionround==1&alt==1
tab car if subsessionround==1&alt==1

graph bar (percent) if subsessionround==1&alt==1, over(trip_mode) asyvars  xsize(8)
graph export "Figures/CE policy/mode_city.svg", replace

corr price_ur price_un price_o ev d_rev* if alt!=3
foreach var in price_ur price_un price_o ev d_rev1 d_rev2 d_rev3 d_rev4 d_rev5 {
gen diff_`var'=`var'-`var'[_n-1] if alt==2
}
corr diff_*


* TREATMENTS AND COVARIATE BALANCE

preserve
keep if subsessionround==1&alt==1

eststo clear
sort treatment

by treatment: eststo: quietly estpost sum age woman car carowner caraccess

esttab using "Tables/Descriptive/treatment_covar.rtf", replace cells(mean(fmt(2))) label mtitles("Control" "Info 1" "Info 2" "Info 3" "Info 4") collabels(, none) fonttbl(\f0\fnil Arial; )

gen controlgroup=treatment=="Baseline"

prtest woman, by(controlgroup)
prtest carowner, by(controlgroup)

restore


* FIGURE: CHOICE OF ALTERNATIVE *

lab def alt 1 "Alt. 1" 2 "Alt. 2" 3 "Alt. 3", replace
lab def alt_tot -1 "No"  0 "Alt. 1 or 2" 1 "Alt. 1" 2 "Alt. 2" 3 "Alt. 3", replace

lab val policy_choice alt

graph bar if alt==1, over(policy_choice) bargap(10) ytitle("") subtitle("Share choosing each alternative") legend(cols(3)) graphregion(color(white)) xsize(8)
graph save "Figures/CE policy/gph/bar_choice.gph", replace

preserve
gen always=0
keep if alt==1
forval i=1/3 {
egen count_alt`i'=total(policy_choice==`i'), by(participantcode)
replace always=`i' if count_alt`i'==8
}
gen always12=count_alt1<8&count_alt2<8&count_alt1+count_alt2==8
replace always=-1 if always==0&always12==0
replace always=0 if always12==1
lab val always alt_tot
keep if subsessionround==1
graph bar if alt==1, over(always) bargap(10) ytitle("") subtitle("Share always choosing same alternative") legend(cols(3)) graphregion(color(white)) xsize(8)
graph save "Figures/CE policy/gph/bar_lexi.gph", replace
restore

graph combine "Figures/CE policy/gph/bar_choice.gph" "Figures/CE policy/gph/bar_lexi.gph",  graphregion(color(white)) xsize(10) iscale(1.3)
graph export "Figures/CE policy/bar_choice_lexi.svg", replace


* FIGURE: CHOICE OF ALTERNATIVE BY PRICE LevEL *

cd "Figures/CE policy/gph"

foreach cat in ur un o {
graph bar (mean) choice if alt!=3, over(price_`cat') ytitle("") subtitle("Choice, by Price `cat'") graphregion(color(white))
graph save choice_price_`cat'.gph, replace
}
graph combine choice_price_ur.gph choice_price_un.gph choice_price_o.gph, ycommon cols(3) xsize(10) graphregion(color(white)) iscale(1.3)
cd ..
graph export "bar_choice_price.svg", replace

cd ../..


* FIGURE: CHOICE OF ALTERNATIVE BY TREATMENT *

cd "Figures/CE policy/gph"
graph bar (mean) choice if alt==1, over(treatment, label(alternate)) ytitle("") subtitle("Alternative 1") graphregion(color(white))
graph save choice_treatment_al1.gph, replace
graph bar (mean) choice if alt==2, over(treatment, label(alternate)) ytitle("") subtitle("Alternative 2") graphregion(color(white))
graph save choice_treatment_al2.gph, replace

graph combine choice_treatment_al1.gph choice_treatment_al2.gph, ycommon cols(3) xsize(10) graphregion(color(white)) iscale(1.3)
cd ..
graph export "bar_choice_treatment.svg", replace

cd ../..


* FIGURE: CHOICE OF ALTERNATIVE BY TREATMENT AND PRICE LEVEL *

cd "Figures/CE policy/gph"
graph bar (mean) choice if alt!=3, over(treatment) asyvars over(price_ur) ytitle("") subtitle("Choice, by Price urban rush") graphregion(color(white))
graph save choice_treatment_price_ur.gph, replace
graph bar (mean) choice if alt!=3, over(treatment) asyvars over(price_un) ytitle("") subtitle("Choice, by Price urban non-rush") graphregion(color(white))
graph save choice_treatment_price_un.gph, replace

graph combine choice_treatment_price_ur.gph choice_treatment_price_un.gph, ycommon cols(3) xsize(10) graphregion(color(white)) iscale(1.3)
cd ..
graph export "bar_choice_treatment_price.svg", replace

cd ../..


/*


graph bar (mean) choice_policy if alt==1, over(Bywhichmode) asyvars bargap(10) ytitle("") subtitle("Share choosing road pricing, by travel mode") legend(cols(3)) graphregion(color(white))  xsize(8)
graph export "Figures\CE policy\policy_mode.svg", replace

graph bar (mean) choice_policy if alt==1, over(Doyouownorhaveaccesstoac) asyvars bargap(10) ytitle("") subtitle("Share choosing road pricing, by car ownership") legend(cols(3)) graphregion(color(white))  xsize(6)
graph export "Figures\CE policy\policy_carowner.svg", replace

preserve
keep if alt==1
forval i=1/3 {
egen count_alt`i'=total(policy_choice==`i'), by(participantcode)
gen always`i'=count_alt`i'==6
}

keep if subsessionround==1

graph bar (mean) always3, over(Bywhichmode) asyvars bargap(10) ytitle("") subtitle(Share always choosing status quo) legend(cols(3)) graphregion(color(white))  xsize(8)
graph export "Figures\CE policy\lexi_statusquo_mode.svg", replace
restore

*/




*****************************************************************************

*** POWER ANALYSIS ***

tabstat choice_policy if alt==1, by(treatment) stat(mean sd count)

prtest choice_policy if alt==1&(treatment==1|treatment==2), by(treatment) cluster(participantcode) rho(0.5)


*****************************************************************************

*** ESTIMATING CHOICE MODELS ***

*drop d_rev1

replace price_o=price_o*10

gen price_U_avg=(price_ur+price_un)/2
gen ev_price_ur=price_ur*(1-ev)
gen ev_price_un=price_un*(1-ev)
gen ev_price_o=price_o*(1-ev)

lab var policy "Alt. 1 or 2"
lab var policy2 "Alt. 2"
lab var price_ur "Price, urban peak-hour"
lab var price_un "Price, urban off-peak"
lab var price_o "Price, non-urban"
lab var ev "EV discount"
lab var d_rev1 "General budget"
lab var d_rev2 "Equal cash transfer"
lab var d_rev3 "Low-income citizens"
lab var d_rev4 "Road investments"
lab var d_rev5 "PT/active travel"
lab var price_U_avg "Price, urban average"
lab var ev_price_ur "EV price, urban rush"
lab var ev_price_un "EV price, urban non-rush"
lab var ev_price_o "EV price, non-urban"

egen treatment_num=group(treatment)

replace treatment_num=treatment_num-1
gen treatment12=treatment_num*(alt<3)
tab treatment12, gen(d_treatment)

lab var d_treatment2 "Info: Pollution/congestion"
lab var d_treatment3 "Info: Public revenues"
lab var d_treatment4 "Info: Road pricing"
lab var d_treatment5 "Info: Social norms"


* 1: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES *

eststo clear 
eststo: cmclogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons

esttab using "Tables/CE policy/logit_simple_nocovar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2 d_rev3 d_rev4 d_rev5) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


cmclogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
*margins, at(price_U_avg=generate(price_U_avg)) at(price_U_avg=generate(price_U_avg+0.01)) contrast(at(r)) outcome(1) alt(1)
margins, dydx(price_ur) outcome(1) alt(1)
margins, dydx(price_un) outcome(1) alt(1)


* 1b: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES, control group *

preserve
keep if treatment=="Baseline"
eststo clear 
eststo: cmclogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons

restore

esttab using "Tables/CE policy/logit_simple_nocovar_controlgroup.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2 d_rev3 d_rev4 d_rev5) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 1c: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES, by treatment *

eststo clear 
forval i=0/4 {
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==`i', nocons
}
esttab using "Tables/CE policy/logit_simple_nocovar_bytreatment.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy price_ur price_un price_o ev d_rev2 d_rev3 d_rev4 d_rev5) ///
mtitles("Control" "Cong./poll." "Pub. rev." "Road pricing" "Soc. norms") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")



* 2: EFFECT OF THE CONGESTION AND POLLUTION TREATMENT *
eststo clear
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 c.price_ur#c.d_treatment2 c.price_un#c.d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment2 d_rev2-d_rev5 d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 c.d_rev4#c.d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment2 d_rev2-d_rev5 c.d_rev4#c.d_treatment2 c.price_ur#c.d_treatment2 c.price_un#c.d_treatment2 d_treatment2 c.policy#c.woman if (treatment_num==0|treatment_num==1), nocons
 
esttab using "Tables/CE policy/logit_treatment_pollcong.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 d_treatment2 if treatment<2&d_madrid==1, nocons
margins, dydx(d_treatment2) outcome(1) alt(1) post
*margins, dydx(d_treatment2) outcome(2) alt(2) post
qui cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 d_treatment2 if treatment<2&d_madrid==1, nocons
margins, at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1) post

cmclogit choice policy c.price_U_avg##c.d_treatment2 price_o ev d_rev2-d_rev5 if treatment<2&d_madrid==1, nocons
margins, dydx(price_U_avg) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1)

cmclogit choice policy price_U_avg price_o c.ev##c.d_treatment2 d_rev2-d_rev5 if treatment<2&d_madrid==1, nocons
margins, dydx(ev) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) outcome(1) alt(1)
margins, dydx(ev) at(d_treatment2=0) outcome(1) alt(1)

esttab


* 3: EFFECT OF THE PUBLIC RevENUE TREATMENT *
eststo clear
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 c.d_rev1#c.d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment3 d_rev2-d_rev5 d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 c.price_ur#c.d_treatment3 c.price_un#c.d_treatment3 c.price_o#c.d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment3 d_rev2-d_rev5 c.d_rev1#c.d_treatment3 c.price_ur#c.d_treatment3 c.price_un#c.d_treatment3 c.price_o#c.d_treatment3 d_treatment3 c.policy#c.woman if (treatment_num==0|treatment_num==2), nocons

esttab using "Tables/CE policy/logit_treatment_revenue.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 4: EFFECT OF THE ROAD PRICING TREATMENT *
eststo clear
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment4 c.price_ur#c.d_treatment4 c.price_un#c.d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment4 d_rev2-d_rev5 d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment4 d_rev2-d_rev5 c.price_ur#c.d_treatment4 c.price_un#c.d_treatment4 d_treatment4 c.policy#c.woman if (treatment_num==0|treatment_num==3), nocons
 
esttab using "Tables/CE policy/logit_treatment_roadpricing.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")



* 3: EFFECT OF THE SOCIAL NORMS TREATMENT *
eststo clear
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 c.price_ur#c.d_treatment5 c.price_un#c.d_treatment5 c.price_o#c.d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment5 d_rev2-d_rev5 d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 c.d_rev4#c.d_treatment5 c.d_rev5#c.d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: cmclogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment5 d_rev2-d_rev5 c.d_rev4#c.d_treatment5 c.d_rev5#c.d_treatment5 c.price_ur#c.d_treatment5 c.price_un#c.d_treatment5 c.price_o#c.d_treatment5 d_treatment5 c.policy#c.woman if (treatment_num==0|treatment_num==4), nocons

esttab using "Tables/CE policy/logit_treatment_socnorm.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")




* 4: ASSOCIATION WITH CAR OWNERSHIP *
eststo clear
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.price_U_avg#c.car c.ev#c.car, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.d_rev4#c.car c.d_rev5#c.car, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car if d_madrid==1, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.d_rev4#c.car c.d_rev5#c.car if d_madrid==1, nocons
*eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman if d_madrid==1, nocons

esttab using "Tables/CE policy/logit_car_nocar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "All" "Madrid" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 5. ASSOCIATION WITH FAIRNESS CONCERNS *
eststo clear
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.d_rev3#c.concern_fairness, nocons
eststo: cmclogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.d_rev3#c.concern_fairness if d_madrid==1, nocons
eststo: cmclogit choice policy price_U_avg c.price_U_avg#c.concern_fairness price_o ev c.ev#c.concern_fairness d_rev2-d_rev5 c.d_rev3#c.concern_fairness, nocons
eststo: cmclogit choice policy price_U_avg c.price_U_avg#c.concern_fairness price_o ev c.ev#c.concern_fairness d_rev2-d_rev5 c.d_rev3#c.concern_fairness if (treatment_num==0|treatment_num==3)&d_madrid==1, nocons

esttab using "Tables/CE policy/logit_concern_fairness.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

/*

eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==1, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==2, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==3, nocons
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment==4, nocons

esttab using "Tables\CE policy\logit_simple_treatment.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("1" "2" "3" "4") nonote stats(N r2_p, fmt(0 2) labels("Observations" "Rho-squared"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

eststo clear 
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==1, nocons constraints(1)
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==2, nocons constraints(1)
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==3, nocons constraints(1)
eststo: cmclogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment==4, nocons constraints(1)

esttab using "Tables\CE policy\logit_simple_treatment_constr.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("1" "2" "3" "4") nonote stats(N r2_p, fmt(0 2) labels("Observations" "Rho-squared"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

* Interaction effects of treatment *
cmclogit choice policy price_ur price_un price_o d_rev* ev c.ev#treatment c.d_rev*#treatment i.treatment, nocons

