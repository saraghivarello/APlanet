cd "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/mlogit"


*****************************************************************************

use "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/CE policy and backgroundvars.dta", clear


*****************************************************************************

*cd home/sara/Documents/A-Planet/Askill stata codes/Second pilot


*** POWER ANALYSIS ***

tabstat choice_policy if alt==1, by(treatment) stat(mean sd count)

*prtest choice_policy if alt==1&(treatment==1|treatment==2), by(treatment) cluster(participantcode) rho(0.5)


*****************************************************************************

*** ESTIMATING CHOICE MODELS ***


* 1: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES *

eststo clear 
eststo: mlogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons

esttab using "Tables/CE policy/logit_simple_nocovar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2 d_rev3 d_rev4 d_rev5) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


mlogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
*margins, at(price_U_avg=generate(price_U_avg)) at(price_U_avg=generate(price_U_avg+0.01)) contrast(at(r)) outcome(1) alt(1)
*margins, dydx(price_ur) outcome(1) alt(1)
*margins, dydx(price_un) outcome(1) alt(1)


* 1b: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES, control group *

preserve
keep if treatment=="Baseline"
eststo clear 
eststo: mlogit choice policy policy2 price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons
eststo: mlogit choice policy price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2-d_rev5, nocons

restore

esttab using "Tables/CE policy/logit_simple_nocovar_controlgroup.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy policy2 price_ur price_un price_o ev ev_price_ur ev_price_un ev_price_o d_rev2 d_rev3 d_rev4 d_rev5) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 1c: MODELS WITHOUT ADDITIONAL EXPLANATORY VARIABLES, by treatment *

eststo clear 
forval i=0/4 {
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 if treatment_num==`i', nocons
}
esttab using "Tables/CE policy/logit_simple_nocovar_bytreatment.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
order(policy price_ur price_un price_o ev d_rev2 d_rev3 d_rev4 d_rev5) ///
mtitles("Control" "Cong./poll." "Pub. rev." "Road pricing" "Soc. norms") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")



* 2: EFFECT OF THE CONGESTION AND POLLUTION TREATMENT *
eststo clear
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 c.price_ur#c.d_treatment2 c.price_un#c.d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment2 d_rev2-d_rev5 d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment2 c.d_rev4#c.d_treatment2 if (treatment_num==0|treatment_num==1), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment2 d_rev2-d_rev5 c.d_rev4#c.d_treatment2 c.price_ur#c.d_treatment2 c.price_un#c.d_treatment2 d_treatment2 c.policy#c.woman if (treatment_num==0|treatment_num==1), nocons
 
esttab using "Tables/CE policy/logit_treatment_pollcong.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 d_treatment2 if treatment_num<2, nocons
*margins, dydx(d_treatment2) outcome(1) alt(1) post
*margins, dydx(d_treatment2) outcome(2) alt(2) post
qui mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 d_treatment2 if treatment_num<2, nocons
margins, at(d_treatment2=0) at(d_treatment2=1) contrast(at(r))  post

mlogit choice policy c.price_U_avg##c.d_treatment2 price_o ev d_rev2-d_rev5 if treatment_num<2==1, nocons
margins, dydx(price_U_avg) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) 

mlogit choice policy price_U_avg price_o c.ev##c.d_treatment2 d_rev2-d_rev5 if treatment_num<2==1, nocons
margins, dydx(ev) at(d_treatment2=0) at(d_treatment2=1) contrast(at(r)) 
margins, dydx(ev) at(d_treatment2=0) 

esttab


* 3: EFFECT OF THE PUBLIC RevENUE TREATMENT *
eststo clear
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 c.d_rev1#c.d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment3 d_rev2-d_rev5 d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment3 c.price_ur#c.d_treatment3 c.price_un#c.d_treatment3 c.price_o#c.d_treatment3 if (treatment_num==0|treatment_num==2), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment3 d_rev2-d_rev5 c.d_rev1#c.d_treatment3 c.price_ur#c.d_treatment3 c.price_un#c.d_treatment3 c.price_o#c.d_treatment3 d_treatment3 c.policy#c.woman if (treatment_num==0|treatment_num==2), nocons

esttab using "Tables/CE policy/logit_treatment_revenue.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 4: EFFECT OF THE ROAD PRICING TREATMENT *
eststo clear
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment4 c.price_ur#c.d_treatment4 c.price_un#c.d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment4 d_rev2-d_rev5 d_treatment4 if (treatment_num==0|treatment_num==3), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment4 d_rev2-d_rev5 c.price_ur#c.d_treatment4 c.price_un#c.d_treatment4 d_treatment4 c.policy#c.woman if (treatment_num==0|treatment_num==3), nocons
 
esttab using "Tables/CE policy/logit_treatment_roadpricing.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")



* 5: EFFECT OF THE SOCIAL NORMS TREATMENT *
eststo clear
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 c.price_ur#c.d_treatment5 c.price_un#c.d_treatment5 c.price_o#c.d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment5 d_rev2-d_rev5 d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: mlogit choice policy price_ur price_un price_o ev d_rev2-d_rev5 d_treatment5 c.d_rev4#c.d_treatment5 c.d_rev5#c.d_treatment5 if (treatment_num==0|treatment_num==4), nocons
eststo: mlogit choice policy price_ur price_un price_o ev c.ev#c.d_treatment5 d_rev2-d_rev5 c.d_rev4#c.d_treatment5 c.d_rev5#c.d_treatment5 c.price_ur#c.d_treatment5 c.price_un#c.d_treatment5 c.price_o#c.d_treatment5 d_treatment5 c.policy#c.woman if (treatment_num==0|treatment_num==4), nocons

esttab using "Tables/CE policy/logit_treatment_socnorm.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
nomtitles nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")



* 6: ASSOCIATION WITH CAR OWNERSHIP *
eststo clear
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.price_U_avg#c.car c.ev#c.car, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.d_rev4#c.car c.d_rev5#c.car, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.d_rev4#c.car c.d_rev5#c.car, nocons
*eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.car c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman if d_madrid==1, nocons

esttab using "Tables/CE policy/logit_car_nocar.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "All" "Madrid" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")


* 7. ASSOCIATION WITH FAIRNESS CONCERNS *
eststo clear
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.policy#c.carowner c.policy#c.caraccess c.policy#c.age c.policy#c.woman, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.d_rev3#c.concern_fairness, nocons
eststo: mlogit choice policy price_U_avg price_o ev d_rev2-d_rev5 c.policy#c.concern_fairness c.d_rev3#c.concern_fairness, nocons
eststo: mlogit choice policy price_U_avg c.price_U_avg#c.concern_fairness price_o ev c.ev#c.concern_fairness d_rev2-d_rev5 c.d_rev3#c.concern_fairness, nocons
eststo: mlogit choice policy price_U_avg c.price_U_avg#c.concern_fairness price_o ev c.ev#c.concern_fairness d_rev2-d_rev5 c.d_rev3#c.concern_fairness if (treatment_num==0|treatment_num==3), nocons

esttab using "Tables/CE policy/logit_concern_fairness.rtf", replace b(%9.3f) se(%9.3f) label pr2 star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("All" "All" "All" "Madrid" "All" "Madrid") nonote stats(N ll, fmt(0 2) labels("Observations" "Final LL"))  ///
addnotes("* p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered on respondent.")

