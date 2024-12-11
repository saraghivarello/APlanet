cd "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/Latent class"


*****************************************************************************

use "/home/sara/Documents/A-Planet/Askill stata codes/Second pilot/CE policy and backgroundvars.dta", clear



// Bernoulli 


* doesn't converge
*. gsem (choice <- price_ur d_rev5) , family(bernoulli) link(logit) lclass(C 2)  

* converge but equal coefficients
*. gsem (choice <- price_ur price_un price_o ev d_rev2-d_rev5) (C <- policy), family(bernoulli) link(logit) lclass(C 2) lcinvariant(coef) nonrtolerance

*. gsem (choice <- price_ur price_un price_o ev d_rev2-d_rev5), family(bernoulli) link(logit) lclass(C 2) lcinvariant(coef) nonrtolerance

* converges 
. gsem (choice <- price_ur price_un price_o ev d_rev2-d_rev5), family(bernoulli) link(logit) lclass(C 2) nonrtolerance

*what are the expected proportions of the population in each group?
. estat lcprob
*goodness-of-fit statistics
. estat lcgof



// Gaussian 

*converges but equal coefficients
*gsem (choice <- price_ur price_un price_o ev d_rev2-d_rev5, regress) (C <- policy), regress lclass(C 2) lcinvariant(coef) nonrtolerance

*converges 
*gsem (choice <- price_ur price_un price_o ev d_rev2-d_rev5, regress) (C <- policy), regress lclass(C 2) nonrtolerance


// FMM 

* doesn't converge
*. fmm 2 : logit choice price_ur d_rev5


* converges
. fmm 2 : mlogit choice price_ur price_un price_o ev d_rev5


*Store the parameter estimates in a matrix named b
matrix b = e(b)   


*Fit a model including age using matrix b for the starting values, to include d_rev2-d_rev4
*converges
. fmm 2 , from(b, skip) : mlogit choice price_ur price_un price_o ev d_rev2-d_rev5 


*cannot compute an improvement -- discontinuous region encountered
*. fmm 3 , from(b, skip) : mlogit choice price_ur price_un price_o ev d_rev5 




*what are the expected proportions of the population in each group?
. estat lcprob
*goodness-of-fit statistics
. estat lcgof


*For individuals in Class 1, what is the probability of responding positively to each question?

. estat lcmean



*We can use predict, classposteriorpr to estimate probabilities of belonging to class 1, 2...
*Let's select the class with the highest predicted probability as being the predicted class.

. predict cpost*, classposteriorpr
. egen max = rowmax(cpost*)
. generate predclass = 1 if cpost1==max
. replace predclass = 2 if cpost2==max
. replace predclass = 3 if cpost3==max
. replace predclass = 4 if cpost4==max
. replace predclass = 5 if cpost5==max

. tabulate predclass


*Let's take a look at these predictions for some individuals in our sample.

. list in 1/2, abbrev(10)


*Do the predicted classes match the actual groups?

. tabulate predclass treatment_num

. gen correct = predclass == treatment_num

. sum correct
