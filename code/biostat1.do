/* Directory
ex_sample - creates population sample of SBPs for a state (Mass.)  Examples 1.1 and 1.2
ex_pdf - graphing a pdfs and parabola. Example 1.3
ex_graph - more functions (extra in example 1.3)
ex_cdf - cdf Example 1.4
ex_icdf - inverse Example 1.5
prob1 - gives links to articles illustrating different goals of statistical practice App 1.2.1
prob2 - set up stratified sampling probl 1.2.2
strat_samp - answer to prob2
prob3 - takes to measurment problem 1.2.3
prob4 - setus up reliability problem 1.2.4
rpt_sbp - answer to problem 1.2.4
prob5 - three heads with 3 flips of a coin - no problem 
*/



di "Welcome to the Stata environment for Chapter 1 - Bistatistics 1 _n  You are now ready to run all the examples and problems in this chapter"

* In text example 1.1 and 1.2
cap prog drop ex_sample
prog define ex_sample
clear
qui set obs 5600000
set seed 39392
qui drawnorm sbp ,mean(122) sd(19)
di _n(3) "This data is from a(n imaginary) census of Massachusetts where every " ///
    _n "adult had a blood pressure measured"
sum
di _n(3) "try stata help - type (or click) " "{stata help sample: help sample}"
end


cap prog drop ex_pdf
prog define ex_pdf
twoway function y=normalden(x,0,1), range(-3 3) ytitle(f(x)) ///
        title(Normal distribution) name(normal,replace)
twoway function y=x^2, range(-3 3) name(parabola,replace)
end

* Polynomials
cap program drop ex_graph
prog def ex_graph
	twoway function y=x^2 , range(-3 3) subtitle(y=x{sup:2}) name(quadratic,replace)
	twoway function y=3*x-.5*x^2 , range(-3 3) subtitle(y=y=3*x+.5*x{sup:2}) name(quadratic_2,replace)
	twoway function y=3+.3*x-.3*x^2-.8*ln(x) , range(-3 3) subtitle(y=3+.3*x-.3*x{sup:2}-.8*ln(x)) name(complex_quadratic,replace)
	twoway function y=ln(x), range(0 5) subtitle(y=e{sup:x}) name(logarithm,replace)
end

// used only in book where ansswers given in text biostat_1.html#nte-bs-ex4
cap prog drop ex_cdf 
prog define ex_cdf
    di ". di normal((140-130)/15)" 
    di normal((140-130)/15)
    di ". di normal((190-140)/20)" 
    di normal((190-140)/20)
end

*ex_cdf-class is given in prob10 and cdf_ans


cap prog drop ex_icdf
prog define ex_icdf
    di "di invnorm(0.90)*15 + 140"
    di invnorm(0.90)*15 + 140
end

cap prog drop prob1
prog prob1
di "{hline}"
display "Which of the three statistical practice goals does each paper represent?" 
di "{hline}" _newline(1)
display `"1. A comparison of traditional diarrhoea measurement methods with {break} microbiological and biochemical indicators - A cross-sectional  {break} observational study in the Cox's Bazar displaced persons camp {browse "https://pubmed.ncbi.nlm.nih.gov/34849477/":link}"' _newline(1)
display `"2. Randomized Trial of Early Detection and Treatment of {break} Postpartum Hemorrhage {browse "https://pubmed.ncbi.nlm.nih.gov/37158447/":link}"' _newline(1)
display `"3. Quality of care, process, and outcomes in elderly patients {break} with pneumonia {browse "https://pubmed.ncbi.nlm.nih.gov/9403422/":link}"' _newline(1)
display `"4. Measuring Returns to Hospital Care: Evidence from Ambulance {break} Referral Patterns {browse "https://www.journals.uchicago.edu/doi/10.1086/677756":link}"'
end

* sampling - Setup weighted mean
cap prog drop prob2
prog prob2
	clear
	set seed 27033
	qui set obs 2000
	gen female=cond(_n<1001,1,0)
	gen sbp=rnormal((140-30*female),15)
	bysort female: gen sample=_n<=(50*female+100*(1-female))
	de
	tab female,sum(sbp)
	table (sample) (female)
    di "A clinic has 2000 people in its population.  {break} You sample 150 and measure their SBP"
end

* Answer prob2
cap prog drop strat_samp
prog strat_samp
display as text _n "{bf:Answer to Problem 2:}" _n
    di "* Population mean"
	di ". sum sbp"
    sum sbp
    di "* Sample mean and CI"
	di ". mean sbp if sample==1"
    mean sbp if sample==1
	di "* Calculate weight with inverse probability of selection"
	di ". gen myweight= (1000/50)*sample if female==1"
    gen myweight= (1000/50)*sample if female==1
	di ". replace myweight= (1000/100)*sample if female==0"
    replace myweight= (1000/100)*sample if female==0
    di "* Weighted sample mean and CI"
	di ". mean sbp [pw=myweight]"
    mean sbp [pw=myweight]
end

cap prog drop prob3
prog prob3
	display _n(3) `"Measurement exercise description {break}  {browse "https://websites.umich.edu/~thofer/teach/hhcr-book/biostat_1.html#sec-bs1-measure-app":link to problem}"'
end

* reliability
cap prog drop prob4
prog define prob4
	clear
	quietly	{
	set seed 34038
	set obs 100
	gen id=_n
	gen true_hs=130+12*rnormal(0)
	expand 5
	bysort id: gen j=_n
	gen error=12*rnormal(0)
	gen bp=true_hs+error
	drop error true_hs
	reshape wide bp, i(id) j(j)
	note: Dataset of repeated blood pressures on patients in a clinic, separated by 5 minutes, each measurement done by randomly selected observers and devices.  The order listed (bp1-bp5) is entered in the dataset randomly.
	}
	de
	list in 1/5
	notes
	di "{stata help sample: help corr}, {stata help sample: help alpha} use std option" 
end
/*
	sum bp1-bp5
	corr bp1-bp5
	alpha bp1-bp5,std
*/


cap prog drop rpt_sbp
prog define rpt_sbp
	set more on
display as text _n "{bf:Answer to Problem 4:}" _n
	di ". corr bp1-bp5"
	corr bp1-bp5
di as text "{p}{txt} This gives you a bunch of correlations and if you were " ///
	"trying to describe the average consistency you would say it is somewhere " ///
	" between a correlation of .4-.6.{p_end}" _n
di as text "{p}{txt} A more precise way to define it is to estimate an " ///
	"average between-measurement consistency. This can be done using a " ///
	"command called {manhelp R summarize}. We use the {cmd: std} option to get the " ///
	"information we need.{p_end}" _n
more
	di ". alpha bp1-bp5,std"
	alpha bp1-bp5,std
di as text _n "{p}{txt} Now you see three numbers there. The first is called the " ///
	"interitem correlation which is 0.48. That is the number we want, so you " ///
	"would say that based on your data, the reliability of using a single SBP " ///
	"to estimate a person's blood pressure is 0.48.{p_end}" _n
di as text "{p}{txt} Another way to express what that tells you is that 48% " ///
	" of the variation in SBP, given the measurement procedure used, is " ///
	"signal and the rest is noise.{p_end}" _n
di as text "{p}{txt} The other number is called the Cronbach's alpha which" ///
	"is 0.82. That tells you the reliability of using an average of all five" ///
	"SBPs to estimate the blood pressure. Obviously that measurement is more" ///
	"precise as the reliability is higher with over 80% of the variation in" ///
	"the measurement consisting of an average of 5 SBPs representing signal" ///
	"and only 18% noise.{p_end}" _n
	set more off
end

cap prog drop prob5
prog define prob5
display as text _n "{bf:Problem 5:}" _n
di as text "{p}{txt} Calculate the probability (chance) of getting three heads out of three " ///
	"flips of the coin given a fair coin, which we will specify means that " ///
	"the $Pr(heads)=0.5$.{p_end}" _n
di as text "{p}{txt} Assume the coins flips are independent, so the result on any one flip " ///
	"is not affected by the result of any other coin flip.{p_end}" _n
end

cap prog drop coin_flip
prog define coin_flip
display as text _n "{bf:Answer to Problem 5:}" _n
di as text "{p}{txt} The probability then, by the " ///
	 `" {browse "https://websites.umich.edu/~thofer/teach/hhcr-book/biostat_1.html#sec-bs1-comb_independent":{it:Rule of intersection of dependent events}}{p_end}"' _n
	di ". di 1/2 * 1/2 * 1/2"
	di 1/2 * 1/2 * 1/2
	di "For a totally negative SMA-12 panel in a well person"
	di ". .95^12"
	di round(.95^12,.01)
	di "Probability of 1 or more false positives"
	di ".1 - .95^12"
	di round(1-.95^12,.01)
end

 
cap prog drop prob6
prog define prob6
	set more on
display as text _n "{bf:Prob 6:}" _n
di as text "{p}{txt} What is the probability of getting a royal flush when drawing 5 cards " ///
	"from a deck of 52?{p_end}" _n
di as text "{p}{txt} The hardest part is to figure out how many ways we can pick 5 cards " ///
	"from 52 (if the order does not matter).{p_end}" _n
	set more off
end


cap prog drop comb_ans
prog define comb_ans
	set more on
	display as text _n "{bf:Answer to Problem 6:}" _n
di as text "{p}{txt} This is given by the formula for combination or " ///
	"n!/(r!*(n-r)!). In stata {cmd: comb(n,r)} or in this case " ///
	"{cmd: comb(52,5)}. With this formula we can answer the question.{p_end}" _n
di as text "{p}{txt} So we need $\frac{\text{number of ways event A can " ///
	"occur}}{\text{number of possible event outcomes}}${p_end}" _n
di as text "{p}{txt} There are 4 ways to get a royal flush out of all possible ways to draw " ///
	"5 cards. So the probability of a royal flush is:{p_end}" _n
	di ". di 4/comb(52,5)"
	di 4/comb(52,5)
	di _n "Which is " round(4/comb(52,5)*1000000,.01) " out of 1,000,000, or 1 in " comb(52,5)/4 " hands of poker"   
	set more off
end


cap prog drop prob7
prog define prob7
	clear
	display as text _n "{bf:Problem 7:}" _n
	di as text "{p}{txt} You have a diagnostic test that gives the results below for a " ///
	"population{p_end}" _n
	di as text "{p}{txt} 1. Calculate the pre-test (population) probability of disease {p_end}" 
	di as text "{p}{txt} 2. Write the sensitivity and specificity in the Pr(T|D) form and calcuate both.{p_end} " 
	di as text "{p}{txt} 3. Calculate the positive and negative predictive probabilities{p_end}" _n
	qui set obs 4
	gen disease = _n<=2
	gen test = _n==1|_n==3
	gen freq = (_n==1)*27 + (_n==2)*3 + (_n==3)*4 + (_n==4)*66
	noi tab disease test [fw=freq]
end

cap prog drop diag_test
prog define diag_test
	set more on
display as text _n "{bf:Answer to Problem 7:}" _n
	di as text "{txt} You can use the user command diagti (only needs to be installed once)"
	di as text ". ssc install diagti"	
	di " < ---- output omitted ----> "
	di ". diagti 27 3 4  66"
	diagti 27 3 4  66
	more
	di as text "{p}{txt} Or you can use logistic regression, first input the " ///
		"data so that it looks like this: {p_end}"
	list
	di ". logistic disease i.test [fw=freq]  // fw=freq tells stata how many of each results"
	logistic disease i.test [fw=freq]  // fw=freq tells stata how many of each results
	di ". estat classification  // this calculates the test characteristics"
	estat classification  // this calculates the test characteristics
	di ". lroc ,nograph"
	lroc ,nograph 
	set more off
end



cap prog drop prob8
prog define prob8
	set more on
display as text _n "{bf:Problem 8:}" _n
di as text "{p}{txt} Most often we are interested in the Probability of disease given a " ///
	"positive test.{p_end}" _n
di as text "{p}{txt} This can be estimated without having the results of testing in a " ///
	"population if you know the sensitivity and specificity, as well as the " ///
	"prevalence of the disease in the population to be tested.{p_end}" _n
di as text "{p}{txt} To do this you need to know bayes rule  " ///
	"and that the P(T+) can be expresses as follows{p_end}" _n
di as text "{p}{txt} P(T+)=sensitivity*Pr(D+)+(1-specificity)(1-Pr(D+)) {p_end}" _n
di as text "{p}{txt} So if Sensitivity is .95 and Specificity is .98 and disease prevalence " ///
	"is .01:{p_end}" _n
di as text `"{p}{txt} 1. Using {browse "https://websites.umich.edu/~thofer/teach/hhcr-book/biostat_1.html#sec-bs1-bayes_rule-concept":bayes rule} calculate the PPV.{p_end}"' 
di as text "{p}{txt} 2. comment on the results, given that this seems like a really good " ///
	"test, and talk about the implications for a screening program.{p_end}" _n
di as text "{p}{txt} The sensitivity and " ///
	"specificity are really high for this test. In fact when testing for " ///
	"HIV first became available this is about what the test characteristics were{p_end}"  _n 
	set more off
end



cap prog drop bayes_calc
prog define bayes_calc
	set more on
display as text _n "{bf:Answer to Problem 8:}" _n
	di ". di (0.95*0.01)/(0.95*0.01+(1-0.98)*(1-.01))"
	di (0.95*0.01)/(0.95*0.01+(1-0.98)*(1-.01))
di as text "{p}{txt} By bayes rule: Pr(Y | X) =Pr(X|Y)*Pr(Y)/Pr(X) {p_end}" _n
di as text "{p}{txt} in the case where event Y is a disease state D+, with the only other " ///
	"possibility is that there is no disease (D-), and X is a positive test " ///
	"T+ then all the possible ways to get T+ is:{p_end}" _n
di as text "{p}{txt} Pr(X)=Pr(T+)=Pr(T+|D+) * Pr(D+)+Pr(T+|D-) * Pr(D-){p_end}" _n
di as text "{p}{txt} If we defined specificity as Pr(T-|D-), then " ///
	"Pr(T+|D-)=(1-specificity) {p_end}" _n
di as text "{p}{txt} then using only the terms of diagnostic testing:{p_end}" _n
di as text "{p}{txt} PPV=(sens * prev) /(sens*prev+(1-spec)*(1-prev)) {p_end}" 
di "or (0.95*0.01)/(0.95*0.01+(1-0.98)*(1-.01))" _n
di as text "{p}{txt} Despite the high sensitivity and specificity, the pre-test probability " ///
	"is really low. This means you have to have an extraordinarily good " ///
	"test to increase the pre-test probability to a level where you could " ///
	"take clinical action (like diagnosing HIV infection and starting " ///
	"treatment) based on the test result. If the population prevalence is " ///
	"about 1% then a screening program for HIV infection would not be very " ///
	"helpful.{p_end}" _n
di as text "{p}{txt} Among people who have symptoms that suggest infection or a high risk " ///
	"exposure, the pre-test probability would be higher than the population " ///
	"average of 1% and the test was used primarily for those groups for " ///
	"many years until the sensitivity and specificity improved further and " ///
	"population screening was recommended.{p_end}" _n
	set more off
end

cap prog drop prob9
prog define prob9
	set more on
display as text _n "{bf:Problem 9:}" _n
di as text "{p}{txt} Probability of infection with 6 exposures.{p_end}" _n
di as text "{p}{txt} Graph the probability for each x (1 to 6) flu infections resulting " ///
	"from a sequence of similar interactions of 6 well people with an " ///
	"infected person where the infection rate in any interaction has a " ///
	"probability of 0.20{p_end}" _n
di as text _n "{bf:Hint}" _n
di as text `"{txt} Use the {cmd: twoway function} - click to view the {help twoway function:help twoway function}"' _n ///
	 `"{txt} or view the {browse  "https://www.stata.com/manuals/g-2graphtwowayfunction.pdf": manual entry}"'
	set more off
end

cap prog drop binom_dist
prog define binom_dist
	set more on
display as text _n "{bf:Answer to Problem 9:}" _n
di as text "{p}{txt} The binomial PDF gives us the probability of any data event given that " ///
	"we know the distribution is binomial and that the probability of " ///
	"infection for any given contact is 0.20 on average. So we can graph " ///
	"for a series of data events (1, 2, 3, 4, 5, 6) the probability of " ///
	"seeing each of those number of infections.{p_end}" _n
di as text "{p}{txt} Although you will get a graph that gives the correct probabilities " ///
	"without them, we add the options {cmd: recast(dropline) n(7)} to " ///
	"change to a graph that only shows probabilities for the integer " ///
	"numbers as we will not see fractional numbers of infections.{p_end}" _n
	di ". twoway function y=binomialp(6,x,.2),range(0 6) recast(dropline) n(7)"
	twoway function y=binomialp(6,x,.2),range(0 6) ytitle(Probability) ///
		xtitle(Number of infections) recast(dropline) n(7) ///
		note("Distribution of expected number of infections when 6 people interact with a single " ///
		"infected person and the probability of infection is 0.20 for an interaction")
	set more off
end

cap prog drop prob10
prog define prob10
	set more on
display as text _n "{bf:Problem 10:}" _n
di as text "{p}{txt} Find the probability of a sampled person having a cholesterol of less " ///
	"than or equal to 140 mg/dl, given that the population mean is 188 and " ///
	"the s.d. is 43 mg/dl.{p_end}" _n
di as text "{p}{txt} The CDF (cumulative distribution function) allows us to calculate the " ///
	"probability that an observed value sampled from a population is less " ///
	"than a specific cutoff given the mean and s.d. (see " ///
	`"this {browse "https://websites.umich.edu/~thofer/teach/hhcr-book/biostat_1.html#sec-bs1-cdf-concept":section of the chapter}){p_end}"'  _n
	set more off
end

cap prog drop cdf_ans
prog define cdf_ans
display as text _n "{bf:Answer to Problem 10:}" _n
di as text "{p}{txt} We use the stata function {cmd: normal(z)}. The z score converts an " ///
	"observation value to the number of standard deviations the value is " ///
	"from the mean.{p_end}" _n
di as text "{p}{txt} As z=(X-m)/s.d.{p_end}" _n
di as text "{p}{txt} So first transforming 140 mg/dl to a z score:{p_end}" _n
	di ". local z=(140-188)/43"
	local z=(140-188)/43
	di ". di \`z'"
	di `z'
di as text "{p}{txt} so the observation 140 mmHg is 1.11 standard deviations below the " ///
	"mean.{p_end}" _n
di as text "{p}{txt} And the probability of drawing an observation that is <= 140 mm Hg " ///
	"is:{p_end}" _n
	di ". local z=(140-188)/43"
	local z=(140-188)/43
	di `". di normal(\`z')"'
	di normal(`z')
end

cap prog drop prob11
prog define prob11
	set more on
display as text _n "{bf:Problem 11:}" _n
di as text "{p}{txt} With the inverse CDF you can find out the z score that represents any " ///
	"given probability.{p_end}" _n
di as text "{p}{txt} Using the inverse CDF find the range of cholesterol levels which " ///
	"contain 95% of the population given the mean (180 mm/dl) and standard " ///
	"deviation (43 mm/dl) in the previous problem.{p_end}" _n
di as text "{p}{txt} Another way of saying this is that we want to find the cholesterol " ///
	"level at which 2.5% of the population have a lower cholesterol __and__ " ///
	"the cholesterol level at which 97.5% of the population have a lower " ///
	"cholesterol. The range between these two levels contains 95% of the " ///
	"population.{p_end}" _n
di as text "{p}{txt} see (see " ///
	`"this {browse "https://websites.umich.edu/~thofer/teach/hhcr-book/biostat_1.html#sec-bs1-inv_cdf-concept":section of the chapter}){p_end}"'  _n
	set more off
end


cap prog drop icdf_ans
prog define icdf_ans
display as text _n "{bf:Answer to Problem 11:}" _n
di as text "{p}{txt} So if we want to calculate the range of choleterol where we will find " ///
	"95% of the observations from our population, we need to know the z " ///
	"score that represents the 2.5% and 97.5% cutoffs for the normal " ///
	"distribution. And then we can calculate the cholesterol levels that " ///
	"are associated with those z scores.{p_end}" _n
	di ". di invnormal(0.025)"
	di invnormal(0.025)
	di ". di invnormal(0.975)"
	di invnormal(0.975)
di as text "{p}{txt} We see those z scores are +/- 1.96{p_end}" _n
di as text "{p}{txt} So 95% of the cholesterol values will be between 1.96 standard " ///
	"deviations above the mean and 1.96 standard deviations below the mean:{p_end}" _n
di as text "{p}{txt} If our distribution has a mean of 130 and standard deviation of 15mm " ///
	"Hg{p_end}" _n
	di ". di 130-1.96*15"
	di 130-1.96*15
	di ". di 130+1.96*15"
	di 130+1.96*15
di as text "{p}{txt} the range for 95% of observations will be [101,159]{p_end}" _n
di as text "{txt} or in one step"
	di ". di 130 + 15*invnormal(0.025)"
	di 130 + 15*invnormal(0.025)
	di ". di 130 + 15*invnormal(0.975)"
	di 130 + 15*invnormal(0.975)	
end




