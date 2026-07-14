cap prog drop prob4_ans
prog define prob4_ans
	set more on
display as text _n "{bf:Answer:}" _n
	di ". corr bp1-bp5"
	corr bp1-bp5
di as text "{p}{txt} This gives you a bunch of correlations and if you were trying to " ///
	"describe the average consistency you would say it is somewhere between " ///
	"a correlation of .4-.6{p_end}" _n
di as text "{p}{txt} A more precise way to define it is to estimate an average " ///
	"between-measurement consistency. This can be done using a command " ///
	"called {cmd: alpha}. We use the {cmd: std} option to get the " ///
	"information we need.{p_end}" _n
more
	di ". run http://www-personal.umich.edu/~thofer/demo/stata_examples.do"
	run http://www-personal.umich.edu/~thofer/demo/stata_examples.do
	di ". qui ex_reliab"
	qui ex_reliab
	di ". corr bp1-bp5"
	corr bp1-bp5
	di ". alpha bp1-bp5,std"
	alpha bp1-bp5,std
di as text _n "{p}{txt} Now you see three numbers there. The first is called the interitem " ///
	"correlation which is 0.48. That is the number we want, so you would " ///
	"say that based on your data, the reliability of using a single SBP to " ///
	"estimate a person's blood pressure is 0.48.{p_end}" _n
di as text "{p}{txt} Another way to express what that tells you is that 48% of the " ///
	"variation in SBP, given the measurement procedure used, is signal and " ///
	"the rest is noise.{p_end}" _n
di as text "{p}{txt} The other number is called the Cronbach's alpha which is 0.82. That " ///
	"tells you the reliability of using an average of all five SBPs to " ///
	"estimate the blood pressure. Obviously that measurement is more " ///
	"precise as the reliability is higher with over 80% of the variation in " ///
	"the measurement consisting of an average of 5 SBPs representing signal " ///
	"and only 18% noise.{p_end}" _n
	set more off
end
