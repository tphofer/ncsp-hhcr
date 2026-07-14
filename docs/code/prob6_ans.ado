cap prog drop prob6_ans
prog define prob6_ans
	set more on
di as text `"{p}{txt} cap prog drop prob6_ans prog define prob6_ans set more on di as text "' ///
	`""{p}{txt} This is given by the formula for combination or $\Large " "' ///
	`"/// "C_r^n=\frac{n!}{r!(n-r)!}$. In stata {cmd: comb(n,r)} or in this "' ///
	`"case " /// "{cmd: comb(52,5)}. With this formula we can answer the "' ///
	`"question.{p_end}" _n di as text "{p}{txt} So we need "' ///
	`"$\frac{\text{number of ways event A can " /// "occur}}{\text{number of "' ///
	`"possible event outcomes}}${p_end}" _n di as text "{p}{txt} There are 4 "' ///
	`"ways to get a royal flush out of all possible ways to draw " /// "5 "' ///
	`"cards.{p_end}" _n di ". di 4/comb(52,5)" di 4/comb(52,5) set more off "' ///
	`"end{p_end}"' _n
	set more off
end
