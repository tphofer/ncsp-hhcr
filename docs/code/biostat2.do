/* Directory
ex_

prob

*/


di "Welcome to the Stata environment for Chapter 2 - Bistatistics 2 _n  You are now ready to run all the examples and problems in this chapter"



* In text example 2.1 nte-bs2-ex1
cap prog drop ex1
prog define ex1
	set more on
	webuse nhanes2,clear
	di ". summarize tcresult"
	summarize tcresult
	more
	di _n "or for more statistics"
	di ". summarize tcresult,detail"
	summarize tcresult,detail
	di _n(2) "After a command many statistics are stored for future use."
	di "See them by typing {cmd: return list} or {cmd: ereturn list} for regressions"
	more
	di ". return list"
	return list
	di ". local sd = \`r(sd)'"
	local sd = `r(sd)'
	di `". di \`sd' "'
	di `sd'
end

* graph associations section example 
cap prog drop ex_models
program define ex_models
	clear
	set obs 925
	set seed 1327
	gen age=rnormal(60,10)
	gen treat=cond(age>60,uniform()>.25,uniform()>.75)
	gen female=uniform()>.5
	gen bp=130-2.5*treat*1*treat*female+2.5*(1-treat)+(age-60)*.75 +.5*female+ rnormal(0,10)
	label var bp "Blood pressure (outcome variable)"
	label var age "Patient age (predictor)"
	label define fm 0 male 1 female
	label values female fm
	label define treat 0 "Control" 1 "Treat"
	label values treat treat
	label var treat "Given BP drug (treatment)"
	de
	di " "
	di `"Click {browse "https://www.stata.com/manuals/u26.pdf#page=6":this link to learn about factor variables in stata}"'
end

***** Application Biostat - 2

cap prog drop nice_pred
prog nice_pred
	syntax , [graph]
	sysuse nlsw88.dta,clear
	qui regress wage ttl_exp
	set seed 38820
	qui gen wage_pred=_b[ttl_exp]*ttl_exp + _b[_cons] + rnormal(0,e(rmse))
	de wage wage_pred ttl_exp
	if "`graph'"=="graph" {
		twoway (scatteri . ., msize(medlarge))  ///
		 (scatteri . ., msize(medlarge)) ///
		 (scatter wage ttl_exp,pstyle(p1) mcolor(%20) ) ///
		 (scatter wage_pred ttl_exp,pstyle(p2) mcolor(%10) ), ///
		 legend(order(1 "Hourly wage" 2 "Simulated hourly wage") ///
		 pos(12) row(1) fc(%100)) xtitle(years of experience)
	}
end


cap prog drop ex_binned
prog ex_binned
	sysuse nlsw88,clear
	qui regress wage ttl_exp
	set seed 38820
	gen wage_pred=_b[ttl_exp]*ttl_exp + _b[_cons] + rnormal(0,e(rmse))
	binscatter wage ttl_exp,nodraw
	local scatter1i=substr(`"`e(graphcmd)'"',9,ustrpos(`"`e(graphcmd)'"',"(function" )-40)
	binscatter wage_pred ttl_exp,nodraw
	local scatter2i=substr(`"`e(graphcmd)'"',9,ustrpos(`"`e(graphcmd)'"',"(function" )-40)
	twoway (`scatter1i') ///
		(`scatter2i') ///
		(lfit wage ttl_exp,pstyle(p1)) ///
		(lfit wage_pred ttl_exp, pstyle(p2)), ///
		legend(order(1 "Hourly wage" 2 "Simulated hourly wage") ///
		pos(12) row(1) fc(%100)) ///
		xtitle(years of experience) name(binned,replace)
end

cap prog drop more_chks
prog more_chks
	qui use http://www.stata-press.com/data/kk/anscombe, clear 
	dtable x1-x4 y1-y4,nformat(%5.2f) column(summary("Mean (sd)"))
	qui reg y1 x1 
	qui etable, cstat(_r_b, nformat(%5.2f)) mstat(r=sqrt(e(r2)))
	qui regress y2 x2
	qui etable, cstat(_r_b, nformat(%5.2f)) mstat(r=sqrt(e(r2))) append
	qui regress y3 x3
	qui etable, cstat(_r_b, nformat(%5.2f)) mstat(r=sqrt(e(r2))) append
	qui regress y4 x4
	etable , cstat(_r_b, nformat(%5.2f)) mstat(r=sqrt(e(r2))) ///
		title("Regression results") append
end

cap prog drop gr_checks
prog gr_checks
	qui use http://www.stata-press.com/data/kk/anscombe, clear
	twoway scatter y1 x1 || lfit y1 x1,ylab(2(2)13) title("c/w model")  ///	
		legend(off) ytitle("") xtitle("") name(one,replace) nodraw
	twoway scatter y2 x2 || lfit y2 x2, ylab(2(2)13) title("Nonlinear relationship") ///
		legend(off) ytitle("")  xtitle("") name(two,replace) nodraw

	twoway scatter y3 x3 || lfit y2 x2,ylab(2(2)13) title("Outlier")   ///	
		legend(off) ytitle("") xtitle("") name(three,replace) nodraw
	twoway scatter y4 x4 || lfit y2 x2,ylab(2(2)13) title("High leverage")   ///	
		legend(off) ytitle("") xtitle("") name(four,replace) nodraw
	graph combine one two three four, imargin(0 0 0 0) graphregion(margin(l=22 r=22))
end

clear
cap prog drop birth_sim
prog define birth_sim, rclass
    drop _all
    set obs 400
    gen baby_sex=rbinomial(1,.488)
    sum baby_sex
    return scalar mean=r(mean)
end



*** FOR FUN
*===============================================================================
* poker_sim.do
*
* Purpose : Simulate dealing 5-card poker hands and classify each hand into one
*           of the ten standard rankings, tracking the determining card rank and
*           (for flush-type hands) the suit.
*
* Programs: make_deck   - builds a 52-card deck (value, suit)
*           deal_poker  - deals `reps' five-card hands from a fresh deck each
*                         replication, classifies them, and returns a results
*                         dataset (one row per replication).
*
* Notes   : Hand categories, best to worst:
*             royal flush, straight flush, four of a kind, full house, flush,
*             straight, three of a kind, two pair, one pair, high card.
*           The ace-low "wheel" (A-2-3-4-5) is recognized as a straight with a
*           high card of five.
*===============================================================================

*-------------------------------------------------------------------------------
* Build a standard 52-card deck
*-------------------------------------------------------------------------------
cap prog drop make_deck
program make_deck
	qui {
		clear all
		set obs 52
		gen order=_n
		gen value=string(mod(_n-1,13)+2)
		replace value="jack" if value=="11"
		replace value="queen" if value=="12"
		replace value="king" if value=="13"
		replace value="ace" if value=="14"
		sort value order
		gen suit=string(mod(_n,4)+1)
		replace suit="club" if suit=="1"
		replace suit="diamond" if suit=="2"
		replace suit="heart" if suit=="3"
		replace suit="spade" if suit=="4"
		sort order
		drop order
	}
end

*-------------------------------------------------------------------------------
* Deal, classify, and tabulate poker hands
*
* Syntax : deal_poker , reps(#) [seed(#)]
*   reps()  number of five-card hands to deal (required)
*   seed()  optional random-number seed for reproducibility
*
* Returns: a dataset in memory with one observation per replication:
*   replication   hand number
*   category      hand ranking (string)
*   key_rank      determining numeric rank (2-14; ace = 14, ace-low straight = 5)
*   key_rank_lab  determining rank as a label ("ace", "king", ... , "2")
*   key_suit      suit of the flush (blank for non-flush hands)
*-------------------------------------------------------------------------------
cap prog drop deal_poker
program deal_poker
	syntax , reps(integer) [seed(integer 0) detail]

	qui {
		if `seed' > 0 set seed `seed'

		* Build the deck once, attach a numeric rank, and stash it on disk so the
		* loop can draw from a fresh, complete deck every replication.
		make_deck
		gen rank = real(value)
		replace rank = 11 if value=="jack"
		replace rank = 12 if value=="queen"
		replace rank = 13 if value=="king"
		replace rank = 14 if value=="ace"

		tempfile deck results
		save `deck'

		tempname pf
		postfile `pf' int replication str20 category byte key_rank ///
			str8 key_rank_lab str8 key_suit using `results', replace

		forvalues hand = 1/`reps' {

			use `deck', clear
			sample 5, count

			*--- Suit / flush -------------------------------------------------
			egen suit_group = group(suit)
			su suit_group, meanonly
			local n_suits = r(max)
			local is_flush = (`n_suits' == 1)

			*--- Rank multiplicities -----------------------------------------
			bysort rank: gen rank_count = _N
			su rank_count, meanonly
			local max_count = r(max)

			egen rank_group = group(rank)
			su rank_group, meanonly
			local n_ranks = r(max)

			su rank, meanonly
			local high_rank = r(max)
			local low_rank  = r(min)

			*--- Straight (incl. ace-low wheel) ------------------------------
			local is_straight = 0
			local straight_high = .
			if `n_ranks' == 5 {
				count if inlist(rank, 2, 3, 4, 5, 14)
				if r(N) == 5 {
					local is_straight  = 1
					local straight_high = 5
				}
				else if `high_rank' - `low_rank' == 4 {
					local is_straight   = 1
					local straight_high = `high_rank'
				}
			}

			*--- Classify (best match first) ---------------------------------
			local key_suit ""
			if `is_flush' & `is_straight' {
				if `straight_high' == 14 {
					local category "royal flush"
				}
				else {
					local category "straight flush"
				}
				local key_rank = `straight_high'
				local key_suit = suit[1]
			}
			else if `max_count' == 4 {
				local category "four of a kind"
				su rank if rank_count == 4, meanonly
				local key_rank = r(mean)
			}
			else if `max_count' == 3 & `n_ranks' == 2 {
				local category "full house"
				su rank if rank_count == 3, meanonly
				local key_rank = r(mean)
			}
			else if `is_flush' {
				local category "flush"
				local key_rank = `high_rank'
				local key_suit = suit[1]
			}
			else if `is_straight' {
				local category "straight"
				local key_rank = `straight_high'
			}
			else if `max_count' == 3 {
				local category "three of a kind"
				su rank if rank_count == 3, meanonly
				local key_rank = r(mean)
			}
			else if `max_count' == 2 & `n_ranks' == 3 {
				local category "two pair"
				su rank if rank_count == 2, meanonly
				local key_rank = r(max)
			}
			else if `max_count' == 2 {
				local category "one pair"
				su rank if rank_count == 2, meanonly
				local key_rank = r(mean)
			}
			else {
				local category "high card"
				local key_rank = `high_rank'
			}

			*--- Rank label --------------------------------------------------
			local key_rank_lab "`key_rank'"
			if `key_rank' == 11 local key_rank_lab "jack"
			if `key_rank' == 12 local key_rank_lab "queen"
			if `key_rank' == 13 local key_rank_lab "king"
			if `key_rank' == 14 local key_rank_lab "ace"

			post `pf' (`hand') ("`category'") (`key_rank') ///
				("`key_rank_lab'") ("`key_suit'")
		}

		postclose `pf'
		use `results', clear
	}
	di _n "Hands by rank"
	qui table category
	collect style header category, title(hide)
	collect preview
	if "`detail'"=="detail" {
		di _n "{bf:Detailed breakdown}"
		qui table (category key_rank_lab) , nototals // totals(category)
		collect style row split, dups(first)
		collect style header category key_rank_lab, title(hide)
		collect label levels category _tot "{bf:Total}", modify
		collect preview
	}
end
* 3030 is a good one full house and straight 

