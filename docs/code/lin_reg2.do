/* Directory


*/


di "Welcome to the Stata environment for Linear regression II  You are now ready to run all the examples and problems in this chapter"

cap program drop load_data
program load_data
    use https://tphofer.github.io/ncsp-hhcr/data/rhc_class_tph.dta,clear
    * LOS variables
    generate los = dschdte - sadmdte
    label variable los "Hospital length of stay (days)"
    drop if missing(los) | los <= 0
    generate lnlos = ln(los)
    label variable lnlos "log length of stay"
    drop if missing(los) | los <= 0
    * In-hospital disposition: death date on or before discharge date.
    generate byte diedhosp = !missing(dthdte) & dthdte <= dschdte
    label define disp 0 "Discharged alive" 1 "Died in hospital"
    label values diedhosp disp
    label variable diedhosp "In-hospital disposition"
    de
    notes
end

* Part 1

cap program drop round1
program round1
    regress los i.cat1 c.surv2md1 c.age i.sex
end

cap program drop ans_x2d
program ans_x2d
    set seed 20260912
    predict double xb_naive
    generate double los_sim = xb_naive + rnormal(0, e(rmse))
    count if los_sim < 0
    twoway (kdensity los, lcolor(navy) lwidth(thick)) ///
        (kdensity los_sim, lcolor(cranberry) lwidth(thick)), ///
        xline(0, lpattern(dash) lcolor(red)) ///
        legend(order(1 "Observed LOS" 2 "Simulated from model") ring(0) pos(2)) ///
        xtitle("Length of stay (days)") ///
        title("The model invents impossible negative stays")
end

* Part 2  looking at raw data

cap prog drop raw_vs_log
prog raw_vs_log
    histogram los, width(10) fcolor(midblue%40) lcolor(white) ///
        xtitle("Length of stay (days)") title("Raw LOS: heavy right skew") ///
        name(d1, replace)
    histogram lnlos, fcolor(midblue%40) lcolor(white) ///
        xtitle("log length of stay") title("log LOS: roughly symmetric") ///
        name(d2, replace)
    graph combine d1 d2, rows(1)
end

cap prog drop by_disease
prog by_disease
    graph hbox los, over(cat1, sort(1)) nooutside ///
        title("LOS varies a lot by disease category")
    table cat1, statistic(count los) statistic(median los) statistic(mean los)
end

cap prog drop by_dnr_died
prog by_dnr_died
    twoway (kdensity los if diedhosp==0, lcolor(navy)) ///
        (kdensity los if diedhosp==1, lcolor(cranberry)), ///
        legend(order(1 "Discharged alive" 2 "Died in hospital") ring(0) pos(2)) ///
        xtitle("length of stay") title("LOS mixes survivors and deaths") ///
        name(g1, replace)
    twoway (kdensity los if dnr1==0, lcolor(navy)) ///
        (kdensity los if dnr1==1, lcolor(cranberry)), ///
        legend(order(1 "Not DNR" 2 "DNR day 1") ring(0) pos(2)) ///
        xtitle("length of stay") title("Shorter stays under day-1 DNR") ///
        name(g2, replace)
    graph combine g1 g2, rows(1)
end

cap prog drop across_sev
prog across_sev
    twoway (lowess los surv2md1, lcolor(black) lwidth(thick) lpattern(dash)) ///
        (lowess los surv2md1 if diedhosp==0, lcolor(navy) lwidth(thick)) ///
        (lowess los surv2md1 if diedhosp==1, lcolor(cranberry) lwidth(thick)), ///
        legend(order(1 "All patients (pooled)" 2 "Discharged alive" ///
                        3 "Died in hospital") ring(0) pos(1) cols(1)) ///
        xtitle("Predicted 2-month survival at day 1") ytitle("LOS (days)") ///
        title("The pooled inverted-U is a mixture of two opposite trends")
end

* Part 3

cap program drop round2
program round2
    regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    capture drop e_resid e_expresid
    predict double e_resid, residuals
    generate double e_expresid = exp(e_resid)
    summarize e_expresid, meanonly
    scalar duan = r(mean)
    display "Duan smearing factor = " duan
end

cap program drop pr_by_dz
program pr_by_dz
    margins cat1, expression(duan*exp(xb()))
    marginsplot, horizontal recast(scatter) ///
        title("Predicted mean LOS by disease category") ///
        xtitle("Predicted LOS (days)")
end

cap program drop dnr_by_dz
program dnr_by_dz
    quietly regress lnlos i.cat1##i.dnr1 c.surv2md1##c.surv2md1 c.age i.sex
    testparm i.cat1#i.dnr1
    estimates store m_interact
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    estimates store m_additive
    estimates stats m_additive m_interact
    qui margins cat1#dnr1, expression(duan*exp(xb()))
    marginsplot, recast(scatter) xlabel(, angle(45))
    qui graph export img/dnr_by_dz.png, replace width(1400)
end

cap program drop pr_dnr_surv
program pr_dnr_surv
    quietly regress lnlos i.cat1##i.dnr1 c.surv2md1##c.surv2md1 c.age i.sex
    qui margins dnr1, expression(duan*exp(xb()))
    marginsplot, recast(scatter) ///
        title("Predicted mean LOS: DNR vs not") ///
        ytitle("Predicted LOS (days)") name(dnr,replace)
    qui margins, at(surv2md1=(0.1(0.05)0.9)) expression(duan*exp(xb()))
    marginsplot, recast(line) recastci(rarea) ciopts(color(%20)) ///
        title("Predicted mean LOS across the severity range (an inverted-U)") ///
        ytitle("Predicted LOS (days)") xtitle("Predicted 2-month survival") ///
        name(survival,replace)
end


cap program drop pr_los_age
program pr_los_age
    qui margins, at(age=(30(10)90)) expression(duan*exp(xb()))
    marginsplot, recast(line) recastci(rarea) ciopts(color(%20)) ///
        title("Predicted mean LOS across age") ytitle("Predicted LOS (days)")
end

* Part 4
cap program drop surv_by_dispo
program surv_by_dispo
    quietly regress lnlos c.surv2md1##c.surv2md1##i.diedhosp
    margins diedhosp, at(surv2md1=(0.1(0.05)0.9)) expression(exp(xb()))
    marginsplot, recast(line) ///
        title("Two trajectories: survivors vs in-hospital deaths") ///
        ytitle("Predicted median LOS (days)") xtitle("Predicted 2-month survival")
end

* Part 5
** Ans 1

cap program drop p5a1_raw_days
program p5a1_raw_days
    quietly regress los i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    predict xb_raw, xb
    local rmse_raw = e(rmse)
    set seed 20260912
    local call ""
    forvalues i = 1/20 {
        quietly generate los_rep`i' = xb_raw + rnormal(0, `rmse_raw')
        local call `call' (kdensity los_rep`i', lcolor(gs13) lwidth(vthin))
    }
    twoway `call' (kdensity los, lcolor(navy) lwidth(thick)), legend(off) ///
        xline(0, lpattern(dash) lcolor(red)) xtitle("Length of stay (days)") ///
        title("Raw-days model: replications invent negative stays") name(raw_days,replace)
    generate pit_raw = normal((los - xb_raw)/`rmse_raw')
    foreach lev in 50 90 {
        local lo = (100 - `lev')/200
        local hi = 1 - `lo'
        quietly count if inrange(los, xb_raw + invnormal(`lo')*`rmse_raw', ///
                                    xb_raw + invnormal(`hi')*`rmse_raw')
        display "RAW coverage of central " `lev' "% = " %4.1f 100*r(N)/_N "% (nominal " `lev' "%)"
    }
    histogram pit_raw, width(0.05) percent fcolor(midblue%50) lcolor(white) ///
        yline(5, lpattern(dash) lcolor(red)) xtitle("PIT value") ///
        title("Raw model: PIT not flat -> miscalibrated") name(PIT,replace)
end

** Ans 2
cap program drop p5a2_lnlos_days
program p5a2_lnlos_days
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    predict xb_log, xb
    local rmse_log = e(rmse)
    set seed 20260912
    local call ""
    forvalues i = 1/20 {
        quietly generate lnlos_rep`i' = xb_log + rnormal(0, `rmse_log')
        local call `call' (kdensity lnlos_rep`i', lcolor(gs13) lwidth(vthin))
    }
    twoway `call' (kdensity lnlos, lcolor(navy) lwidth(thick)), legend(off) ///
        xtitle("log length of stay") title("log model: replications track the data") ///
        name(lnlos_days,replace)
    generate pit_log = normal((lnlos - xb_log)/`rmse_log')
    foreach lev in 50 90 {
        local lo = (100 - `lev')/200
        local hi = 1 - `lo'
        quietly count if inrange(lnlos, xb_log + invnormal(`lo')*`rmse_log', ///
                                        xb_log + invnormal(`hi')*`rmse_log')
        display "LOG coverage of central " `lev' "% = " %4.1f 100*r(N)/_N "% (nominal " `lev' "%)"
    }
    histogram pit_log, width(0.05) percent fcolor(midblue%50) lcolor(white) ///
        yline(5, lpattern(dash) lcolor(red)) xtitle("PIT value") ///
        title("log model: PIT much flatter -> calibrated") name(lnlos_PIT)
end

** Ans 3  - model_comp

cap program drop p5a3_model_comp
program p5a3_model_comp
    quietly regress los i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    quietly predict double xbr, xb
    local rr = e(rmse)
    quietly count if xbr < 0
    scalar neg_raw = r(N)
    qui foreach lev in 50 90 {
        local lo = (100 - `lev')/200
        local hi = 1 - `lo'
        quietly count if inrange(los, xbr + invnormal(`lo')*`rr', xbr + invnormal(`hi')*`rr')
        scalar cov`lev'_raw = 100*r(N)/_N
    }
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    quietly predict double xbl, xb
    local rl = e(rmse)
    qui foreach lev in 50 90 {
        local lo = (100 - `lev')/200
        local hi = 1 - `lo'
        quietly count if inrange(lnlos, xbl + invnormal(`lo')*`rl', xbl + invnormal(`hi')*`rl')
        scalar cov`lev'_log = 100*r(N)/_N
    }
    display "                              naive RAW-days      final LOG model"
    display "impossible (<0) predictions  " %8.0f neg_raw  "            " %8.0f 0
    display "50% interval coverage        " %7.1f cov50_raw "%           " %7.1f cov50_log "%"
    display "90% interval coverage        " %7.1f cov90_raw "%           " %7.1f cov90_log "%"
end

* Part 6

** Ans 1

cap program drop p6a1_fnxn
program p6a1_fnxn
    quietly regress lnlos i.cat1 c.surv2md1 i.dnr1 c.age i.sex
    capture noisily acprplot surv2md1, lowess lsopts(lcolor(cranberry)) ///
        title("Component-plus-residual: surv2md1 (linear fit)")
    estat ovtest
    estimates store ff_linear
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    estimates store ff_quad
    estimates stats ff_linear ff_quad
end

** Ans 2 endogeneity - no checks
** Ans 3

cap program drop p6a3_indep
program p6a3_indep
    quietly regress lnlos c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex i.cat1
    estimates store se_default
    quietly regress lnlos c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex i.cat1, vce(cluster cat1)
    estimates store se_cluster
    estimates table se_default se_cluster, keep(1.dnr1 age) b(%6.3f) se ///
    title("Default vs. diagnosis-clustered standard errors")
end

** Ans 4

cap program drop p6a4_hetero
program p6a4_hetero
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    rvfplot, yline(0) title("log LOS: residual spread roughly even")
    estimates store hs_default
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex, vce(robust)
    estimates store hs_robust
    estimates table hs_default hs_robust, keep(1.dnr1 age) b(%6.3f) se ///
        title("Default vs. heteroskedasticity-robust standard errors")
end

** Ans 5

cap program drop p6a5_normal
program p6a5_normal
    quietly regress lnlos i.cat1 c.surv2md1##c.surv2md1 i.dnr1 c.age i.sex
    predict r_log, resid
    qnorm r_log, title("log LOS residuals: close to normal")
end
