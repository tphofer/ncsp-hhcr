* Cox model on the survival::lung data exported from R
version 17
clear all
set more off

use "lung.dta", clear

* Declare survival data: time in days, failure = death
stset time, failure(stata_status == 1)

* ------------------------------------------------------------------
* Replicating the survminer README examples (https://rpkgs.datanovia.com/survminer/)
* R: fit <- survfit(Surv(time, status) ~ sex, data = lung)
* ------------------------------------------------------------------

* Kaplan-Meier estimates by sex (equivalent of print/summary of survfit)
sts list, by(sex) compare at(0(100)1000)

* Log-rank test (survminer's pval = TRUE)
sts test sex, logrank
local lr_p = chi2tail(r(df), r(chi2))
local lr_p : display %6.4f `lr_p'

* Median survival by sex (survminer's surv.median.line)
stci, by(sex) median
forvalues s = 1/2 {
    quietly stci if sex == `s', median
    local med`s' = r(p50)
}

* 1. Basic KM plot: ggsurvplot(fit, data = lung)
sts graph, by(sex) name(km_basic, replace)

* 2. Censor marks: censor.shape = "|"
sts graph, by(sex) censored(single) name(km_censor, replace)

* 3. Customized: conf.int, pval, risk table colored by strata
sts graph, by(sex) ci censored(single)                        ///
    plot1opts(lcolor("231 184 0")) plot2opts(lcolor("46 159 223")) ///
    ci1opts(color("231 184 0%30")) ci2opts(color("46 159 223%30")) ///
    risktable(, order(1 "Male" 2 "Female") color(black))       ///
    legend(order(5 "Male" 6 "Female") rows(1) position(12))   ///
    text(0.1 50 "Log-rank p = `lr_p'", placement(e))          ///
    title("Survival by sex") scheme(s1mono)                    ///
    name(km_custom, replace)

* 4/5. More customized: xlim 0-500, breaks every 100, median lines
sts graph, by(sex) ci censored(single)        ///
    tmax(500) xlabel(0(100)500) xtitle("Time in days")         ///
    ytitle("Survival probability")                             ///
    plot1opts(lcolor("231 184 0")) plot2opts(lcolor("46 159 223")) ///
    ci1opts(color("231 184 0%30")) ci2opts(color("46 159 223%30")) ///
    risktable(0(100)500, order(1 "Male" 2 "Female"))           ///
    addplot(scatteri 0.5 0 0.5 `med2', recast(line) lpattern(dash) lcolor(gs8) || ///
            scatteri 0.5 `med1' 0 `med1', recast(line) lpattern(dash) lcolor(gs8) || ///
            scatteri 0.5 `med2' 0 `med2', recast(line) lpattern(dash) lcolor(gs8)) ///
    legend(order(5 "Male" 6 "Female") rows(1) position(12))   ///
    text(0.1 20 "Log-rank p = `lr_p'", placement(e))          ///
    title("Survival curves") subtitle("Based on Kaplan-Meier estimates") ///
    note("Data: survival::lung") name(km_uber, replace)

* 6. Number-censored plot (survminer's ncensor.plot = TRUE)
histogram time if stata_status == 0 & time <= 500, frequency  ///
    width(20) start(0) by(sex, rows(2) note(""))              ///
    xlabel(0(100)500) xtitle("Time in days")                   ///
    ytitle("Number censored") name(ncensor, replace)

* Combine the KM plot and the censoring plot
graph combine km_uber ncensor, cols(1) ysize(8) name(km_combined, replace)

* ------------------------------------------------------------------
* Cox proportional hazards model (hazard ratios)
stcox age i.sex i.ph_ecog

* Proportional hazards check (Schoenfeld residuals)
estat phtest, detail
