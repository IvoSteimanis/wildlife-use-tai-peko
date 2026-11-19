*==============================================================================
* 18_robustness_referee.do  (R1, 2026-11-19)
*
* Reviewer 1 asked (R1-ST4, ST5, ST7) for: diagnostics on the village-level
* distance gradients, a count model, and a pooled model with park x distance.
* Estimand unchanged: the slope of reported species richness on distance to the
* park boundary, one observation per village, separately by park.
*
*   A. Poisson GLM with robust SE; slope reported as % change per km (100*(e^b-1))
*      and as the average marginal effect in species per km, comparable to OLS.
*   B. OLS with HC3 standard errors (heteroskedasticity, small n).
*   C. Leave-one-village-out: min and max OLS slope, and number of drops after
*      which p >= 0.05 (for slopes significant at full sample).
*   D. Cook's distance: number of villages with D > 4/n.
*   E. Pooled OLS: richness on distance, park and distance x park.
*
* Input : ../processed/localities.csv (36 villages)
* Output: ../processed/computed_robustness_referee.csv
*==============================================================================
version 16
capture log close _all
clear all
set more off
capture mkdir "../output"
log using "../output/02_robustness.log", replace text

local P "../data"
local outcomes richness_all richness_consumed richness_secondary richness_threatened

import delimited "`P'/villages.csv", clear varnames(1) case(preserve) encoding("utf-8")
destring distance_km richness_* n_respondents, replace force
assert _N == 36
assert n_respondents > 0 & !missing(n_respondents)
gen double ln_n = ln(n_respondents)
gen byte mpnp = park == "MPNP"
gen int vid = _n

tempfile res
postfile pf str5 park str20 outcome double(n b_ols p_ols p_hc3 b_pois_pct p_pois ame_pois ///
    loo_min loo_max loo_flip cook_n b_int p_int p_quad b_off_pct p_off b_eff_pct p_eff elast p_elast) using `res', replace

foreach y of local outcomes {
    * E. pooled interaction (one per outcome, stored on both rows)
    quietly regress `y' c.distance_km##i.mpnp
    local bint = _b[1.mpnp#c.distance_km]
    local pint = 2*ttail(e(df_r), abs(`bint'/_se[1.mpnp#c.distance_km]))

    foreach pk in TNP MPNP {
        preserve
        quietly keep if park == "`pk'"
        local n = _N
        quietly regress `y' distance_km
        local b = _b[distance_km]
        local p = 2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))
        quietly predict double cd, cooksd
        quietly count if cd > 4/`n' & !missing(cd)
        local cookn = r(N)
        quietly regress `y' distance_km, vce(hc3)
        local p3 = 2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))

        quietly poisson `y' distance_km, vce(robust)
        local bp = 100*(exp(_b[distance_km]) - 1)
        local pp = 2*normal(-abs(_b[distance_km]/_se[distance_km]))
        quietly margins, dydx(distance_km)
        matrix M = r(table)
        local ame = M[1,1]

        * F. linearity: does a squared distance term improve the OLS fit?
        quietly regress `y' c.distance_km##c.distance_km
        local pq = 2*ttail(e(df_r), abs(_b[c.distance_km#c.distance_km]/_se[c.distance_km#c.distance_km]))

        * G. survey effort as a Poisson offset, ln(respondents per village). Strong
        *    assumption: richness proportional to interviews, whereas species counts
        *    saturate with effort, so this is a deliberately conservative check.
        quietly poisson `y' distance_km, offset(ln_n) vce(robust)
        local bo = 100*(exp(_b[distance_km]) - 1)
        local po = 2*normal(-abs(_b[distance_km]/_se[distance_km]))
        * H. the same with ln(respondents) as a free covariate: the data estimate the
        *    effort elasticity instead of fixing it at 1 as the offset does.
        quietly poisson `y' distance_km ln_n, vce(robust)
        local bf = 100*(exp(_b[distance_km]) - 1)
        local pf_ = 2*normal(-abs(_b[distance_km]/_se[distance_km]))
        local el = _b[ln_n]
        local elp = 2*normal(-abs(_b[ln_n]/_se[ln_n]))

        local lmin = .
        local lmax = .
        local flip = 0
        quietly levelsof vid, local(vs)
        foreach v of local vs {
            quietly regress `y' distance_km if vid != `v'
            local bl = _b[distance_km]
            local pl = 2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))
            local lmin = min(`lmin', `bl')
            local lmax = max(`lmax', `bl')
            if `p' < 0.05 & `pl' >= 0.05 local flip = `flip' + 1
        }
        post pf ("`pk'") ("`y'") (`n') (`b') (`p') (`p3') (`bp') (`pp') (`ame') ///
            (`lmin') (`lmax') (`flip') (`cookn') (`bint') (`pint') (`pq') (`bo') (`po') (`bf') (`pf_') (`el') (`elp')
        display as text "`pk' `y': quad p=" %6.3f `pq' " | offset %/km=" %6.2f `bo' " p=" %6.3f `po' ///
            " | free %/km=" %6.2f `bf' " p=" %6.3f `pf_' " elasticity=" %6.3f `el' " p=" %6.3f `elp'
        display as text "`pk' `y': OLS b=" %7.3f `b' " p=" %6.3f `p' " HC3 p=" %6.3f `p3' ///
            " | Pois %/km=" %6.2f `bp' " p=" %6.3f `pp' " AME=" %7.3f `ame' ///
            " | LOO [" %7.3f `lmin' ", " %7.3f `lmax' "] flips=" `flip' " cook>4/n=" `cookn'
        restore
    }
    display as text "`y': pooled distance x MPNP = " %7.3f `bint' "  p=" %6.3f `pint'
}
postclose pf
use `res', clear
export delimited "../output/robustness_estimates.csv", replace
display "ROBUSTNESS_DONE"
log close
