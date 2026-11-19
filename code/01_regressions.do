*------------------------------------------------------------------------------
* 01_regressions.do -- distance-gradient regressions.
*
* Reproduces the eight regressions reported in the article and the survey-effort
* sensitivity analysis reported in Table S5.
*
* Unit of analysis is the village, one observation per village, with the two parks
* fitted separately: 21 villages around Taï National Park and 15 around Mont Péko
* National Park. Species richness is regressed on distance to the park boundary.
*
* The article reports the UNWEIGHTED fit. The estimand is the average village, not
* the average person interviewed. Weighting each village by its respondents would
* give roughly 40% of the weight to the eight settlements holding 39 to 41
* respondents against a median of 11, four of which are sub-prefecture seats rather
* than villages. Both weighted and effort-adjusted specifications are reported below
* for sensitivity.
*
* Input  : ../data/villages.csv
* Outputs: ../output/tables/table_S3_regressions.tex
*          ../output/tables/table_S8_effort.tex
*          ../output/regression_estimates.csv
*          ../output/effort_sensitivity.csv
*
* Requires: Stata 17 or later, and estout (ssc install estout).
* Run from the code/ directory.
*------------------------------------------------------------------------------

capture log close _all
version 16
set more off
set linesize 200

local T "../output/tables"
local BS "\"
local outcomes richness_all richness_consumed richness_secondary richness_threatened

capture mkdir "../output"
capture mkdir "../output/tables"

import delimited "../data/villages.csv", clear varnames(1) case(preserve) ///
    encoding("utf-8")
destring distance_km richness_* n_respondents, replace force

tab park
assert _N == 36
count if park == "TNP"
assert r(N) == 21
count if park == "MPNP"
assert r(N) == 15

*==============================================================================
* The eight regressions reported in the article
*==============================================================================
eststo clear
tempfile est
postfile pf str5 park str20 outcome double(n slope se ci_lo ci_hi intercept r2 df p) ///
    using `est', replace

foreach pk in TNP MPNP {
    foreach y of local outcomes {
        quietly regress `y' distance_km if park == "`pk'"
        eststo `pk'_`y'
        local b  = _b[distance_km]
        local se = _se[distance_km]
        local df = e(df_r)
        local pv = 2*ttail(`df', abs(`b'/`se'))
        post pf ("`pk'") ("`y'") (e(N)) (`b') (`se') ///
            (`b' - invttail(`df',0.025)*`se') (`b' + invttail(`df',0.025)*`se') ///
            (_b[_cons]) (e(r2)) (`df') (`pv')
        display as text "`pk' `y': " as result "b=" %8.4f `b' ///
            as text "  R2=" %6.4f e(r2) "  n=" e(N) "  p=" %7.4f `pv'
    }
}
postclose pf

preserve
    use `est', clear
    export delimited "../output/regression_estimates.csv", replace
restore

capture which esttab
if _rc {
    display as error "estout not installed. Run: ssc install estout"
    exit 199
}

esttab TNP_richness_all TNP_richness_consumed TNP_richness_secondary ///
       TNP_richness_threatened MPNP_richness_all MPNP_richness_consumed ///
       MPNP_richness_secondary MPNP_richness_threatened ///
    using "`T'/table_S3_regressions.tex", replace booktabs nonumbers nostar ///
    cells(b(fmt(3)) se(par fmt(3)) p(par([ ]) fmt(3))) ///
    mtitles("All forms" "Consumed" "Second." "Threat." ///
            "All forms" "Consumed" "Second." "Threat.") ///
    mgroups("Taï National Park" "Mont Péko National Park", ///
            pattern(1 0 0 0 1 0 0 0) span ///
            prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
    coeflabels(distance_km "Distance to boundary (km)" _cons "Constant") ///
    stats(N r2, labels("Villages" "\$R^2\$") fmt(0 3))

*==============================================================================
* Sensitivity to survey effort
*
* Respondents per village run from 6 to 41. Species richness rises with the number
* of people asked, whatever the wildlife present, but effort is uncorrelated with
* distance to the boundary, so it cannot bias the gradient.
*==============================================================================
tempfile estw
postfile pw str5 park str20 outcome str8 spec double(n slope se p r2) using `estw', replace

foreach pk in TNP MPNP {
    foreach y of local outcomes {
        quietly regress `y' distance_km if park == "`pk'"
        post pw ("`pk'") ("`y'") ("ols") (e(N)) (_b[distance_km]) (_se[distance_km]) ///
            (2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))) (e(r2))

        quietly regress `y' distance_km [aweight = n_respondents] if park == "`pk'"
        post pw ("`pk'") ("`y'") ("wls") (e(N)) (_b[distance_km]) (_se[distance_km]) ///
            (2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))) (e(r2))

        quietly regress `y' distance_km n_respondents if park == "`pk'"
        post pw ("`pk'") ("`y'") ("effort") (e(N)) (_b[distance_km]) (_se[distance_km]) ///
            (2*ttail(e(df_r), abs(_b[distance_km]/_se[distance_km]))) (e(r2))
    }
}
postclose pw

preserve
    use `estw', clear
    keep park outcome spec slope p
    reshape wide slope p, i(park outcome) j(spec) string

    gen byte park_ord = cond(park == "TNP", 1, 2)
    gen byte out_ord = .
    replace out_ord = 1 if outcome == "richness_all"
    replace out_ord = 2 if outcome == "richness_consumed"
    replace out_ord = 3 if outcome == "richness_secondary"
    replace out_ord = 4 if outcome == "richness_threatened"
    gen str26 label = ""
    replace label = "All forms of use"   if outcome == "richness_all"
    replace label = "Consumed"           if outcome == "richness_consumed"
    replace label = "Secondary use"      if outcome == "richness_secondary"
    replace label = "Threatened species" if outcome == "richness_threatened"
    sort park_ord out_ord

    file open tex using "`T'/table_S8_effort.tex", write replace text
    file write tex "`BS'begin{tabular}{llrrrrrr}" _n "`BS'toprule" _n
    file write tex " & & `BS'multicolumn{2}{c}{OLS} & `BS'multicolumn{2}{c}{Weighted} & `BS'multicolumn{2}{c}{+ effort} `BS'`BS'" _n
    file write tex "`BS'cmidrule(lr){3-4} `BS'cmidrule(lr){5-6} `BS'cmidrule(lr){7-8}" _n
    file write tex "Park & Outcome & Slope & \$p\$ & Slope & \$p\$ & Slope & \$p\$ `BS'`BS'" _n
    file write tex "`BS'midrule" _n
    forvalues i = 1/`=_N' {
        local pk = park[`i']
        local parkname = cond("`pk'" == "TNP", "Taï", "Mont Péko")
        local lb = label[`i']
        file write tex "`parkname' & `lb' & " %7.3f (slopeols[`i']) " & " %5.3f (pols[`i']) ///
            " & " %7.3f (slopewls[`i']) " & " %5.3f (pwls[`i']) ///
            " & " %7.3f (slopeeffort[`i']) " & " %5.3f (peffort[`i']) " `BS'`BS'" _n
    }
    file write tex "`BS'bottomrule" _n
    file write tex "`BS'end{tabular}" _n
    file close tex
    export delimited "../output/effort_sensitivity.csv", replace
restore

display "MARKER_DONE 01_regressions"
