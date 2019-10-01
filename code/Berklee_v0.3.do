clear all
// global DataFile "..\data\Berklee Audition and Ensemble Data vF.dta"
global DataFile "D:\Projects\Berklee\data\Berklee Audition and Ensemble Data vF.dta"
global OutDir "D:\Projects\Berklee\report"
global Predictors "audition_preparedpiece audition_reading audition_improv audition_melodic audition_rhythmic audition_score scholarship_rating"

use "$DataFile", clear

/* Specify outcome variable */
global Outcome "y_ensemble"

kapwgt exact 1 \ 0 1 \ 0 0 1 \ 0 0 0 1 \ 0 0 0 0 1 \ 0 0 0 0 0 1
kapwgt within1 1 \ 1 1 \ 0 1 1 \ 0 0 1 1 \ 0 0 0 1 1 \ 0 0 0 0 1 1
kapwgt with5 1 \ .5 1 \ 0 .5 1 \ 0 0 .5 1 \ 0 0 0 .5 1 \ 0 0 0 0 .5 1
global kapweight "exact"

/* Clean & recode data */

label define audition_lbl 0 "None" 1 "Beginner" 2 "Intermediate" 3 "Strong" 4 "Advanced"

// Destring string score
destring audition_preparedpiece, replace

// Encode string audition vars & apply label; remove "Low-Intermediate"
foreach var of varlist audition_reading audition_melodiccr audition_rhythmiccr {
	encode `var', gen(_num_`var') label(audition_lbl)
	replace _num_`var' = 1 if `var' == "Low-Intermediate"
	tab `var' _num_`var', mi
	rename `var' _`var'
	rename _num_`var' `var'
}

encode enteringcohort, gen(cohort)
rename instrument _instrument
encode _instrument, gen(instrument)

// Remove trailing "cr"
foreach var of varlist audition_melodiccr audition_rhythmiccr {
	local newvar = subinstr("`var'", "cr", "", .)
	rename `var' `newvar'
}

// Prefix to exclude from "audition*"
rename audition_date _audition_date

// Quadratic & cubic terms
foreach var of varlist $Predictors {
	gen p2_`var' = (`var' - 3) ^ 2
	gen p3_`var' = (`var' - 3) ^ 3
}

// Reassign 7s to 6
recode ensemble_overall (7=6), gen(y_ensemble)

// Random cross-validation strata
set seed 123
forvalues i = 1/5 {
	gen _ru = runiform()
	xtile _xv`i' = _ru, nq(5)
	drop _ru
}

// SVM variables
xi i.instrument, noomit
renpfix _Iinstrumen inst

foreach var of varlist audition_reading  audition_melodic audition_rhythmic {
	gen sv_`var' = (`var' - 2)/2
}

foreach var of varlist audition_preparedpiece audition_improv audition_score scholarship_rating {
	gen sv_`var' = (`var' - 4.5)/3.5
}

/*
foreach var of varlist sv_* {
	forvalues i = 1/8 {
		gen `var'Xinst_`i' = `var'*inst_`i'
	}
}
*/

// Drop any missing
qui reg $Outcome $Predictors instrument
keep if e(sample)


/* Custom commands */

program predictcat
	syntax [newvarname] [if]

	if "`varlist'" == "" {
		local varlist "category"
	}

	local estcmd = e(cmd)

	quietly {

		if "`estcmd'" == "regress" || "`estcmd'" == "svmachines" {
			predict `varlist' `if'
			replace `varlist' = round(`varlist')
			replace `varlist' = 1 if `varlist' < 1 & !missing(`varlist')
			replace `varlist' = 6 if `varlist' > 6 & !missing(`varlist')
		}
		else {
			predict `varlist'_p* `if'

			desc `varlist'_p*, varlist
			local wc: word count `r(varlist)'

			egen _maxp = rowmax(`varlist'_p*)
			gen `varlist' = .


			forvalues i = 1(1)`wc' {
				replace `varlist' = `i' if `varlist'_p`i' == _maxp & !missing(_maxp)
			}

			drop _maxp

		}

	}

end




program fitstats, rclass
	/*
	   Usage: fitstats [actual] [predicted] [if]

	   actual:		Actual outcomes (required)
	   predicted: 	Predicted outcomes (required)

	   Scalars:
	   r(kappa)		Cohen's kappa from -kap- command
	   r(prop_e)		Expected accuracy from -kap- command
	   r(prop_o)		Observed accuracy from -kap- command

	   Matrices:
	   r(c_matrix)		Confusion matrix
	   r(recall)		Recall/sensitivity vector
	   r(precision)	Precision/positive predicted value vector


	*/

	syntax varlist(min=2 max=2) [if]

	local actual: word 1 of `varlist'
	local predicted: word 2 of `varlist'

	preserve
	if "`if'" != "" {
		qui keep `if'	
	}

	if "$kapweight" == "within1" {
		qui gen _match = abs(`actual' - `predicted') <= 1 if !missing(`actual', `predicted')
	}
	else {
		qui gen _match = `actual' == `predicted' if !missing(`actual', `predicted')
	}
	

	qui levelsof `actual', local(vals_actual)
	qui levelsof `predicted', local(vals_predicted)
	local num_actual: word count `vals_actual'
	local num_predicted: word count `vals_predicted'

	// Confusion Matrix
	qui tab `actual' `predicted', matcell(CM)
	mat rownames CM = `vals_actual'
	mat colnames CM = `vals_predicted'
	return matrix c_matrix = CM, copy

	// Recall/Precision matrix
	mat RP = J(`num_actual', 2, 0)
	mat rownames RP = `vals_actual'
	mat colnames RP = "Recall" "Precision"
	// Recall matrix
	forvalues i = 1/`num_actual' {
		local val: word `i' of `vals_actual'
		qui sum _match if `actual' == `val'
		mat RP[`i', 1] = r(mean)
		qui sum _match if `predicted' == `val'
		mat RP[`i', 2] = r(mean)
	}
	mat R = RP[...., 1]
	mat P = RP[...., 2]
	return matrix recall = R, copy
	return matrix precision = P, copy

	// Kappa
	qui kap `actual' `predicted' //, wgt($kapweight)
	return scalar kappa = `r(kappa)'
	return scalar prop_e = `r(prop_e)'
	return scalar prop_o = `r(prop_o)'

	restore

	// Display results
	di as text _dup(60) "-"
	di as text  %-40s "  Prediction Summary" _newline
	di as text %12s "Actual: " as result %-32s "`actual'"
	di as text %12s "Predicted: " as result %-32s "`predicted'"
	di as text _newline _dup(60) "-"

	di as text "  Confusion matrix (rows: Actual, cols: Predicted)"
	mat li CM, noheader

	di as text _newline _dup(60) "-"
	di as text "  Recall, Precision:"
	mat li RP, noheader format(%4.3f)

	di as text _newline _dup(60) "-"
	di as text %10s "Kappa" _col(16) %10s "Expected" _col(32) %10s "Observed"
	di as text "{hline 42}"
	di as result %10.3f r(kappa) _col(16) %10s string(`r(prop_e)'*100, "%3.1f") + "%" _col(32) %10s string(`r(prop_o)'*100, "%3.1f") + "%"
	di as text _newline _dup(60) "-"

end



program xval, rclass
	syntax anything(name=cmd) [if], group(varname) [holdout] [*]
	
	capture drop _cat* _samp*
	
	di as text _newline _dup(60) "-"
	di as text "  Performing Cross-validation"
	di as text "  Model: " as result "`cmd' `if', `options'"

	qui levelsof `group', local(grpvals)
	local numvals: word count `grpvals'
	matrix R_out = J(6, `numvals', .)
	matrix P_out = J(6, `numvals', .)
	matrix k_out = J(1, `numvals', .)

	local hold = "`holdout'" == "holdout"
	// Get sample indicator for cmd
	local cmdvars = subinstr("`cmd'", word("`cmd'", 1), "", 1)
	qui reg `cmdvars' `if'
	qui gen _samp = e(sample)

	foreach val of local grpvals {

 		quietly {
			gen _samp_`group'_`val' = `hold' if _samp == 1
			replace _samp_`group'_`val' = 1 - `hold' if `group' == `val' & _samp == 1
			preserve

			keep if _samp_`group'_`val' == 1
			`cmd' `if', `options'
// 			est sto Model`val'

			local depvar = e(depvar)

			restore

			predictcat _cat_`group'_`val' `if'

			fitstats `depvar' _cat_`group'_`val' if _samp_`group'_`val' == 1
			mat r_in = r(recall)
			mat p_in = r(precision)
			mat c_matrix_in = r(c_matrix)
			local kap_in = r(kappa)
			local obs_in = r(prop_o)
			local exp_in = r(prop_e)

			fitstats `depvar' _cat_`group'_`val' if _samp_`group'_`val' == 0
			mat r_out = r(recall)
			mat p_out = r(precision)
			mat c_matrix_out = r(c_matrix)
			local kap_out = r(kappa)
			local obs_out = r(prop_o)
			local exp_out = r(prop_e)

			///////////////////
			count if _samp_`group'_`val' == 1
			local N_in = r(N)
			count if _samp_`group'_`val' == 0
			local N_out = r(N)
			if `hold' {
				local op "!="
				local notop "=="
			}
			else {
				local op "=="
				local notop "!="
			}
		}

		di as text _newline _dup(60) "-"

		di as text "  In: " as result "`group' `op' `val'" _skip(10) as text "N: " as result "`N_in'"
		di as text " Out: " as result "`group' `notop' `val'" _skip(10)  as text "N: " as result "`N_out'"
		mat Fit = r_in, r_out, p_in, p_out
		mat colnames Fit = "In: Recall" "Out: Recall" "In: Precision" "Out: Precision"
		mat li Fit, noheader format(%4.3f)



		di as text _newline _dup(60) "-"
		di as text %14s "Kappa" _col(16) %10s "Expected" _col(32) %10s "Observed"
		di as text "{hline 42}"
		di as text "In:" as result %10.3f `kap_in' _col(16) %10s string(`exp_in'*100, "%3.1f") + "%" _col(32) %10s string(`obs_in'*100, "%3.1f") + "%"
		di as text "Out:" as result %9.3f `kap_out' _col(16) %10s string(`exp_out'*100, "%3.1f") + "%" _col(32) %10s string(`obs_out'*100, "%3.1f") + "%"
		di as text _newline _dup(60) "-"

		return scalar N_in`val' = `N_in'
		return scalar kappa_in`val' = `kap_in'
		return scalar prop_o_in`val' = `obs_in'
		return scalar prop_e_in`val' = `exp_in'
		return matrix c_matrix_in = c_matrix_in
		return scalar N_out`val' = `N_out'
		return scalar kappa_out`val' = `kap_out'
		return scalar prop_o_out`val' = `obs_out'
		return scalar prop_e_out`val' = `exp_out'
		return matrix c_matrix_out = c_matrix_out
		return local group_in = "`group' `op' `val'"
		return local group_out = "`group' `notop' `val'"
		
		mat R = Fit[...., 1..2]
		return matrix recall`val' = R, copy
		mat P = Fit[...., 3..4]
		return matrix precision`val' = P, copy
		
		mat R_out[1, `val'] = R[...., 2]
		mat P_out[1, `val'] = P[...., 2]
		mat k_out[1, `val'] = `kap_out'

		
	}
	return local estcmd = "`cmd' `if', `options'"

	di _newline as text "Out-of-sample recall:"
	mat li R_out, noheader format(%4.3f)
	
	di _newline as text "Out-of-sample precision:"
	mat li P_out, noheader format(%4.3f)
	
	di _newline as text "Out-of-sample kappa:"
	mat li k_out, noheader format(%4.3f)
	
	return matrix R_out = R_out
	return matrix P_out = P_out
	return matrix k_out = k_out
	
	drop _cat* _samp*

end


////////////////////////////////////////////////////////////////////////////////

/* Descriptives */
tab $Outcome cohort, col chi2
tab $Outcome instrument, col chi2

reg $Outcome ib3.cohort, vce(clus instrument)
reg $Outcome ib7.instrument
reg $Outcome ib3.cohort ib7.instrument $Predictors

pca ensemble_reading ensemble_instrumentalskills ensemble_improvisation ensemble_rhythmicinterpretation audition*


/* Reg */

global PrBase "ib7.instrument audition_preparedpiece audition_reading audition_improv audition_melodic audition_rhythmic audition_score scholarship_rating"
global PrIndi "ib7.instrument ib2.audition_reading ib2.audition_melodic ib2.audition_rhythmic audition_preparedpiece audition_improv audition_score scholarship_rating"
global PrInte "ib7.instrument##c.audition* ib7.instrument##c.scholarship_rating"
global PrPoly "ib7.instrument audition_preparedpiece audition_reading audition_improv audition_melodic audition_rhythmic audition_score scholarship_rating p2* p3*"


global PredSets "PrBase PrIndi PrInte PrPoly"

capture drop _cat*
foreach predset in $PredSets {
	putexcel set "${OutDir}\TablesTest.xlsx", modify sheet("`predset'")
	
	local cn = "reg oprobit mlogit"
	local rn = "1 2 3 4 5 6"
	
	mat R`predset' = J(6, 3, .)
	mat colnames R`predset' = `cn'
	mat rownames R`predset' = `rn'
	
	mat P`predset' = J(6, 3, .)
	mat colnames P`predset' = `cn'
	mat rownames P`predset' = `rn'
	
	mat kap`predset' = J(1, 3, .)
	mat colnames kap`predset' = `cn'
	mat rownames kap`predset' = "kappa"
	
	di _newline as text _dup(80) "="
	
	local i = 1
	foreach cmd in "reg $Outcome $`predset'" "oprobit $Outcome $`predset'" "mlogit $Outcome $`predset', iter(50)" {
		local c: word 1 of `cmd'
		qui `cmd'
		qui predictcat _cat_`c'_`predset'
		fitstats $Outcome _cat_`c'_`predset'
		mat R`predset'[1, `i'] = r(recall)
		mat P`predset'[1, `i'] = r(precision)
		mat kap`predset'[1, `i'] = r(kappa)
		local i = `i' + 1
		di _newline
	}
	
	putexcel A1 = matrix(R`predset'), names
	putexcel A9 = matrix(P`predset'), names
	putexcel A17 = matrix(kap`predset'), names
}

forvalues i = 0/2 {
	local g = 0.05 + `i'*.2
	svmachines $Outcome inst* sv*, kernel(poly) degree(3) gamma(`g')
	predict _cat_sv`i'
	fitstats $Outcome _cat_sv`i'
}

// 5-fold xval
foreach predset in $PredSets {
	di _newline as text _dup(80) "="

	foreach cmd in "xval reg $Outcome $`predset', group(_xv1) holdout" "xval oprobit $Outcome $`predset', group(_xv1) holdout" "xval mlogit $Outcome $`predset', group(_xv1) holdout iter(50)" {
		local c: word 2 of `cmd'
		qui `cmd'
		di "`c': `predset'"

		mat li r(R_out)
		mat li r(P_out)
		mat li r(k_out)
		di as text _dup(80) "-" _newline
		putexcel set "${OutDir}\Tables1.xlsx", modify sheet("XV_`c'_`predset'")
		mat Recall = r(R_out)
		mat Precision = r(P_out)
		mat Kappa = r(k_out)
		putexcel A1 = matrix(Recall)
		putexcel A9 = matrix(Precision)
		putexcel A17 = matrix(Kappa)
	}
}

forvalues i = 0/1 {
	local g = 0.05 + `i'*.2
	xval svmachines $Outcome inst* sv*, group(_xv1) holdout kernel(poly) degree(3) gamma(`g')
	xval svmachines $Outcome inst* sv*, group(cohort) holdout kernel(poly) degree(3) gamma(`g')
}

// Cohort xval
foreach predset in $PredSets {
	di _newline as text _dup(80) "="

	foreach cmd in "xval reg $Outcome $`predset', group(cohort) holdout" "xval oprobit $Outcome $`predset', group(cohort) holdout" "xval mlogit $Outcome $`predset', group(cohort) holdout iter(50)" {
		local c: word 2 of `cmd'
		qui `cmd'
		di "`c': `predset'"

		mat li r(R_out)
		mat li r(P_out)
		mat li r(k_out)
		di as text _dup(80) "-" _newline
		putexcel set "${OutDir}\Tables.xlsx", modify sheet("XV_`c'_`predset'_coh")
		mat Recall = r(R_out)
		mat Precision = r(P_out)
		mat Kappa = r(k_out)
		putexcel A1 = matrix(Recall)
		putexcel A9 = matrix(Precision)
		putexcel A17 = matrix(Kappa)
	}
}
