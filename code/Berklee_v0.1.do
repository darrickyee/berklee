clear all
global DataFile "D:\Projects\Berklee\data\Berklee Audition and Ensemble Data vF.dta"
global Predictors "audition_preparedpiece audition_reading audition_improv audition_melodic audition_rhythmic audition_score scholarship_rating"

use "$DataFile", clear

/* Custom commands */

program define predictcat
	if ("`1'"=="") {
		local 1 "category"
	}

	quietly {
		predict `1'_p*
		
		desc `1'_p*, varlist
		local wc: word count `r(varlist)'
		
		egen _maxp = rowmax(`1'_p*)
		gen `1' = .

		forvalues i = 1(1)`wc' {
			replace `1' = `i' if `1'_p`i' == _maxp & !missing(_maxp)
		}
		
		drop _maxp
	}
	
end

program define catmatch, rclass
	quietly {
		local distance = 0
		if ("`3'" != "") {
			local distance = `3'
		}
		
		gen _nonmiss = !missing(`1', `2')
		
		count if _nonmiss
		local tot_n = `r(N)'
		count if _nonmiss & abs(`1' - `2') <= `distance'
		local match_n = `r(N)'
		local prop = `match_n' / `tot_n'

		return scalar proportion = `prop'
		return scalar match = `match_n'
		return scalar total = `tot_n'
		
		
		
		drop _nonmiss
	}
	
	di _newline "=========== Confusion matrix: `1' vs. `2' ==========="
	tab `1' `2', mi
	
	di _newline "=========== Matched values: `1' vs. `2' ===========" _newline
	di "    Total: `tot_n'"
	di "    Matched (within `distance'): `match_n'"
	di "    Proportion: `prop'"
	di _newline "[Results saved in r()]"
	
	
end

/* Data cleaning */
destring audition_preparedpiece, replace

label define audition_lbl 0 "None" 1 "Beginner" 2 "Intermediate" 3 "Strong" 4 "Advanced"

foreach var of varlist audition_reading audition_melodiccr audition_rhythmiccr {
	encode `var', gen(_num_`var') label(audition_lbl)
	replace _num_`var' = 1 if `var' == "Low-Intermediate"
	tab `var' _num_`var', mi
	rename `var' str_`var'
	rename _num_`var' `var'
}

foreach var of varlist audition_melodiccr audition_rhythmiccr {
	local newvar = subinstr("`var'", "cr", "", .)
	rename `var' `newvar'
}

rename audition_date x_audition_date

/* oprobit, linear */
oprobit ensemble_overall $Predictors
predictcat cat_op1
catmatch ensemble_overall cat_op1

/* oprobit, linear w/instruments */
encode instrument, gen(num_instrument)
oprobit ensemble_overall $Predictors i.num_instrument
predictcat cat_op2
catmatch ensemble_overall cat_op2

/* oprobit, free w/instruments */
foreach var of varlist $Predictors {
	local basevar = subinstr("`var'", "audition_", "", 1)
	clonevar _`basevar' = `var'
	xi i._`basevar'
	renpfix _I I
	drop _`basevar'
}
oprobit ensemble_overall I_* i.num_instrument
predictcat cat_op3
catmatch ensemble_overall cat_op3

/* oprobit, linear w/instrument interactions */
oprobit ensemble_overall ib7.num_instrument##c.audition* ib7.num_instrument##c.scholarship_rating
predictcat cat_op4
catmatch ensemble_overall cat_op4

// Cross-validation


/* 
Questions
Advanced > Strong?
ensemble_overall 1-7 prior to 2018; 1-6 in 2018
*/