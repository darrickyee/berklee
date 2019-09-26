clear all
global DataFile "..\data\Berklee Audition and Ensemble Data vF.dta"
global Predictors "audition_preparedpiece audition_reading audition_improv audition_melodic audition_rhythmic audition_score scholarship_rating"

use "$DataFile", clear

/* Custom commands */


program predictcat
	syntax [newvarname] [if]
	if "`varlist'" == "" {
		local varlist "category"
	}

	quietly {
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

end

program predkap, rclass
	syntax varlist(min=2 max=2) [if]
	local actual: word 1 of `varlist'
	local predicted: word 2 of `varlist'
	local if_cond = subinstr("`if'", "if ", "& ", 1)

	qui levelsof `actual' if !missing(`predicted'), local(vals_actual)

	qui proportion `actual' if !missing(`predicted') `if_cond'
	mat prs = r(table)[1, ....]
	mat dotprod= prs * prs'
	local exp = el(dotprod, 1, 1)

	qui count if !missing(`actual', `predicted') `if_cond'
	local tot = `r(N)'
	qui count if `actual' == `predicted' & !missing(`actual', `predicted') `if_cond'
	local match = `r(N)'
	local obs = `match' / `tot'

	local pkap = (`obs' - `exp') / (1 - `exp')

	di "Expected: " round(`exp'*100, .01) "%, Observed: " round(`obs'*100, .01) "%, Pseudo-kappa: " round(`pkap', .0001)
	
	return scalar kappa = `pkap'
	return scalar prop_e = `exp'
	return scalar prop_o = `obs'
end




program catsummary
	syntax varlist(min=2 max=2) [if]
	local actual: word 1 of `varlist'
	local predicted: word 2 of `varlist'

	local if_cond = subinstr("`if'", "if ", "& ", 1)

	di _newline "================ Confusion matrix: `actual' (actual) vs. `predicted' (predicted) ================"

	tab `actual' `predicted' `if'

	qui levelsof `actual', local(vals_actual)
	qui levelsof `predicted', local(vals_predicted)

	di _newline "Sensitivity:"
	foreach val of local vals_actual {
		qui count if `actual' == `val' & !missing(`actual', `predicted') `if_cond'
		local tot = `r(N)'
		qui count if `actual' == `val' & `predicted' == `actual' & !missing(`actual', `predicted') `if_cond'
		local match = `r(N)'
		di "`actual'==`val': " round(`match' / `tot'*100, .1)
	}

	di _newline "Positive predicted value:"
	foreach val of local vals_predicted {
		qui count if `predicted' == `val' & !missing(`actual', `predicted') `if_cond'
		local tot = `r(N)'
		qui count if `predicted' == `val' & `predicted' == `actual' & !missing(`actual', `predicted') `if_cond'
		local match = `r(N)'
		di "`predicted'==`val': " round(`match' / `tot'*100, .1)
	}


	predkap `actual' `predicted'

	/*
	   di _newline "Kappa (within 1):"
	   kapwgt win1 1 \ 1 1 \ 0 1 1 \ 0 0 1 1 \ 0 0 0 1 1 \ 0 0 0 0 1 1 \ 0 0 0 0 0 1 1
	   kap `actual' `predicted', wgt(win1)

	   di _newline "Kappa (within 1, 0.5 weight)"
	   kapwgt win1_2 1 \ .5 1 \ 0 .5 1 \ 0 0 .5 1 \ 0 0 0 .5 1 \ 0 0 0 0 .5 1 \ 0 0 0 0 0 .5 1
	   kap `actual' `predicted', wgt(win1_2)
	*/
end



/* Kappa matrices */
// I7
mat I_7 = I(7)

mat I1_7 = I_7
forvalues i = 1/6 {
	mat I1_7[`i', `i'+1] = 1
	mat I1_7[`i'+1, `i'] = 1
}

mat I2_7 = I_7
forvalues i = 1/6 {
	mat I2_7[`i', `i'+1] = .5
	mat I2_7[`i'+1, `i'] = .5
}




/* Data cleaning */
destring audition_preparedpiece, replace
encode instrument, gen(num_instrument)

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
encode enteringcohort, gen(year)


foreach var of varlist $Predictors {
	gen p2_`var' = (`var' - 3) ^ 2
	gen p3_`var' = (`var' - 3) ^ 3
}

////////////////////////////////////////////////////////////////////////////////

/* Reg */
reg ensemble_overall i.num_instrument $Predictors, vce(clus num_instrument)
predict olsp
gen cat_ols = round(olsp)
catsummary ensemble_overall cat_ols

oprobit ensemble_overall i.num_instrument

program define xval1yr
	forvalues i = 1/3 {
		quietly {
			preserve
			keep if year == `i'
			`0'
			restore
			predictcat _tmpcat
			predkap ensemble_overall _tmpcat if year == `i'
			local kap_in = `r(kappa)'
			predkap ensemble_overall _tmpcat if year != `i'

		}
		di "kappa: in-sample " `kap_in' ", out-of-sample " `r(kappa)' " | expected: " `r(prop_e)' " | observed: " `r(prop_o)'
		qui drop _tmpcat*
	}
end

program define xval2yr
	forvalues i = 1/3 {
		quietly {
			preserve
			keep if year != `i'
			`0'
			restore
			predictcat _tmpcat
			predkap ensemble_overall _tmpcat if year != `i'
			local kap_in = `r(kappa)'
			predkap ensemble_overall _tmpcat if year == `i'

		}
		di "============================= Excluded year: `i' ============================="
		di "kappa: in-sample " `kap_in' ", out-of-sample " `r(kappa)' " | expected: " `r(prop_e)' " | observed: " `r(prop_o)' _newline
		catsummary ensemble_overall _tmpcat if year == `i'
		qui drop _tmpcat*
	}
end

hist ensemble_overall , by(year, cols(1)) disc ysize(5) xsize(3) freq

// 1. Argue for OP & HOP?
// 2. Explain 

/*
/* oprobit, linear */
   oprobit ensemble_overall $Predictors
   predictcat cat_op1
   catsummary ensemble_overall cat_op1

/* oprobit, linear w/instruments */
   oprobit ensemble_overall $Predictors i.num_instrument
   predictcat cat_op2
   catsummary ensemble_overall cat_op2

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
   catsummary ensemble_overall cat_op3


/* oprobit, linear w/instrument interactions */
   oprobit ensemble_overall ib7.num_instrument##c.audition* ib7.num_instrument##c.scholarship_rating
   predictcat cat_op4
   catsummary ensemble_overall cat_op4


/* hetoprobit, linear w/instrument interactions */
   hetoprobit ensemble_overall ib7.num_instrument##c.audition* ib7.num_instrument##c.scholarship_rating, het(ib7.num_instrument)
   predictcat cat_op5
   catsummary ensemble_overall cat_op5

   // Cross-validation
   // By year
   hetoprobit ensemble_overall ib7.num_instrument##c.audition* ib7.num_instrument##c.scholarship_rating if year == 2, het(ib7.num_instrument)
   predictcat catyr2 if year == 3
/*
/* 
   Questions
   Advanced > Strong?
   ensemble_overall 1-7 prior to 2018; 1-6 in 2018
*/




   hetoprobit ensemble_overall ib7.num_instrument##c.audition* ib7.num_instrument##c.scholarship_rating ib7.num_instrument##c.p2*, het(ib7.num_instrument)
