* cleanup
    quietly: eststo drop *
    quietly: global drop *char

* macros for models
    quietly: global testchars by_test_avg f1ses male ib4.race
    quietly: global dropchars f1_test_avg f1ses male ib4.race








** MODELS

* BASIC
	quietly {
    eststo tb: reg f1_test_avg isolation
        estadd local Controls "N"
        estadd local FE "N"
    eststo db: reg dropout isolation
        estadd local Controls "N"
        estadd local FE "N"
		}

* CONTROLS
	quietly {
    eststo tc: reg f1_test_avg isolation $testchars
        estadd local Controls "Y"
        estadd local FE "N"
    eststo dc: reg dropout isolation $dropchars
        estadd local Controls "Y"
        estadd local FE "N"
		}

* FIXED EFFECTS
	quietly {
    eststo tf: xtreg f1_test_avg isolation $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo df: xtreg dropout isolation $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
		}


        

* ALTERNATE PREDICTORS
	quietly {
    eststo tfself: xtreg f1_test_avg f1cncpt2 $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo tfdep: xtreg f1_test_avg depression $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo dfself: xtreg dropout f1cncpt2 $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo dfdep: xtreg dropout depression $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
		}

* PRINT TABLES TO CONSOLE

* Main Models
   esttab tb tc tf db dc df, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
       obslast title("Primary Models") nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
       order(isolation by_test_avg f1_test_avg f1ses male 1.race 2.race 3.race 5.race) ///
       coeflabels(isolation Isolation by_test_avg "8th Avg." f1_test_avg "10th Avg." f1ses "SES" male "Male" ///
                  1.race "Asian" 2.race "Hispanic" 3.race "Black" 5.race "Nat. Am.") ///
       nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" "Controls: prior test average, composite SES, sex, and race.")

* Alternate predictors
    esttab tf tfself tfdep df dfself dfdep,  keep(isolation f1cncpt2 depression) ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
        coeflabels(isolation Isolation f1cncpt2 "Self Concept" depression Depression) ///
        obslast title("Alternate Predictors with School Fixed Effects")  varwidth(15) ///
        nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
        nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" )

