** differences in test score average, by isolation

    * binary isolation indicator, 1 = isolation > 1
        gen iso_binary = (isolation > 0)

    * table
        table iso_binary, c(mean f1_test_centered)

     * chart
        graph bar f1_test_centered, over(iso_binary, relabel(1 "Isolation <= 0" 2 "Isolation > 1")) name(isotest, replace) /// 
        ytitle("10th Grade Test Average")



** differences in isolation score, by dropouts & non-dropouts
    
    * table
        table dropout, c(mean isolation)

    * chart
        graph bar isolation, over(dropout, relabel(1 "Non-Dropout" 2 "Dropout")) name(isodropout, replace) ///
        ytitle("Isolation")


** construction of isolation

    * correlation matrix
        corr f1s67*, wrap

    * compare alphas
        alpha f1s67*
        alpha $social

    * example table
        tab f1s67a

** IV strategy
    table hispanic, c(mean isolation mean f1_test_centered mean dropout)
    table spanish, c(mean isolation mean f1_test_centered mean dropout)
    table hispanic majwht, c(mean isolation)
    table spanish majwht, c(mean isolation)



** TABLES

* restrict sample
    foreach var of varlist isolation depression f1cncpt2 dropout f1_test_centered by_test_centered f1ses male ///
        race hispanic majwht spanish sch_lunch sch_dropout region urbanicity {
    drop if `var' == .
    }

* cleanup
    eststo drop *
    global drop *char

* macros for models
    global testchars by_test_centered f1ses male ib4.race
    global dropchars f1_test_centered f1ses male ib4.race








** MODELS

* BASIC
    eststo tb: reg f1_test_centered isolation
        estadd local Controls "N"
        estadd local FE "N"
    eststo db: reg dropout isolation
        estadd local Controls "N"
        estadd local FE "N"
    esttab tb db, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) obslast ///
        mtitles("Test" "Dropout") title("Simple OLS") coeflabels(isolation Isolation)

* CONTROLS
    eststo tc: reg f1_test_centered isolation $testchars
        estadd local Controls "Y"
        estadd local FE "N"
    eststo dc: reg dropout isolation $dropchars
        estadd local Controls "Y"
        estadd local FE "N"
    esttab tc dc, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
    scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("Multiple OLS") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" ///
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

* FIXED EFFECTS
    eststo tf: xtreg f1_test_centered isolation $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo df: xtreg dropout isolation $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    esttab tf df, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
    scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("School Fixed Effects") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" /// 
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

* ALL OF THE ABOVE
   esttab tb tc tf db dc df, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase nogaps scalars(Controls FE) ///
       obslast title("Primary Models") nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
       order(isolation by_test_centered f1_test_centered f1ses male 1.race 2.race 3.race 5.race) ///
       coeflabels(isolation Isolation by_test_centered "8th Avg." f1_test_centered "10th Avg." f1ses "SES" male "Male" ///
                  1.race "Asian" 2.race "Hispanic" 3.race "Black" 5.race "Nat. Am.") ///
       nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" "Controls: prior test average, composite SES, sex, and race.")

        

* ALTERNATE PREDICTORS
    eststo tfself: xtreg f1_test_centered f1cncpt2 $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo tfdep: xtreg f1_test_centered depression $testchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo dfself: xtreg dropout f1cncpt2 $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    eststo dfdep: xtreg dropout depression $dropchars, fe i(f1sch_id)
        estadd local Controls "Y"
        estadd local FE "Y"
    esttab tf tfself tfdep df dfself dfdep,  keep(isolation f1cncpt2 depression) ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
        coeflabels(isolation Isolation f1cncpt2 "Self Concept" depression Depression) ///
        obslast title("Alternate Predictors w/ School Fixed Effects")  varwidth(15) ///
        nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
        nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" )


* ALL MODELS TO CONSOLE

    esttab tb db, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) obslast ///
        mtitles("Test" "Dropout") title("Simple OLS") coeflabels(isolation Isolation)

    esttab tc dc, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
    scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("Multiple OLS") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" ///
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

    esttab tf df, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
    scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("School Fixed Effects") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" /// 
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

   esttab tb tc tf db dc df, b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
       obslast title("Primary Models") nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
       order(isolation by_test_centered f1_test_centered f1ses male 1.race 2.race 3.race 5.race) ///
       coeflabels(isolation Isolation by_test_centered "8th Avg." f1_test_centered "10th Avg." f1ses "SES" male "Male" ///
                  1.race "Asian" 2.race "Hispanic" 3.race "Black" 5.race "Nat. Am.") ///
       nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" "Controls: prior test average, composite SES, sex, and race.")

    esttab tf tfself tfdep df dfself dfdep,  keep(isolation f1cncpt2 depression) ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
        coeflabels(isolation Isolation f1cncpt2 "Self Concept" depression Depression) ///
        obslast title("Alternate Predictors with School Fixed Effects")  varwidth(15) ///
        nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
        nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" )


* ALL TO HTML 

    esttab tb db using pbasic.html, replace ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) obslast ///
        mtitles("Test" "Dropout") title("Simple OLS") coeflabels(isolation Isolation)

    esttab tc dc using pcontrol.html, replace ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
        scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("Multiple OLS") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" ///
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

    esttab tf df using pfixed.html, replace ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps keep(isolation) /// 
        scalars(Controls FE) obslast mtitles("Test Avg." "Dropout") title("School Fixed Effects") coeflabels(isolation Isolation) nonotes ///
        addnotes("Standard errors in parentheses." ///
                 "* p<.1, **p<.05, *** p<.01" /// 
                 "Controls: prior test average," ///
                 "composite SES, sex, and race.")

   esttab tb tc tf db dc df using pall.html, replace ///
       b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
       obslast title("Primary Models") nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
       order(isolation by_test_centered f1_test_centered f1ses male 1.race 2.race 3.race 5.race) ///
       coeflabels(isolation Isolation by_test_centered "8th Avg." f1_test_centered "10th Avg." f1ses "SES" male "Male" ///
                  1.race "Asian" 2.race "Hispanic" 3.race "Black" 5.race "Nat. Am.") ///
       nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" "Controls: prior test average, composite SES, sex, and race.")

    esttab tf tfself tfdep df dfself dfdep using palt.html, replace ///
        keep(isolation f1cncpt2 depression) ///
        b(3) se(3) star(* .1 ** .05 *** .01) nocons nobase compress nogaps scalars(Controls FE) ///
        coeflabels(isolation Isolation f1cncpt2 "Self Concept" depression Depression) ///
        obslast title("Alternate Predictors with School Fixed Effects")  varwidth(15) ///
        nomtitles mgroups("Test Avg." "Dropout", pattern(1 0 0 1 0 0 )) ///
        nonotes addnotes("Standard errors in parentheses. * p<.1, **p<.05, *** p<.01" )
