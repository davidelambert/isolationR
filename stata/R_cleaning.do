* greyscale for graphs
    set scheme s1mono
* wide output
    set linesize 120


**************
** OUTCOMES **
**************

****************************************************************
* standardized test scores
    * recode missing values from standardized test scores in base year, followup 1, and followup 2
        recode by2x*std (99.98/99.99 = .)
        recode f12x*std (99.98/99.99 = .)
        recode f22x*std (99.98/99.99 = .)
	

    * generate unweighted averages for each year
        gen by_test_avg = (by2xrstd + by2xmstd + by2xsstd + by2xhstd) / 4
            label var by_test_avg "8th test avg"
        gen f1_test_avg = (f12xrstd + f12xmstd + f12xsstd + f12xhstd) / 4
            label var f1_test_avg "10th test avg"
        gen f2_test_avg = (f22xrstd + f22xmstd + f22xsstd + f22xhstd) / 4
            label var f2_test_avg "12th test avg"
			
	* code scores for CENTERING at mean = 0, SD = 1
	foreach var of varlist *2x*std {
		gen `var'_centered = (`var' - 50) / 10
	}			

	* generate CENTERED unweighted averages for each year
        gen by_test_centered = (by2xrstd_centered + by2xmstd_centered + by2xsstd_centered + by2xhstd_centered) / 4
            label var by_test_centered "8th test avg"
            notes by_test_centered: recenented by_test_avg to ~N(0,1)
        gen f1_test_centered = (f12xrstd_centered + f12xmstd_centered + f12xsstd_centered + f12xhstd_centered) / 4
            label var f1_test_centered "10th test avg"
            notes f1_test_centered: recenented f1_test_avg to ~N(0,1)
        gen f2_test_centered = (f22xrstd_centered + f22xmstd_centered + f22xsstd_centered + f22xhstd_centered) / 4
            label var f2_test_centered "12th test avg"
            notes f2_test_centered: recenented f2_test_avg to ~N(0,1)

    * check to make sure CENTERED is the same: (commenting out unless needed)
        * hist f1_test_avg, bin(50) name(avg, replace) 
        * hist f1_test_centered, bin(50) name(centered, replace) 

***************************************************************

* dropout status in followup 2 (12th grade)
    recode f2univ2d (3 = 1) (1/2 = 0) (4/8 = .), gen(dropout)
    label var dropout "dropped out by followup 2"




****************
** ISOLATION  **
** 10th GRADE **
****************


* variables on how student thinks peers see them. All coded
    * recode missings & apply labels for all
        recode f1s67*  (6 8 = .)
            label define threept 1 "very" 2 "somewhat" 3 "not at all"
            label values f1s67* threept

    * examine correlations
        pwcorr f1s67*, star(.05)
    
    * macro for the well-correlated & logically similar peer variables: popular, socially active, important, part of leading crowd
        global social f1s67a f1s67c f1s67e f1s67g

    * compare Cronbach's alpha for all peer var's & just the isolation ones
        alpha f1s67*
        alpha $social

    * create factor of the 4 peer social variables
    * result is standardized w/ positive SD's indicating increased isolation
        factor $social, pcf
        rotate
        predict isolation
            label var isolation "isolation"
            notes isolation: ~N(0,1), positive = more isolated

    

*******************************
** ALTERNATIVE PREDICTORS:   **
** DEPRESSION & SELF CONCEPT **
*******************************

* recode missings from the internal self-concept variables
* note: these are standardized ~N(0,1) composites of: f1s62a, -d, -e, -h, -i, -j, -l, 
* with -a, -d, -e, -h reversed. Positive is "better" self-image
    recode f1cncpt* (99.98 = .)
        label var f1cncpt2 "self-concept"


* create depressive symptoms pricipal component factors

    * drop missing
        recode f1s62* (6 8 = .)

    * reverse "positive" coded self-concept/locus of control variables.
    * now all scale 1 = best -> 4 = worst.
        recode f1s62b f1s62c f1s62f f1s62g f1s62i f1s62j f1s62l f1s62m f1s62n (1 = 4) (2 = 3) (3 = 2) (4 = 1)

    * create macros for factor analysis:
        * first contains same variables as the included self-concept variable above:
        *   f162a: feels good about self, -d: feels a person of worth, -e: able to do things as well as others,
        *   -h: satisfied for self, -i: feels useless, -j: thinks no good at all, -l: does not have much to be proud of
            global self1 f1s62a f1s62d f1s62e f1s62h f1s62i f1s62j f1s62l
        * add in f1s62n: feels emotionally empty most of the time
            global self2 f1s62a f1s62d f1s62e f1s62h f1s62i f1s62j f1s62l f1s62n

    * compare alpha coefficients for each specification to rationalize including uselessness:
        alpha $self1
        alpha $self2

    * conduct factor analysis with principal componet specification:
    * yields 2 factors, differentially loaded on positive & negative wording
        factor $self2, pcf
        rotate
            * factor 1 loads on a,d,e,h with positive wording
        predict depression
            label var depression "depression"
            notes depression: ~N(0,1), positive = more depressed





***********************
** MAIN CONFOUNDERS **
***********************

* parents' highest level of education
    recode f1pared (7 98 = .)
        label define pared 1 "nohs" 2 "hs/ged" 3 "some coll" 4 "bachelor" 5 "master" 6 "phd/md/etc"
        label values f1pared pared
        label var f1pared "parents' highest ed"

* dummy on male sex
    recode sex (2 = 0), gen(male)
        label var male "male"

* family income, base year
    recode byfaminc (98 = .), g(faminc)
        label define income 1 "none" 2 "< 1,000" 3 "1,000 - 2,999" 4 "3,000 - 4,999" ///
              5 "5,000 - 7,499" 6 "7,500 - 9,999" 7 "10,000 - 14,999" 8 "15,000 - 19,999" ///
              9 "20,000 - 24,999" 10 "25,000 - 34,999" 11 "35,000 - 49,000" ///
              12 "50,000 - 74,999" 13 "75,000 - 99,999" 14 "100,000 - 199,999" 15 "> 200,000"
        label values faminc income
        label var faminc "family income"

        * reduced price lunch threshold is 185% of federal poverty rate
        * rate for family of 4, 1988: $11,650
        * 1.85 * $11,650 = $21,553
        * coding lowinc as < $25,000, which is 214% fpr
                gen lowinc = (faminc < 10)
                label var lowinc "income <$25K"

* socio-economic status
    recode f1ses (99.998 = .)
        label var f1ses "composite SES"

* race & minority status
    recode race (8 = .)
        label define races 1 "asian" 2 "hispanic" 3 "black" 4 "white" 5 "native"
        label values race races
        label var race "single category race"
    gen nonwhite = (race != 4 & race != .)
        label define nonwhite 0 "white" 1 "nonwhite"
        label values nonwhite nonwhite
        label var nonwhite "nonwhite"
    recode race (2 = 1 "hispanic") (1 3/5 = 0 "non-hispanic"), gen(hispanic) label(hispanic)
        label var hispanic "hispanic"


* hours spent on extracurriculars
    recode f1s42 (0=0 "none") (1=1 "<1") (2=2 "1-4") (3=3 "5-9") (4=4 "10-19") (5=5 ">20") (96/98 = .), gen(extracurr) label(extracurr)
        label var extracurr "hours spent on extracurriculars"

* number of times skipped class
    recode f1s10b (0=0 "none") (1=1 "1-2") (2=2 "3-6") (3=3 "7-9") (4=4 "10+") (8=.), gen(skip) label(skip)
        label var skip "times skipped class 1/2 yr"

***********************************
** OTHER STUDENT CHARACTERISTICS **
***********************************

* student's language minority status
    recode bylm (0 = 0 "not language minority") (1 = 1 "language minority") (8 = .), gen(stu_langmin) label(langmin)
        label var stu_langmin "language minority student"   


* first language spanish speaker
    recode f1n12 (1 3/96 99 = 0 "non-spanish") (2 = 1 "spanish") (98 = .), gen(spanish) label(spanish)
        label var spanish "1st lang spanish"

* ANY minority first language speaker (English as a Second or Other Language)
    recode f1n12 (1 99 = 0 "native speaker") (2/96 = 1 "NON-native speaker") (98 = .), gen(esol) label(esol)
        label var esol "non-native speaker"

* immigration as measured by foreign birth
    recode byp17 (1/2 = 0 "native born") (3 = 1 "foreign born") (else = .), gen(immigrant) label(immigrant)
        label var immigrant "immigrant"

* EVER attended school outside US
    recode byp19 (1 = 1 "any") (2 = 0 "none") (6 8 = .), gen(foreign_sch) label(foreign_sch)
        label var foreign_sch "non-US school"


****************************
** SCHOOL LEVEL VARIABLES **
****************************

* school percentage white (maybe useful for to instrument for social isolation)
    recode f1c27f (997/998 = .), gen(sch_white)
        label define sch_white 1 "0-25" 2 "26-50" 3 "50-75" 4 "75-90" 5 "90-100"
        label values sch_white sch_white

* school is majority white
    recode sch_white (3/5 = 1 "majwht") (1/2 = 0 "nonmajwht"), gen(majwht) label(majwht)
        label var majwht "school >50% white"

* school language minority percentage
    recode f1c29 (98 = .), gen(sch_langmin)
        label define sch_langmin 0 "none" 1 "<10%" 2 "10%-19%" 3 "20%-29%" 4 "30%-39%" 5 ">=40%"
        label values sch_langmin sch_langmin
        label var sch_langmin "% language minority or lep"

* school is 10% or less language minority/LEP
    recode sch_langmin (0 = .) (1 = 1 "<10%") (2/5 = 0 ">10%"), gen(langmin10) label(langmin10)
        label var langmin10 "<10% lan.e min./lep"

* interaction b/w individual language minority status & school being less that 10% language minority students
    gen int_langmin = stu_langmin * langmin10
        label var int_langmin "lang. min. student * <10% lang. min. school"

* interaction between hispanic self-identity & school non-hispanic
    gen int_hispwht = hispanic * majwht

* school esl teachers
    * 0 esl teachers
        recode f1c45 (0 = 1) (1/40 = 0) (998 = .), gen(esl_none)
        label var esl_none "no esl teachers"
    * exactly 1 esl teacher
        recode f1c45 (1 = 1) (2/40 = 0) (998 = .), gen(esl_one)
        label var esl_one "one esl teacher"
    * more than 1 esl teacher
        recode f1c45 (0/1 = 0) (2/40 = 1) (998 = .), gen(esl_multi)
        label var esl_multi "more than 1 esl teacher"

* school dropout rate
    recode f1c32 (998 = .), gen(sch_dropout)
        label var sch_dropout "est. % dropout"

*school free/reduced lunch percentage
    recode f1c30a (998 = .) (0=0 "0%") (1=1 "1-10%") (2=2 "11-50%") (3=3 "50-100%"), gen(sch_lunch) label(sch_lunch)
        label var sch_lunch "% free/reduced lunch"

* school urbanicity
    recode g10urban (1 = 1 "urban") (2 = 2 "suburban") (3 = 3 "rural") (8 = .), gen(urbanicity) label(urbanicity)
        label var urbanicity "urbanicity"
* school census region
    recode g10regon (1=1 "northeast") (2=2 "midwest") (3=3 "south") (4=4 "west") (98=.), gen(region) label(region)
        label var region "region"




