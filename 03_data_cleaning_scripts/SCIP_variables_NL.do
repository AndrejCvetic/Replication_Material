*** VARIABLES FOR THE REPLICATION ***

*** Original code by Andrej Cvetic
*** cvetica@tcd.ie

*** 11/10/2023


*** READ IN THE ORIGINAL SCIP DATASET (UK data, wave 1)

**cd "C:\Users\Andrej Cvetic\Desktop\PhD\2023_TT_Replication Paper\1_Datasets for Replication\01_Working Datasets"
**use "ZA5956_nl_wave1_v1-0-0", clear

** Follow the instructions from README file to access the data from GESIS. 
** Download the data to the folder of your choce. 

numlabel, add force

*** NL Dataset ***

********************************************************************************
************************* EXPLANATORY VARIABLES ********************************
********************************************************************************

** Politilcal Discrimination

** RACIAL WITHOUT FREQUENCY 
** principle for creation: disc var = 1 if discrimination is yes and is racial or national 

tab DSCROFFREQ 
tab DSCROFWHY

g race_insti_disc_1 = 0
replace race_insti_disc_1 = 1 if DSCROFFREQ == 1 & (DSCROFWHY == 3 | DSCROFWHY == 6)
tab race_insti_disc_1

tab DSCRLMKFREQ
tab DSCRLMKWHY

g race_labor_disc_1 = 0
replace race_labor_disc_1 = 1 if DSCRLMKFREQ == 1 & (DSCRLMKWHY == 3 | DSCRLMKWHY == 6)
tab race_labor_disc_1

g race_pol_disc_1 = race_insti_disc_1 + race_labor_disc_1
tab race_pol_disc_1

** RACIAL WITH FREQUENCY 
** principle for creation: var == n if race..._1 == 1 & freq == n  
** transform freq in three point scale

tab DSCRFREQ
g disc_freq = DSCRFREQ
recode disc_freq -97=. -98=. 5=0 4=1 3=2 2=3 1=3 
tab disc_freq

g race_insti_disc_2 = 0
replace race_insti_disc_2 = 0 if race_insti_disc_1 == 0 | disc_freq ==0
replace race_insti_disc_2 = 1 if race_insti_disc_1 == 1 & disc_freq ==1
replace race_insti_disc_2 = 2 if race_insti_disc_1 == 1 & disc_freq ==2
replace race_insti_disc_2 = 3 if race_insti_disc_1 == 1 & disc_freq ==3
tab race_insti_disc_2

g race_labor_disc_2 = 0
replace race_labor_disc_2 = 0 if race_labor_disc_1 == 0 | disc_freq ==0
replace race_labor_disc_2 = 1 if race_labor_disc_1 == 1 & disc_freq ==1
replace race_labor_disc_2 = 2 if race_labor_disc_1 == 1 & disc_freq ==2
replace race_labor_disc_2 = 3 if race_labor_disc_1 == 1 & disc_freq ==3
tab race_labor_disc_2

g race_pol_disc_2 = race_insti_disc_2 + race_labor_disc_2
tab race_pol_disc_2

** NO RACIAL AND NO FREQUENCY
** principle: keeps disc. as inst/labour and includes all forms, not just racial 

tab DSCROFFREQ 

g insti_disc_1 = 0
replace insti_disc_1 = 1 if DSCROFFREQ == 1
tab insti_disc_1

g labor_disc_1 = 0
replace labor_disc_1 = 1 if DSCRLMKFREQ == 1 
tab labor_disc_1

g pol_disc_1 = insti_disc_1 + labor_disc_1
tab pol_disc_1

** NO RACIAL WITH FREQUENCY
** principle: keeps disc. as inst/labour and includes all forms and accounts
** for frequency 

g insti_disc_2 = 0
replace insti_disc_2 = 0 if insti_disc_1 == 0 | disc_freq ==0
replace insti_disc_2 = 1 if insti_disc_1 == 1 & disc_freq ==1
replace insti_disc_2 = 2 if insti_disc_1 == 1 & disc_freq ==2
replace insti_disc_2 = 3 if insti_disc_1 == 1 & disc_freq ==3
tab insti_disc_2

g labor_disc_2 = 0
replace labor_disc_2 = 0 if labor_disc_1 == 0 | disc_freq ==0
replace labor_disc_2 = 1 if labor_disc_1 == 1 & disc_freq ==1
replace labor_disc_2 = 2 if labor_disc_1 == 1 & disc_freq ==2
replace labor_disc_2 = 3 if labor_disc_1 == 1 & disc_freq ==3
tab labor_disc_2

g pol_disc_2 = insti_disc_2 + labor_disc_2
tab pol_disc_2


** Societal Discrimination

** RACIAL WITHOUT FREQUENCY 
** principle for creation: disc var = 1 if discrimination is yes and is racial or national 

tab DSCRHMKFREQ 
tab DSCRHMKWHY

g race_house_disc_1 = 0
replace race_house_disc_1 = 1 if DSCRHMKFREQ == 1 & (DSCRHMKWHY == 3 | DSCRHMKWHY == 6)
tab race_house_disc_1

tab HARFREQ
tab HARWHY

g race_harras_disc_1 = 0
replace race_harras_disc_1 = 1 if HARFREQ == 1 & (HARWHY == 3 | HARWHY == 6)
tab race_harras_disc_1

g race_soc_disc_1 = race_house_disc_1 + race_harras_disc_1
tab race_soc_disc_1

** RACIAL WITH FREQUENCY 
** principle for creation: var == n if race..._1 == 1 & freq == n  
** transform freq in three point scale

g race_house_disc_2 = 0
replace race_house_disc_2 = 0 if race_house_disc_1 == 0 | disc_freq ==0
replace race_house_disc_2 = 1 if race_house_disc_1 == 1 & disc_freq ==1
replace race_house_disc_2 = 2 if race_house_disc_1 == 1 & disc_freq ==2
replace race_house_disc_2 = 3 if race_house_disc_1 == 1 & disc_freq ==3
tab race_house_disc_2

g race_harras_disc_2 = 0
replace race_harras_disc_2 = 0 if race_harras_disc_1 == 0 | disc_freq ==0
replace race_harras_disc_2 = 1 if race_harras_disc_1 == 1 & disc_freq ==1
replace race_harras_disc_2 = 2 if race_harras_disc_1 == 1 & disc_freq ==2
replace race_harras_disc_2 = 3 if race_harras_disc_1 == 1 & disc_freq ==3
tab race_harras_disc_2

g race_soc_disc_2 = race_house_disc_2 + race_harras_disc_2
tab race_soc_disc_2

** NO RACIAL AND NO FREQUENCY
** principle: keeps disc. as inst/labour and includes all forms, not just racial 

g house_disc_1 = 0
replace house_disc_1 = 1 if DSCRHMKFREQ == 1
tab house_disc_1


g harras_disc_1 = 0
replace harras_disc_1 = 1 if HARFREQ == 1
tab harras_disc_1

g soc_disc_1 = house_disc_1 + harras_disc_1
tab soc_disc_1

** NO RACIAL WITH FREQUENCY
** principle: keeps disc. as inst/labour and includes all forms and accounts
** for frequency 

g house_disc_2 = 0
replace house_disc_2 = 0 if house_disc_1 == 0 | disc_freq ==0
replace house_disc_2 = 1 if house_disc_1 == 1 & disc_freq ==1
replace house_disc_2 = 2 if house_disc_1 == 1 & disc_freq ==2
replace house_disc_2 = 3 if house_disc_1 == 1 & disc_freq ==3
tab house_disc_2

g harras_disc_2 = 0
replace harras_disc_2 = 0 if harras_disc_1 == 0 | disc_freq ==0
replace harras_disc_2 = 1 if harras_disc_1 == 1 & disc_freq ==1
replace harras_disc_2 = 2 if harras_disc_1 == 1 & disc_freq ==2
replace harras_disc_2 = 3 if harras_disc_1 == 1 & disc_freq ==3
tab harras_disc_2

g soc_disc_2 = house_disc_2 + harras_disc_2
tab soc_disc_2


tab race_pol_disc_1
tab race_pol_disc_2
tab pol_disc_1
tab pol_disc_2

tab race_soc_disc_1
tab race_soc_disc_2
tab soc_disc_1
tab soc_disc_2

* BROAD DISCRIMINATION (both broad disc I amd II need to be racial) 

g broad_discrimination_i = 0
replace broad_discrimination_i = 1 if race_insti_disc_1 == 1| race_labor_disc_1 == 1 | race_house_disc_1 == 1 | race_harras_disc_1 == 1   
tab broad_discrimination_i

g broad_discrimination_ii = 0 
replace broad_discrimination_ii = 0 if broad_discrimination_i == 0 | disc_freq == 0
replace broad_discrimination_ii = 1 if broad_discrimination_i == 1 | disc_freq == 1
replace broad_discrimination_ii = 2 if broad_discrimination_i == 1 | disc_freq == 2
replace broad_discrimination_ii = 3 if broad_discrimination_i == 1 | disc_freq == 3
tab broad_discrimination_ii

********************************************************************************
***************************** RESPONSE VARIABLES *******************************
********************************************************************************

** Ethnic based engagement 

** principle if respondent reported participation and if half or more people
** involved are from CO. 

tab MSHIPSPO
tab SPOPPCO

**

g ebe_sport = 0
replace ebe_sport = 1 if MSHIPSPO == 1 & (SPOPPCO <= 3 & SPOPPCO >= 1)
tab ebe_sport

tab MSHIPPOL
tab POLPPCO

g ebe_pol = 0
replace ebe_pol = 1 if MSHIPPOL == 1 & (POLPPCO <= 3 & POLPPCO >= 1)
tab ebe_pol

tab MSHIPGRP
tab GRPPPCO

g ebe_oth = 0
replace ebe_oth = 1 if MSHIPGRP == 1 & (GRPPPCO <= 3 & GRPPPCO >= 1)
tab ebe_oth

tab RELGRP
tab RELGRPPPCO

g ebe_rel = 0
replace ebe_rel = 1 if RELGRP == 1 & (RELGRPPPCO <= 3 & RELGRPPPCO >=1)
tab ebe_rel

g ebe = ebe_sport + ebe_pol + ebe_oth + ebe_rel
recode ebe 2=1 3=1 4=1
tab ebe 

** In group attachment - view of value correspeondence is proxy for identifying 
** with members of host nation 

tab VAL

** principle: recode to have var of three and five cat; smaller numbers = less agreement
** more agreement = greater percieved difference
** distributions is skewed to the left = more people support that values are inconcievable

g val_five = VAL
recode val_five -97=. -98=. 1=5 2=4 3=3 4=2 5=1
tab val_five

g val_three = VAL
recode val_three -97=. -98=. 1=3 2=3 3=2 4=1 5=1
tab val_three


********************************************************************************
****************************** CONTROL VARIABLES *******************************
********************************************************************************

** Worship attendance
** principle: use only D8 (attendance in the present) and recode so bigger numbers
** mean more attendance

tab WSHIP
tab WSHIP_CO

g rel_att = WSHIP
recode rel_att -99=. -98=. -97=. -56=. 7=0 6=1 5=2 4=3 3=4 2=5 1=6
tab rel_att

** Political interest 
** principle in the RC and recode so bigger numbers mean more attendance

tab POL_RC
g pol_int = POL_RC
recode pol_int -98=. -97=. 1=3 2=2 3=1 4=0
tab pol_int

** Political Knowledge
** strategy loop over these variables to recode them, because the codings of these vars is making the problem. 

tab PRTCDA_NL 
tab PRTPVDA_NL 
tab PRTVVD_NL 
tab PRTPVV_NL 
tab PRTD66_NL 
tab PRTGL_NL 
tab PRTSP_NL 
tab PRTCHU_NL 
tab PRTSGP_NL 
tab PRTPAR_NL 
tab PRTELSE_NL 
tab PRT_DK_NL 
tab PRT_REF_NL

foreach v in PRTCDA_NL PRTPVDA_NL PRTVVD_NL PRTPVV_NL PRTD66_NL PRTGL_NL PRTSP_NL PRTCHU_NL PRTSGP_NL PRTPAR_NL PRTELSE_NL {
	
	g `v'_2 = `v'
	recode `v'_2 1=1 0=0 -56=.
	tab `v'_2
}

g PRT_DK_NL_2 = PRT_DK_NL
recode PRT_DK_NL_2 1=0 0=0 -56=.
tab PRT_DK_NL_2

g PRT_REF_NL_2 = PRT_REF_NL
recode PRT_REF_NL_2 1=. 0=0 -56=.
tab PRT_REF_NL_2

g political_knowledge = PRTCDA_NL_2 + PRTPVDA_NL_2 + PRTVVD_NL_2 + PRTPVV_NL_2 + PRTD66_NL_2 + PRTGL_NL_2 + PRTSP_NL_2 + PRTCHU_NL_2 + PRTSGP_NL_2 + PRTPAR_NL_2 + PRTELSE_NL_2 + PRT_DK_NL_2 + PRT_REF_NL_2
tab political_knowledge

** Identity 
** principle: this variable is just a proxy for British ID
** it is also an alternative outcome for val five 

tab IPIDRES
g host_cntry_importance = IPIDRES
recode host_cntry_importance -98=. -97=. 4=0 3=1 2=2 1=3
tab host_cntry_importance

g host_cntry_importance_2 = IPIDRES
recode host_cntry_importance_2 -98=. -97=. 4=0 3=0 2=1 1=1
tab host_cntry_importance_2

** Language
** Divide into two variables. 
** One imitating the original one combining speaking with partner and children
** under assumption that these conversations mostly happen at home. 
** to save obs. code -99 (filtered) as 0 (never) and the rest as one which will give
** two binary indicators to be summed up. 

** speaking with friends should be alternative to this because it does not have
** this number of missing values and is theoretically meaningful as well.


tab LRCUD
tab LRCSPK
tab LRCRD
tab LSPKP
tab LSPKKID
tab LSPKFR

g lan_part = LSPKP
recode lan_part -99=0 1/4=1 5=0 -98=. -97=.
tab lan_part

g lan_child = LSPKKID
recode lan_child -99=0 1/4=1 5=0
tab lan_child 

g lan_home = lan_part + lan_child
tab lan_home

g lan_friends_sca = LSPKFR
recode lan_friends_sca -99=0 -98=. 5=0 4=1 3=2 2=3 1=4 -97=. -56=.
tab lan_friends_sca

** even better measure is understanding the language of the RC which is not completely compatible with 

g lan_und = LRCUD
g lan_speak = LRCSPK
g lan_read = LRCRD

recode lan_und -99=. -98=. 4=0 3=1 2=2 1=3 -97=.
recode lan_speak -99=. -98=. 4=0 3=1 2=2 1=3 -97=.
recode lan_read 4=0 3=1 2=2 1=3 -98=. -97=.

tab lan_und
tab lan_speak
tab lan_read 

g lan_compet = lan_und + lan_speak + lan_read
tab lan_compet

** country of birth 

tab CB
tab CTZSHIP_CO
tab CB_op

** Gender: female 

tab SEX

g female = SEX
recode female 1=0 2=1
tab female

** Age 

tab YB
tab YB_op
codebook YB_op

codebook SD_NL
* create interview year
g int_year = year(SD_NL)
tab int_year

g age = int_year - YB_op
tab age


** Education 

tab FTEDUY
tab FTEDUY_op
g educ_yrs = FTEDUY_op
recode educ_yrs -99=. -52=.
tab educ_yrs

** should not be used because of high non-response rate
tab EDUMAX_CO_PL 
tab EDUMAX_CO_TR 
tab EDUMAX_CO_MOR 
tab EDUMAX_CO_BUL 
tab EDUMAX_CO_ANT 
tab EDUMAX_CO_SUR

** Income 

tab INCW_op 
tab INCM_op 
tab INCY_op

** income adjusted for weekly and daily to make the equivalent groups as in Oskooii
** low income less than 25000 | 25000/52 (weeks) | 25000/12
** mid between low and high
** high income higher than 50000 | 50000/12 (weeks) | 50000/12
display 25000/12
display 25000/52
display 50000/12
display 50000/52

g INCW_op_2 = INCW_op
g INCM_op_2 = INCM_op
g INCY_op_2 = INCY_op

recode INCW_op_2 -99 = .
recode INCM_op_2 -99 = .
recode INCY_op_2 -99 = .

tab INCW_op_2 
tab INCM_op_2 
tab INCY_op_2

gen lowinc=0
replace lowinc=1 if INCY_op_2 < 25000 | INCM_op_2 < 2083 | INCW_op_2 < 480
tab lowinc

gen medinc=0 
replace medinc=1 if (INCY_op_2 >= 25000 & INCY_op_2 < 50000) | (INCM_op_2 >= 2083 & INCM_op_2< 4167) | (INCW_op_2 >= 480 & INCW_op_2 < 962)
tab medinc

g highinc=0 
replace highinc=1 if (INCY_op_2 >= 50000 & INCY_op_2 != .)| (INCM_op_2 >= 4167 & INCM_op_2 != .) | (INCW_op_2 >= 962 & INCW_op_2 != .)
tab highinc

tab lowinc
tab medinc
tab highinc

gen misinc=0
replace misinc=1 if lowinc == 0 & medinc == 0 & highinc == 0
tab misinc

** Ethnic group 

tab CB 
tab CB_op

tab RESRG_CO_PL 
tab RESRG_CO_PL_op 
tab RESRG_CO_MOR 
tab RESRG_CO_BUL 
tab RESRG_CO_ANT

tab LMA 
tab LMA_op

tab EG

g antillean = EG
g bulgarian = EG
g moroccan = EG
g polish = EG
g surinamese = EG
g turkish = EG

recode antillean 1=1 else=0
recode bulgarian 2=1 else=0
recode moroccan 3=1 else=0
recode polish 5=1 else=0
recode surinamese 6=1 else=0 
recode turkish 7=1 else=0

tab antillean
tab bulgarian
tab moroccan
tab polish
tab surinamese
tab turkish

********************************************************************************
*************************** ADDITIONAL CONTROLS ********************************
********************************************************************************

** Citizenship 
** All of them are citizens of their CO, so there is no variation 

tab CTZSHIP_CO 
tab CTZSHIP_RC 
tab CTZSHIP_ELSE 
tab CTZSHIP_dk 
tab CTZSHIP_rf 
tab CTZSHIP_op

** Duration of stay 

tab IMDATE
tab IMDATE_OP_1_NL 
tab IMDATE_OP_2_NL
codebook IMDATE_OP_1_NL
codebook IMDATE_OP_2_NL

g immig_date = IMDATE_OP_1_NL
codebook immig_date
tab immig_date

g immig_yr = year(IMDATE_OP_1_NL)
tab immig_yr
recode immig_yr 1582=1982
egen immig_year = rowmax(immig_yr IMDATE_OP_2_NL)
tab immig_year
recode immig_year -99=.

g dur_stay = int_year - immig_year
tab dur_stay

** do this for months

** transform both interwiew date and immigration date into stata readable format

** date 2 and date 3 is date of interview
codebook SD_NL
tab SD_NL

** tranform from numeric daily date to monthly date for interview date
gen month_int = mofd(SD_NL)
format month_int %tm
tab month_int

** tranform from numeric daily date to monthly date for immigration date date
gen month_immig = mofd(IMDATE_OP_1_NL)
format month_immig %tm
tab month_immig

g dur_stay_month = month_int - month_immig
tab dur_stay_month
recode dur_stay_month -1=. 5137=. 5138=. 5139=. 5140=. 5141=. 5142=. 5143=. 5144=. 5145=.
tab dur_stay_month

** Network local area

tab LOCAPP_CO

g loc_area = LOCAPP_CO
recode loc_area -98=. 5=0 4=1 3=2 2=3 1=4 -97=.
tab loc_area


** Life Satisfaction 

* Feel at home

tab HM_RC
g feel_home = HM_RC
recode feel_home -98=. 1=2 2=1 3=0 -97=. 
tab feel_home

* Life satisfaction 

tab SATIS_RC
g life_satis = SATIS_RC
recode life_satis -98=. 4/5=1 3=2 1/2=3 -97=.
tab life_satis

** Employment

tab ACT
g working = ACT
recode working -97=. 1=1 -98=. -99=. else=0 
tab working 


** Network 

* almost no variation 
tab GRPPPCO 

tab FRN_RC 
g friends = FRN_RC
recode friends -98=. -97=. 1=0 2=1 3=2 4=3 5=4 6=5
tab friends

* almost no variation 
tab FRCO_RC 
tab FRCO_RC_op

save "SCIP_NL_W1", replace 



