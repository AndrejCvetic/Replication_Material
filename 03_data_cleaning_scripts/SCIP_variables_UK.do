*** VARIABLES FOR THE REPLICATION ***

*** Original code by Andrej Cvetic
*** cvetica@tcd.ie

*** 05/10/2023


*** READ IN THE ORIGINAL SCIP DATASET (UK data, wave 1)

** WINDOWS FILEPATH cd "C:\Users\Andrej Cvetic\Desktop\PhD\2023_TT_Replication Paper\1_Datasets for Replication\01_Working Datasets"

pwd
use "ZA5956_uk_wave1_v1-0-0", clear

numlabel, add force

*** UK Dataset ***

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
recode disc_freq -98=. 5=0 4=1 3=2 2=3 1=3 
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
recode ebe 2=1
tab ebe


** In group attachment - view of value correspeondence is proxy for identifying 
** with members of host nation 

tab VAL

** principle: recode to have var of three and five cat; smaller numbers = less agreement = smaller percieved dif.
** more agreement = bigger numbers = greater percieved difference
** distributions is skewed to the left = more people support that values are inconcievable

g val_five = VAL
recode val_five -98=. 1=5 2=4 3=3 4=2 5=1
tab val_five

g val_three = VAL
recode val_three -98=. 1=3 2=3 3=2 4=1 5=1
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
recode rel_att -99=. -98=. -97=. 7=0 6=1 5=2 4=3 3=4 2=5 1=6
tab rel_att

** Political interest 
** principle in the RC and recode so bigger numbers mean more attendance

tab POL_RC
g pol_int = POL_RC
recode pol_int -98=. -97=. 1=3 2=2 3=1 4=0
tab pol_int

** Political Knowledge
** strategy loop over these variables to recode them, because the codings of these vars is making the problem. 

tab PRTLAB_GB 
tab PRTCON_GB 
tab PRTLD_GB 
tab PRTSNP_GB 
tab PRTGR_GB 
tab PRTBNP_GB 
tab PRTPC_GB 
tab PRTUKIP_GB 
tab PRTDUP_GB 
tab PRTUUP_GB 
tab PRTSF_GB 
tab PRTELSE_GB 
tab PRTRES_GB 
tab PRTVER_GB 
tab PRTPOP_GB 
tab PRT_GB_dk 
tab PRT_GB_rf

foreach v in PRTLAB_GB PRTCON_GB PRTLD_GB PRTSNP_GB PRTGR_GB PRTBNP_GB PRTPC_GB PRTUKIP_GB PRTDUP_GB PRTUUP_GB PRTSF_GB PRTELSE_GB PRTRES_GB PRTVER_GB PRTPOP_GB {
	
	g `v'_2 = `v'
	recode `v'_2 1=1 0=0
	tab `v'_2
}

g PRT_GB_dk_2 = PRT_GB_dk
recode PRT_GB_dk_2 1=0 0=0
tab PRT_GB_dk_2

g PRT_GB_rf_2 = PRT_GB_rf
recode PRT_GB_rf_2 1=. 0=0
tab PRT_GB_rf_2

g political_knowledge = PRTLAB_GB_2 + PRTCON_GB_2 + PRTLD_GB_2 + PRTSNP_GB_2 + PRTGR_GB_2 + PRTBNP_GB_2 + PRTPC_GB_2 + PRTUKIP_GB_2 + PRTDUP_GB_2 + PRTUUP_GB_2 + PRTSF_GB_2 + PRTELSE_GB_2 + PRTRES_GB_2 + PRTVER_GB_2 + PRTPOP_GB_2 + PRT_GB_dk_2 + PRT_GB_rf_2
tab political_knowledge

** Identity 
** principle: this variable is just a proxy for British ID
** equivalent to val_five and val_three 


tab IPIDRES
g host_cntry_importance = IPIDRES
recode host_cntry_importance -98=. 4=0 3=1 2=2 1=3
tab host_cntry_importance


** 0 is that host country is not important :: 1 is that host country is important

g host_cntry_importance_2 = IPIDRES
recode host_cntry_importance_2 -98=. 4=0 3=0 2=1 1=1
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
recode lan_part -99=0 1/4=1 5=0
tab lan_part

g lan_child = LSPKKID
recode lan_child -99=0 1/4=1 5=0
tab lan_child 

g lan_home = lan_part + lan_child
tab lan_home

g lan_friends_sca = LSPKFR
recode lan_friends_sca -99=0 -98=. 6=. 5=0 4=1 3=2 2=3 1=4
tab lan_friends_sca


** even better measure is understanding the language of the RC which is not completely compatible with 

g lan_und = LRCUD
g lan_speak = LRCSPK
g lan_read = LRCRD

recode lan_und -99=. -98=. 4=0 3=1 2=2 1=3
recode lan_speak -99=. -98=. 4=0 3=1 2=2 1=3
recode lan_read 4=0 3=1 2=2 1=3

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

codebook datetime

g date = datetime
codebook date
tab date
* remove seconds and minutes
g date2 = dofc(datetime)
* reformat the date and bring in the mask
format date2 %td
tab date2
codebook date2
* create interview year
g int_year = year(date2)

g age = int_year - YB_op
tab age

** Education 

tab FTEDUY
tab FTEDUY_op
g educ_yrs = FTEDUY_op
recode educ_yrs -99=.
tab educ_yrs


tab EDUMAX_CO_PK_GB
tab EDUMAX_CO_PL

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

tab EG

tab CB
tab RESCY_CO_P_OP
tab RESRG_CO_PL

tab LMA_GB_PL
tab LMA_GB_PL_op
tab LMA_PK

* g polish = 0 
* replace polish = 1 if LMA_GB_PL == 1 | LMA_GB_PL == 13 | LMA_GB_PL == 14 | LMA_GB_PL == 15
* tab polish

* g pakistani = 0 
* replace pakistani = 1 if LMA_PK != -99
* tab pakistani 

g pakistani = EG
recode pakistani 4=1 else =0 
tab pakistani 

g polish = EG
recode polish 5=1 else=0 
tab polish

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
tab IMDATE_op
codebook IMDATE_op

g immig_date = IMDATE_op
codebook immig_date
tab immig_date

gen immig_year = year(dofm(immig_date))
tab immig_year

g dur_stay = int_year - immig_year
tab dur_stay

** do this for months

** transform both interwiew date and immigration date into stata readable format

** date 2 and date 3 is date of interview
codebook date2
tab date2

** tranform from numeric daily date to monthly date
gen date3 = mofd(date2)
format date3 %tm
tab date3

g dur_stay_month = date3 - immig_date
tab dur_stay_month
recode dur_stay_month -2=. -3=.
tab dur_stay_month

** Network local area

tab LOCAPP_CO

g loc_area = LOCAPP_CO
recode loc_area -98=. 5=0 4=1 3=2 2=3 1=4
tab loc_area


** Life Satisfaction 

* Feel at home

tab HM_RC
g feel_home = HM_RC
recode feel_home -98=. 1=2 2=1 3=0
tab feel_home

* Life satisfaction 

tab SATIS_RC
g life_satis = SATIS_RC
recode life_satis -98=. 4/5=1 3=2 1/2=3
tab life_satis

** Employment

tab ACT
g working = ACT
recode working -97=. 1=1 else=0 
tab working 


** Network 

* almost no variation 
tab GRPPPCO 

tab FRN_RC 
g friends = FRN_RC
recode friends -98=. -97=. 1=1 2=2 3=3 4=4 5=5 6=0
tab friends

* almost no variation 
tab FRCO_RC 
tab FRCO_RC_op

save "SCIP_UK_W1", replace


