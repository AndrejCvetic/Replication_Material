*** VARIABLES FOR EXTENSION OF THE REPLICATION ***

*** Original code by Kassra A. R. Oskooii 

*** Additional variables created by Andrej Cvetic
*** cvetica@tcd.ie

*** 19/9/2023


*** READ IN THE ORIGINAL EMBES DATASET

cd "C:\Users\Andrej Cvetic\Desktop\PhD\2023_TT_Replication Paper\1_Datasets for Replication\01_Working Datasets"


use "british_election_study_ethnic_minority_survey_2010_data_anon", clear

numlabel, add force

**********************
***Response Variables
**********************

*** NOTE: THE KEY IDEA IS TO ACCURATELY REPRODUCE ALL THE VARIABLES OSKOOII USED IN HIS ORIGINAL ANALYSIS. 
*** HIS SOMEWHAT ARBITRARY WAY OF HANDLING MISSING VALUES IS REPRODUCED HERE. 

* Voted in May 2010 General ELECTION
gen voted2010 = bq12_1
** remove -2 here.
recode voted2010 -2=. -1=0 1=1 2=0 
tab voted2010


* Voted in May 6 2010 Local Election
gen voted2010_local = bq52_1
recode voted2010_local -1=0 1=1 2=0 3=. 5=.
*recode voted2010_local -1=0 -2=. 1=1 2=0 3=.
tab voted2010_local


* Took part in ethnic, cultural, or religious association in past 12 months
** Original code: gen ethnic_active = 
gen ethnic_active = eq42
recode ethnic_active -2=. -1=0 1=1 2=0 
tab ethnic_active

*Identity coding below (under control variables)


**********************
***Explanatory Variables
**********************

** General DISC
* Have you experienced disc in past 5 years (Yes or No)
gen disc = eq37
recode disc -2=. -1=. 2=0 1=1 
tab disc

* General DISC + Frequency (If Yes)
gen disc_level = .
replace disc_level=0 if disc==0
replace disc_level=1 if eq38==3
replace disc_level=2 if eq38==2
replace disc_level=3 if eq38==1
tab disc_level

* Disc Based on Ethnicity, Race, or Skin Colour (Not age, gender, etc.)
gen disc_race_binary=. 
replace disc_race_binary=0 if disc==0 
replace disc_race_binary=2 if disc==1 & eq39a==0
replace disc_race_binary=1 if disc==1 & eq39a==1
recode disc_race_binary 2=0
tab disc_race_binary

* Disc based on RACE/ETH + Frequency 
gen disc_race=.
replace disc_race=0 if disc_level==0 | disc_race_binary==0
replace disc_race=1 if disc_level==1 & disc_race_binary==1
replace disc_race=2 if disc_level==2 & disc_race_binary==1
replace disc_race=3 if disc_level==3 & disc_race_binary==1
tab disc_race


* RACE/ETH Disc By Domain & Frequency

* On the street
gen street_racedisc=. 
replace street_racedisc=0 if eq40a==0 | disc_race==0
replace street_racedisc=1 if eq40a==1 & disc_race==1
replace street_racedisc=2 if eq40a==1 & disc_race==2
replace street_racedisc=3 if eq40a==1 & disc_race==3
tab street_racedisc

* In a shop...
gen shop_racedisc=.
replace shop_racedisc =0 if eq40b==0 | disc_race==0
replace shop_racedisc =1 if eq40b==1 & disc_race==1
replace shop_racedisc =2 if eq40b==1 & disc_race==2
replace shop_racedisc =3 if eq40b==1 & disc_race==3
tab shop_racedisc

* Social gathering with friends, neighbours 
gen neighbor_racedisc=.
replace neighbor_racedisc =0 if eq40g==0 | disc_race==0
replace neighbor_racedisc =1 if eq40g==1 & disc_race==1
replace neighbor_racedisc =2 if eq40g==1 & disc_race==2
replace neighbor_racedisc =3 if eq40g==1 & disc_race==3
tab neighbor_racedisc

* Police/Courts
gen polic_racedisc=.
replace polic_racedisc =0 if eq40d ==0 | disc_race==0
replace polic_racedisc =1 if eq40d ==1 & disc_race==1
replace polic_racedisc =2 if eq40d ==1 & disc_race==2
replace polic_racedisc =3 if eq40d ==1 & disc_race==3
tab polic_racedisc

* Immigration/Gov officials
gen gov_racedisc=.
replace gov_racedisc =0 if eq40f ==0 | disc_race==0
replace gov_racedisc =1 if eq40f ==1 & disc_race==1
replace gov_racedisc =2 if eq40f ==1 & disc_race==2
replace gov_racedisc =3 if eq40f ==1 & disc_race==3
tab gov_racedisc

* Work/When applying for a job/promotion 
gen job_racedisc=. 
replace job_racedisc=0 if eq40c==0 |disc_race==0
replace job_racedisc=1 if eq40c==1 & disc_race==1
replace job_racedisc=2 if eq40c==1 & disc_race==2
replace job_racedisc=3 if eq40c==1 & disc_race==3
tab job_racedisc

*School/college/university
gen school_racedisc=.
replace school_racedisc =0 if eq40e ==0 | disc_race==0
replace school_racedisc =1 if eq40e ==1 & disc_race==1
replace school_racedisc =2 if eq40e ==1 & disc_race==2
replace school_racedisc =3 if eq40e ==1 & disc_race==3
tab school_racedisc

* Disc by Source
gen socdisc= street_racedisc+  shop_racedisc +  neighbor_racedisc 
gen poldisc = polic_racedisc +  gov_racedisc + school_racedisc + job_racedisc 



**********************
***Control Variables
**********************

*** Worship Attendance 
* Belong to Religion
** original code: gen belong_rel = bq106_1
gen belong_rel = zq106_1
recode belong_rel -2=. -1=. 1=1 2=0
tab belong_rel

* Includes only those who Belong to a Religion (survey design)

** original code: gen relatt_oth=bq106_4 
gen relatt_oth=eq106_4
recode relatt_oth -1=. -2=. 1=5 2=4 3=3 4=2 5=1 6=0 
tab relatt_oth

* Combine to include those who don't belong to a religion
gen relatt_oth_r=. 
replace relatt_oth_r = 0 if belong_rel==0 | relatt_oth==0
replace relatt_oth_r = 1 if belong_rel==1 & relatt_oth==1
replace relatt_oth_r = 2 if belong_rel==1 & relatt_oth==2
replace relatt_oth_r = 3 if belong_rel==1 & relatt_oth==3
replace relatt_oth_r = 4 if belong_rel==1 & relatt_oth==4
replace relatt_oth_r = 5 if belong_rel==1 & relatt_oth==5
tab relatt_oth_r


* Interest in British politics; not at all --> a great deal
gen pol_interest = bq1
recode pol_interest -2/-1=. 1=4 2=3 3=2 4=1 5=0 
tab pol_interest 


* Political knowledge 
gen polknow1 = bq79_1
recode polknow1 -1=0 2=0 
tab polknow1

gen polknow2 =  bq79_3
recode polknow2 -1=0 1=0 2=1 
tab polknow2

gen polknow3 = bq79_5
recode polknow3 -1=0 1=0 2=1 
tab polknow3

gen polknow4 = bq79_8
recode polknow4 -1=0 1=1 2=0
tab polknow4

** Original code: gen polknow5 = bq79_9
gen polknow5 = eq79_9
recode polknow5 -1=0 1=1 2=0
tab polknow5

gen polknowledge= polknow1 + polknow2+ polknow3+ polknow4+ polknow5
tab polknowledge



* Party Identification 
gen partyid=bq9_1
recode partyid -2=. -1=0 1=0 2/12=1
tab partyid 


*Identity
gen identity = eq16a
recode identity -1=. -2=. 1/2=0 3=1 4/5=2 6=1
tab identity


* English Main Langauge
gen english = eq61_1
recode english -2=. -1=. 1=1 2=0
tab english


* Nativity 
gen native_born=0 
replace native_born =1 if bq102_1 ==1  | bq102_1 ==2 | bq102_1 ==3 | bq102_1 ==42
tab native_born

* Gender
gen female=0 
** Original code: replace female=1 if bq88==2
replace female=1 if zq88==2
tab female 

* Age
** Original code: gen age= bq89 
gen age= zq89
tab age 


* Education
gen education2 = 0

** Original code: 
*replace education2=1 if  bq95_3==14 | bq95_3==15 | bq95_3==16 | bq95_3==17 | bq95_3==18 | eq64_2==5 | eq64_2==6 | eq64_2==7
*replace education2=2 if bq95_3==10 | bq95_3==11| bq95_3==12 | bq95_3==13 |eq64_2==1
*replace education2=3 if bq95_3==6 | bq95_3==7| bq95_3==8 | bq95_3==9 | eq64_2==2
*replace education2=4 if bq95_3==2 | bq95_3==3 | bq95_3==4 | bq95_3==5 | eq64_2==3 
*replace education2=5 if bq95_3==1 | eq64_2==4 

replace education2=1 if  zq95_3==14 | zq95_3==15 | zq95_3==16 | zq95_3==17 | zq95_3==18 | eq64_2==5 | eq64_2==6 | eq64_2==7
replace education2=2 if zq95_3==10 | zq95_3==11| zq95_3==12 | zq95_3==13 |eq64_2==1
replace education2=3 if zq95_3==6 | zq95_3==7| zq95_3==8 | zq95_3==9 | eq64_2==2
replace education2=4 if zq95_3==2 | zq95_3==3 | zq95_3==4 | zq95_3==5 | eq64_2==3 
replace education2=5 if zq95_3==1 | eq64_2==4 


*** Income

** Original code:
*gen lowinc=0
*replace lowinc=1 if bq96==1|bq96==2|bq96==3|bq96==4|bq96==5
*tab lowinc

*gen medinc=0 
*replace medinc=1 if bq96==6|bq96==7|bq96==8|bq96==9|bq96==10
*tab medinc

*gen highinc=0 
*replace highinc=1 if bq96==11|bq96==12|bq96==13|bq96==14
*tab highinc

*gen misinc=0
*replace misinc=1 if bq96==-2|bq96==-1
*tab misinc

gen lowinc=0
replace lowinc=1 if zqinc==1|zqinc==2|zqinc==3|zqinc==4|zqinc==5
tab lowinc

gen medinc=0 
replace medinc=1 if zqinc==6|zqinc==7|zqinc==8|zqinc==9|zqinc==10
tab medinc

gen highinc=0 
replace highinc=1 if zqinc==11|zqinc==12|zqinc==13|zqinc==14
tab highinc

gen misinc=0
replace misinc=1 if zqinc==-2|zqinc==-1
tab misinc

* Ethnic/Racial Background

/* Original code: 
gen black_caribbean =0 
replace black_caribbean=1 if screenet==1 
tab black_caribbean

gen black_african =0 
replace black_african=1 if screenet==2
tab black_african

gen indian=0 
replace indian=1 if screenet==3
tab indian

gen pakistani=0 
replace pakistani=1 if screenet==4
tab pakistani

gen bangladeshi=0 
replace bangladeshi=1 if screenet==5 
tab bangladeshi */

gen black_caribbean =0 
replace black_caribbean=1 if ScreenEthQairre==1 
tab black_caribbean 

gen black_african =0 
replace black_african=1 if ScreenEthQairre==2
tab black_african

gen indian=0 
replace indian=1 if ScreenEthQairre==3
tab indian

gen pakistani=0 
replace pakistani=1 if ScreenEthQairre==4
tab pakistani

gen bangladeshi=0 
replace bangladeshi=1 if ScreenEthQairre==5 
tab bangladeshi

* Voting as duty
gen vote_duty = bq18_6
recode vote_duty -2/-1=. 1=5 2=4 3=3 4=2 5=1 
tab vote_duty


* Efficacy: how much influence on politics
gen efficacy= bq59
recode efficacy -1=. -2=.
tab efficacy 


* Democractic Satisfaction
gen democ_satis = bq61
recode democ_satis -2/-1=. 1=3 2=2 3=1 4=0 
tab democ_satis 


* Trust in British Parliament
gen trust_parliament = bq16_2
recode trust_parliament -1=. -2=.
tab trust_parliament 


********************************************************************************
****************************** NEW VARIABLES ***********************************
********************************************************************************

*** CONVENTIONS FOR RECODING OF THE MISSING VALUES WITH NEW VARIABLES:
*** ALL -1 (IDK) and -2 (REFUSED) ARE CODED AS . (MISSING VALUES). 

*** RESPONSE VARIABLES

** Support for violent demonstration 

tab eq34 
* 254
tab eq35
* 253
tab eq36
* 221

g agains_war = eq34
recode agains_war -1=. -2=. 2=0 1=1
tab agains_war 

g unfair_tax = eq35 
recode unfair_tax -1=. -2=. 2=0 1=1
tab unfair_tax

g job_cuts = eq36
recode job_cuts -1=. -2=. 2=0 1=1
tab job_cuts

g sup_dem = agains_war + unfair_tax + job_cuts
tab sup_dem

g sup_dem_binary = sup_dem
recode sup_dem_binary 0=0 1=1 2=1 3=1
tab sup_dem_binary


tab eq34_2a
tab eq34_2b
tab eq35_2a
tab eq35_2b
tab eq36_2a
tab eq36_2b
** Not enough observations - should not be used in constructing the violent protest support var. 

** Non-electoral political participation: 

tab eq13_1
tab eq13_2
tab eq13_3
tab eq13_4

g protest = eq13_1
recode protest -2=. -1=. 1=1 2=0
tab protest

g petition = eq13_2
recode petition -2=. -1=. 1=1 2=0
tab petition

g boycott = eq13_3
recode boycott -2=. -1=. 1=1 2=0
tab boycott

g money_pol_org = eq13_4 
recode money_pol_org -2=. -1=. 1=1 2=0
tab money_pol_org

g non_elec_partic = protest + petition + boycott + money_pol_org
tab non_elec_partic

g non_elec_partic_binary = non_elec_partic
recode non_elec_partic_binary 0=0 1=1 2=1 3=1 4=1
tab non_elec_partic_binary

** Participation in organisations

tab eq43
g participation_org = eq43
recode participation_org -2=. -1=. 1=1 2=0
tab participation_org
** binary as placebo for ethnic org involvement 


*** EXPLANATORY VARIABLES 

** General DISC
/* Have you experienced disc in past 5 years (Yes or No)
gen disc = eq37
recode disc -2=. -1=. 2=0 1=1 
tab disc

* General DISC + Frequency (If Yes)
gen disc_level = .
replace disc_level=0 if disc==0
replace disc_level=1 if eq38==3
replace disc_level=2 if eq38==2
replace disc_level=3 if eq38==1
tab disc_level */

* Disc Based on Language 
gen disc_lang_binary=. 
replace disc_lang_binary=0 if disc==0 
replace disc_lang_binary=2 if disc==1 & eq39b==0
replace disc_lang_binary=1 if disc==1 & eq39b==1
recode disc_lang_binary 2=0
tab disc_lang_binary

* Disc based on LANG + Frequency 
gen disc_lang=.
replace disc_lang=0 if disc_level==0 | disc_lang_binary==0
replace disc_lang=1 if disc_level==1 & disc_lang_binary==1
replace disc_lang=2 if disc_level==2 & disc_lang_binary==1
replace disc_lang=3 if disc_level==3 & disc_lang_binary==1
tab disc_lang


* LANG Disc By Domain & Frequency

* On the street
gen street_langdisc=. 
replace street_langdisc=0 if eq40a==0 | disc_lang==0
replace street_langdisc=1 if eq40a==1 & disc_lang==1
replace street_langdisc=2 if eq40a==1 & disc_lang==2
replace street_langdisc=3 if eq40a==1 & disc_lang==3
tab street_langdisc

* In a shop...
gen shop_langdisc=.
replace shop_langdisc =0 if eq40b==0 | disc_lang==0
replace shop_langdisc =1 if eq40b==1 & disc_lang==1
replace shop_langdisc =2 if eq40b==1 & disc_lang==2
replace shop_langdisc =3 if eq40b==1 & disc_lang==3
tab shop_langdisc

* Social gathering with friends, neighbours 
gen neighbor_langdisc=.
replace neighbor_langdisc =0 if eq40g==0 | disc_lang==0
replace neighbor_langdisc =1 if eq40g==1 & disc_lang==1
replace neighbor_langdisc =2 if eq40g==1 & disc_lang==2
replace neighbor_langdisc =3 if eq40g==1 & disc_lang==3
tab neighbor_langdisc

* Police/Courts
gen polic_langdisc=.
replace polic_langdisc =0 if eq40d ==0 | disc_lang==0
replace polic_langdisc =1 if eq40d ==1 & disc_lang==1
replace polic_langdisc =2 if eq40d ==1 & disc_lang==2
replace polic_langdisc =3 if eq40d ==1 & disc_lang==3
tab polic_langdisc

* Immigration/Gov officials
gen gov_langdisc=.
replace gov_langdisc =0 if eq40f ==0 | disc_lang==0
replace gov_langdisc =1 if eq40f ==1 & disc_lang==1
replace gov_langdisc =2 if eq40f ==1 & disc_lang==2
replace gov_langdisc =3 if eq40f ==1 & disc_lang==3
tab gov_langdisc

* Work/When applying for a job/promotion 
gen job_langdisc=. 
replace job_langdisc=0 if eq40c==0 |disc_lang==0
replace job_langdisc=1 if eq40c==1 & disc_lang==1
replace job_langdisc=2 if eq40c==1 & disc_lang==2
replace job_langdisc=3 if eq40c==1 & disc_lang==3
tab job_langdisc

*School/college/university
gen school_langdisc=.
replace school_langdisc =0 if eq40e ==0 | disc_lang==0
replace school_langdisc =1 if eq40e ==1 & disc_lang==1
replace school_langdisc =2 if eq40e ==1 & disc_lang==2
replace school_langdisc =3 if eq40e ==1 & disc_lang==3
tab school_langdisc

* Language Disc by Source
gen socdisc_lang = street_langdisc+  shop_langdisc +  neighbor_langdisc 
gen poldisc_lang = polic_langdisc +  gov_langdisc + school_langdisc + job_langdisc 

tab socdisc_lang
tab poldisc_lang


* Disc Based on Religion 
gen disc_relg_binary=. 
replace disc_relg_binary=0 if disc==0 
replace disc_relg_binary=2 if disc==1 & eq39c==0
replace disc_relg_binary=1 if disc==1 & eq39c==1
recode disc_relg_binary 2=0
tab disc_relg_binary

* Disc based on RELG + Frequency 
gen disc_relg=.
replace disc_relg=0 if disc_level==0 | disc_relg_binary==0
replace disc_relg=1 if disc_level==1 & disc_relg_binary==1
replace disc_relg=2 if disc_level==2 & disc_relg_binary==1
replace disc_relg=3 if disc_level==3 & disc_relg_binary==1
tab disc_relg


* RELG Disc By Domain & Frequency

* On the street
gen street_relgdisc=. 
replace street_relgdisc=0 if eq40a==0 | disc_relg==0
replace street_relgdisc=1 if eq40a==1 & disc_relg==1
replace street_relgdisc=2 if eq40a==1 & disc_relg==2
replace street_relgdisc=3 if eq40a==1 & disc_relg==3
tab street_relgdisc

* In a shop...
gen shop_relgdisc=.
replace shop_relgdisc =0 if eq40b==0 | disc_relg==0
replace shop_relgdisc =1 if eq40b==1 & disc_relg==1
replace shop_relgdisc =2 if eq40b==1 & disc_relg==2
replace shop_relgdisc =3 if eq40b==1 & disc_relg==3
tab shop_relgdisc

* Social gathering with friends, neighbours 
gen neighbor_relgdisc=.
replace neighbor_relgdisc =0 if eq40g==0 | disc_relg==0
replace neighbor_relgdisc =1 if eq40g==1 & disc_relg==1
replace neighbor_relgdisc =2 if eq40g==1 & disc_relg==2
replace neighbor_relgdisc =3 if eq40g==1 & disc_relg==3
tab neighbor_relgdisc

* Police/Courts
gen polic_relgdisc=.
replace polic_relgdisc =0 if eq40d ==0 | disc_relg==0
replace polic_relgdisc =1 if eq40d ==1 & disc_relg==1
replace polic_relgdisc =2 if eq40d ==1 & disc_relg==2
replace polic_relgdisc =3 if eq40d ==1 & disc_relg==3
tab polic_relgdisc

* Immigration/Gov officials
gen gov_relgdisc=.
replace gov_relgdisc =0 if eq40f ==0 | disc_relg==0
replace gov_relgdisc =1 if eq40f ==1 & disc_relg==1
replace gov_relgdisc =2 if eq40f ==1 & disc_relg==2
replace gov_relgdisc =3 if eq40f ==1 & disc_relg==3
tab gov_relgdisc

* Work/When applying for a job/promotion 
gen job_relgdisc=. 
replace job_relgdisc=0 if eq40c==0 |disc_relg==0
replace job_relgdisc=1 if eq40c==1 & disc_relg==1
replace job_relgdisc=2 if eq40c==1 & disc_relg==2
replace job_relgdisc=3 if eq40c==1 & disc_relg==3
tab job_relgdisc

*School/college/university
gen school_relgdisc=.
replace school_relgdisc =0 if eq40e ==0 | disc_relg==0
replace school_relgdisc =1 if eq40e ==1 & disc_relg==1
replace school_relgdisc =2 if eq40e ==1 & disc_relg==2
replace school_relgdisc =3 if eq40e ==1 & disc_relg==3
tab school_relgdisc

* Language Disc by Source
gen socdisc_relg = street_relgdisc+  shop_relgdisc +  neighbor_relgdisc 
gen poldisc_relg = polic_relgdisc +  gov_relgdisc + school_relgdisc + job_relgdisc 

tab socdisc_lang
tab poldisc_lang

tab socdisc_relg
tab poldisc_relg

tab socdisc
tab poldisc

pwcorr socdisc_relg socdisc
pwcorr poldisc_lang poldisc

pwcorr socdisc poldisc socdisc_relg poldisc_relg
pwcorr socdisc poldisc socdisc_relg poldisc_relg poldisc_lang socdisc_lang 
pwcorr socdisc_lang poldisc_lang
pwcorr socdisc_relg poldisc_relg 


/* Since there are much more observations in the racial/ethinc discrimination 
then binary variable for language and religious discrimination can be created to account for the lack of observations. */ 

g socdisc_lang_binary = . 
replace socdisc_lang_binary = 0 if socdisc_lang == 0 
replace socdisc_lang_binary = 1 if socdisc_lang > 0 
tab socdisc_lang_binary

g poldisc_lang_binary = . 
replace poldisc_lang_binary = 0 if poldisc_lang == 0 
replace poldisc_lang_binary = 1 if poldisc_lang > 0 
tab poldisc_lang_binary

g socdisc_relg_binary = . 
replace socdisc_relg_binary = 0 if socdisc_relg == 0 
replace socdisc_relg_binary = 1 if socdisc_relg > 0 
tab socdisc_relg_binary

g poldisc_relg_binary = . 
replace poldisc_relg_binary = 0 if poldisc_relg == 0 
replace poldisc_relg_binary = 1 if poldisc_relg > 0 
tab poldisc_relg_binary


** CONTROL VARIABLES 

** Voted in elections in 2005

tab bq46_1

g vote_2005 = bq46_1
recode vote_2005 -1=. -2=. 1=0 10=0 2=1 3=1 4=1 5=1 7=1 8=1 9=1 11=1
tab vote_2005


** Private religious practice
* Belong to Religion
** original code: gen belong_rel = bq106_1
*gen belong_rel = zq106_1
*recode belong_rel -2=. -1=. 1=1 2=0
*tab belong_rel

* Includes only those who Belong to a Religion (survey design)
* -1 = .

** original code: gen relatt_oth=bq106_4 
g rel_private = zq106_5
recode rel_private -1=. -2=. 1=5 2=4 3=3 4=2 5=1 6=0 
tab rel_private

* Combine to include those who don't belong to a religion
gen rel_private_r=. 
replace rel_private_r = 0 if belong_rel==0 | rel_private==0
replace rel_private_r = 1 if belong_rel==1 & rel_private==1
replace rel_private_r = 2 if belong_rel==1 & rel_private==2
replace rel_private_r = 3 if belong_rel==1 & rel_private==3
replace rel_private_r = 4 if belong_rel==1 & rel_private==4
replace rel_private_r = 5 if belong_rel==1 & rel_private==5
tab rel_private_r

** Strength of Party Identification 

tab partyid 
tab bq9_4

g strength_partyid = bq9_4
recode strength_partyid -2=. -1=. 1=3 2=2 3=1
tab strength_partyid
replace strength_partyid = 1 if strength_partyid == 1 & partyid ==1
replace strength_partyid = 2 if strength_partyid == 2 & partyid ==1
replace strength_partyid = 3 if strength_partyid == 3 & partyid ==1
tab strength_partyid

** Trust: Trust in Parliament: Trust in Police; Trust in Politicians; Overall trust  

g trust_politicians = bq16_3
recode trust_politicians -1=. -2=. 
tab trust_politicians 

g trust_police = bq16_4
recode trust_police -1=. -2=. 
tab trust_police

pwcorr trust_parliament trust_politicians trust_police, sig
factor trust_parliament trust_politicians trust_police, pcf
rotate
* results from factor analysis indicate these variables retain one factor 
* this suggests they can be combined in a single trust scale

g trust_scale = trust_parliament + trust_politicians + trust_police
tab trust_scale

** Generalized trust 

tab eq16a
tab identity
* recode identity -1=. -2=. 1/2=0 3=1 4/5=2 6=1

** eq2 and eq3 not to be used: 
tab eq2
tab eq3

* closeness to ethnic groups
* -1 = .

tab eq17
tab eq19

g closeness_ethn = eq17
recode closeness_ethn -2=. -1=. 1=3 2=2 3=1 4=0
tab closeness_ethn

g closeness_brit = eq19
recode closeness_brit -2=. -1=. 1=3 2=2 3=1 4=0
tab closeness_brit

pwcorr closeness_brit closeness_ethn, sig
** 0.22

** Religious identity 

tab eq106_a

g religion = eq106_a
recode religion -2=. 1=1 2=. 3=2 4=3 5=2 6=2 7=.
tab religion

g muslim = 0
replace muslim = 1 if religion == 3
tab muslim 

g indian_rel = 0
replace indian_rel = 1 if religion == 2
tab indian_rel

g christian = 0
replace christian =1 if religion == 1
tab christian

tab eq4
** distribution is skewed towards very important and extremely important 


** Migration variables

** Time spend in the UK
* Date + bq102_2r

tab Date
codebook Date

tab bq102_2r
codebook bq102_2r

g imm_year = bq102_2r
replace imm_year = . if bq102_2r < 1947
tab imm_year
codebook imm_year

tostring Date, gen(date)
gen date2 = date(date, "YMD")
format date2 %td
g int_year = year(date2)

g dur_of_stay = int_year - imm_year
tab dur_of_stay

** citizenship

tab  eq59_1
g citizen = eq59_1
recode citizen 1=1 2=0 3=1 -2=. -1=.
tab citizen

** visits to the CO

tab eq54_1
tab eq54_2

g visit_co = eq54_2
recode visit_co -2=. -1=. 2=1 3=2 4=3 5=4
tab visit_co


** Involvement in community affairs

tab bq54_2
tab bq54_3

g comm_affairs = bq54_3
recode comm_affairs -2=. -1=. 4=0 3=1 2=2 1=3
tab comm_affairs

** Evaluations of economic situations 

tab bq21
tab bq22
tab bq23
tab bq24

g personal_econ_retro = bq21
g national_econ_retro = bq22
g personal_econ_future = bq23
g national_econ_future = bq24

recode personal_econ_retro -2=. -1=. 1/2=1 3=2 4/5=3
recode national_econ_retro -2=. -1=. 1/2=1 3=2 4/5=3
recode personal_econ_future -2=. -1=. 1/2=1 3=2 4/5=3
recode national_econ_future -2=. -1=. 1/2=1 3=2 4/5=3

tab personal_econ_retro
tab national_econ_retro
tab personal_econ_future
tab national_econ_future

pwcorr personal_econ_retro national_econ_retro personal_econ_future national_econ_future
** personal econ future and national econ future are correlated in 0.58 lvl. 

pwcorr personal_econ_retro personal_econ_future
pwcorr national_econ_retro national_econ_future



** Information >> what are my assumptions here?

tab bq80_1
tab eq48_1
tab eq49a
tab eq49b
tab eq49c
tab eq49d
tab eq49e
tab eq49f
tab eq51
tab bq84_1

g internet = eq51
recode internet -2=. -1=. 1=1 2=0
tab internet 

** Participation 
* for all of them small numbers
tab eq44a
tab eq44b
tab eq44c
tab eq44d
tab eq44e
tab eq44f
tab eq44g

tab eq45a
tab eq45b
tab eq45c
tab eq45d
tab eq45e
tab eq45f


tab eq46_1 
tab eq46_2 
tab eq46_3 
tab eq46_4

g network_friends = eq46_1 
recode network_friends -2=. -1=. 5=0 4=1 3=2 2=3 1=4
tab network_friends 

g network_neighbour = eq46_3 
recode network_neighbour -2=. -1=. 5=0 4=1 3=2 2=3 1=4
tab network_neighbour 

g network_church = eq46_4 
recode network_church -2=. -1=. 5=0 4=1 3=2 2=3 1=4 7=.
tab network_church 

factor network_friends network_neighbour network_church, pcf
rotate

** factor score of 0.7 for all of them - can be put in a scale. 

g social_net = network_friends + network_neighbour + network_church
tab social_net

** friends, neighbourhood and place of worhip should all go into one var
** scales to be calculated

** Prejudice in Britain

tab eq28_2 
*recode to three >> is it getting better for ethnic group (eval of group standing)

g better_worse = eq28_2 
recode better_worse -2=. -1=. 5=1 4=1 3=2 2=3 1=3
tab better_worse

tab eq24
* amount of racial prejudice today in Britain today

*tab eq25
tab eq26

tab eq27
*more or less prejudice agains R's ethnic group (eval of group standing)

g more_prejudice = eq27
recode more_prejudice -2=. -1=. 1=3 2=1 3=2
tab more_prejudice

pwcorr better_worse more_prejudice
* correlatred on -0.2 which is good, but still should be used as alternatives. 

*** save the dataset
save "EMBES_REPLICATION_DATASET", replace





