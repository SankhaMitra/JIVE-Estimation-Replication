cd "D:\Replication_Exercises\Mixtape_IV_JudgeFE_JIVE\STATA"
**** Codes are adapted from Causal Inference: The Mixtape" e-book by Scott Cunningham
use judge_fe

vl create judge_pre = (judge_pre_1 judge_pre_2 judge_pre_3 judge_pre_4 judge_pre_5 judge_pre_6 judge_pre_7 judge_pre_8) 
vl create off = (fel mis sum F1 F2 F3 F M1 M2 M3 M1) 
vl create demo = (white black age male) 
vl create control2 = (day day2 day3 bailDate t1 t2 t3 t4 t5 t6) 
vl create prior = (priorCases prior_felChar prior_guilt onePrior threePriors priorWI5) 
vl create maxcontrols = (robbery possess aggAss drugSell DUI1st) 

***normal standard errors***
eststo model_ols_min: qui regress guilt $control2 jail3
eststo model_ols_max: qui regress guilt $control2 $off $demo $prior $maxcontrols jail3 
eststo model_iv_min: qui ivregress 2sls guilt $control2 (jail3 = $judge_pre)
eststo model_iv_max: qui ivregress 2sls guilt $control2 $off $demo $prior $maxcontrols (jail3 = $judge_pre) 
eststo model_jive_min: qui jive guilt $control2 (jail3 = $judge_pre)
eststo model_jive_max: qui jive guilt $control2 $off $demo $prior $maxcontrols (jail3 = $judge_pre) 
esttab model_ols_min model_ols_max model_iv_min model_iv_max model_jive_min model_jive_max, keep(jail3)  

***robust standard errors***
eststo model_ols_min: qui regress guilt $control2 jail3, robust
eststo model_ols_max: qui regress guilt $control2 $off $demo $prior $maxcontrols jail3, robust 
eststo model_iv_min: qui ivregress 2sls guilt $control2 (jail3 = $judge_pre), robust first
eststo model_iv_max: qui ivregress 2sls guilt $control2 $off $demo $prior $maxcontrols (jail3 = $judge_pre), robust first 
eststo model_jive_min: qui jive guilt $control2 (jail3 = $judge_pre), robust
eststo model_jive_max: qui jive guilt $control2 $off $demo $prior $maxcontrols (jail3 = $judge_pre), robust 
esttab model_ols_min model_ols_max model_iv_min model_iv_max model_jive_min model_jive_max, keep(jail3)  
