set more off, perman 
gl base "C:\Users\Public\Documents\bekircan gonzalo\eciraz"
use "$base\finaldata.dta" , clear 
destring industry, replace

*SPEC 1
probit direc wageprem longtenure loglabor pexec pinter pworker, cl(siren) 
outreg2 excel using "$base\results.xls", replace
*SPEC 2
probit direc i.year pexec pinter pworker loglabor wageprem longtenure excessturnover netgrowth loglt  i.industry , cl(siren) 
outreg2 excel using "$base\results.xls", append keep(i.year pexec pinter pworker loglabor wageprem longtenure excessturnover netgrowth loglt)
*SPEC 3
probit direc i.year pexec pinter pworker loglabor wageprem longtenure direc1 direc2 excessturnover netgrowth loglt  i.industry , cl(siren)  
outreg2 excel using "$base\results.xls", append keep(i.year pexec pinter pworker loglabor wageprem longtenure direc1 direc2 excessturnover netgrowth loglt)
*SPEC 4
probit direc i.year pexec pinter pworker loglabor wageprem longtenure excessturnover netgrowth loglt i.industry firmage eff,cl(siren) 
outreg2 excel using "$base\results.xls", append keep( direc i.year pexec pinter pworker loglabor wageprem longtenure excessturnover netgrowth loglt firmage eff)
*SPEC 5
probit direc i.year pexec pinter pworker loglabor wageprem longtenure direc1 direc2 excessturnover netgrowth loglt i.industry firmage eff,cl(siren) 
outreg2 excel using "$base\results.xls", append keep(direc i.year pexec pinter pworker loglabor wageprem longtenure direc1 direc2 excessturnover netgrowth loglt firmage eff)

