sysdir
sysdir set PLUS " C:\Users\Afua\Documents\Stata\Stata16\ado\plus"
cd "K:\project\BayesEdu\Datasets\Brazil\Brazil"

*For all countries we do single year, single ages and 5 year interval and 5 year age groups
use BRIR01FL.DTA
tfr2, len(29) trend(1) cy
by v106, sort: tfr2, len(20) trend(1) cy
tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
*Values for education . or unknown were ignored as the sample sizes were very small
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


*Second wave
use BRIR21FL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(25) trend(1) cy
tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy

*Values for education . or unknown were ignored as the sample sizes were very small
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


*Third wave
use BRIR31FL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(30) trend(1) cy
tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy

*Values for education . or unknown were ignored as the sample sizes were very small
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


*We continue the process for Turkey and the other countries
*Single year and single age groups as well as 5 year interval and 5 year age groups 
cd "K:\project\BayesEdu\Datasets\Turkey\Turkey DHS"
use TRIR31FL.DTA
tfr2, len(20) trend(1) cy
by v106, sort: tfr2, len(20) trend(1) cy
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates

tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


use TRIR41FL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(25) trend(1) cy
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates

tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


use TRIR4AFL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(25) trend(1) cy
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates

tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates



use TRIR51FL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(30) trend(1) cy
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates

tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


use TRIR62FL.DTA
tfr2, len(30) trend(1) cy
by v106, sort: tfr2, len(25) trend(1) cy
tabexp, len(30) trend(1) ageg(1) cy rates
by v106, sort: tabexp, len(30) trend(1) ageg(1) cy rates

tfr2, len(30) trend(5) cy
by v106, sort: tfr2, len(30) trend(5) cy
tabexp, len(30) trend(5) ageg(5) cy rates
by v106, sort: tabexp, len(30) trend(5) ageg(5) cy rates


*Add data from world fertility survey
wfs using trsr01, dhs
wfs  V704A V006 V007 V008 V009 V011 B011-B245 using trsr01, dhs

tabexp, len(30) trend(5) ageg(5) rates
by v704a, sort: tabexp, len(30) trend(5) ageg(5) rates

tabexp, len(30) trend(1) ageg(1) rates
by v704a, sort: tabexp, len(30) trend(1) ageg(1) rates


*tfr2 code itself did not work quite well...Maybe something can be done?
