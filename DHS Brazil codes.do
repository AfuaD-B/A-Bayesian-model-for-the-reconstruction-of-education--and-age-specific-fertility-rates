
*This is an example code that was used to retrieve trf2 module fertility estimates in STATA from DHS data
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







