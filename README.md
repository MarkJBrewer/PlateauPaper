# PlateauPaper
Repository for code from the "plateau" climate envelope analysis in Brewer, O'Hara, Anderson and Ohlemüller (2016) in Methods in Ecology and Evolution

The file "Plateau.rmd" requires a number of packages:

R2WinBUGS,mgcv,mapproj,maps,coda,boot,oce,gsw

in addition to the package "plateau", the focus of the paper, which is downloaded from GitHub and installed as part of the code. The separate WinBUGS package must also be installed (see http://www.mrc-bsu.cam.ac.uk/software/bugs/).

The code should, for the most part, need little or no intervention from the user. Note that the MCMC analyses (which call WinBUGS) each take several hours to run. If WinBUGS is not installed in the folder "C:/Program Files (x86)/WinBUGS" then the option "bugs.directory" will need to be set to the correct location when calling the function fit.bugs.env. Note also that depending on your version of Windows, you may need to run your R program with administrator permissions.
