# CRHMr
This package is pronounced 'Krimmer' in honour of my wife's family.
The CRHMr functions are used with the Cold Regions Hydrological Modelling (CRHM) platform, which is was developed at the Centre for Hydrology at the University of Saskatchewan. CRHM is available from http://www.usask.ca/hydrology/CRHM.php.

Functions are provided to create time series of forcing meteorological data, read and aggregate CRHM output, set model run parameters, run the CRHM program, and to post-process the output. This package works on Windows, MacOS or Linux. Currently, running CRHM on Linux or MacOS requires the installation of WINE, which is available from https://www.winehq.org/. CRHM is being refactored, and will then be able to run natively on any platform.

CRHMr also contains functions for gap removal, infilling and imputation of time series data, which may be useful for other models. Functions are also provided to deaccumulate, plot and correct weighing gauge precipitation data.

## Installation instructions

### Dependencies
CRHMr depends on several other packages, which you need to install first, from CRAN, before installing CRHMr.
These packages are:
- ggplot2(>= 2.0.0)
- lubridate(>= 1.3)
- plyr
- reshape2
- scales
- signal
- stringr(>= 1.0)
- zoo

To install the dependencies, you can use the menu command **Packages | Install** in Rstudio, or the command `install.packages` as in

`install.packages("ggplot2")`

In addition, CRHMr also supports the package **tidyhydat** , which is available from CRAN. If you are using this package, then you will have to install it, as well as all of its dependencies. You will also have to download the current HYDAT database. It is strongly recommended that you use **tidyhydat** on its
own before using it in CRHMr.

The package **EcoHydRology** is optionally used by the CRHMr function `distributeQsi`. Unfortunately the package
has been kicked off CRAN, which will prevent you installing CRHMr. I don't want to remove the functionality (which
would probably break some people's workflow), so you will have to install and old version of **EcoHydRology** with
the following steps:

1. Download the old version [https://cran.r-project.org/src/contrib/Archive/EcoHydRology/EcoHydRology_0.4.12.1.tar.gz](https://cran.r-project.org/src/contrib/Archive/EcoHydRology/EcoHydRology_0.4.12.1.tar.gz)

2. In **R** install the packages operators, topmodel, DEoptim", and XML. You can use the command `install.packages(c("operators", "topmodel", "DEoptim", "XML"))`

3. Install the downloaded package. Assuming it is in your `Downloads` folder, the command would be `install.packages("~/Downloads/EcoHydRology_0.4.12.1.tar.gz", repos = NULL, type = "source")`




### Installing CRHMr
You can download the complete package, as well as the manual .pdf by clicking on **releases**. However, you can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" - you only have to do this once. Note that this will also install several dependancies
2. Load the devtools library
3. Install the package.

The commands are:

`install.packages("devtools")`
`library(devtools)`
`install_github("CentreForHydrology/CRHMr")`
