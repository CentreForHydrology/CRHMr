# CRHMr
This package is pronounced 'Krimmer' in honour of my wife's family.
The CRHMr functions are used with the Cold Regions Hydrological Modelling (CRHM) platform, which is was developed at the Centre for Hydrology at the University of Saskatchewan. CRHM is available from http://www.usask.ca/hydrology/CRHM.php.

Functions are provided to create time series of forcing meteorological data, read and aggregate CRHM output, set model run parameters, run the CRHM program, and to post-process the output. This package works on Windows, OSX or Linux, although running CRHM on Linux or OSX requires the installation of WINE, which is available from https://www.winehq.org/.

CRHMr also contains functions for gap removal, infilling and imputation of time series data, which may be useful for other models. Functions are also provided to deaccumulate, plot and correct weighing gauge precipitation data.

##Installation

You can download the complete package, as well as the manual .pdf by clicking on **releases**. However, you can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" - you only have to do this once. Note that this will also install several dependancies
2. Load the devtools library
3. Install the package.

The commands are:

	install.packages("devtools")
	library(devtools)
	install_github("CentreForHydrology/CRHMr")
