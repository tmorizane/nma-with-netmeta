## R scripts with netmeta, tcltk2
netmeta is a R package for network meta-analysis with frequentist method developed by Balduzzi S, Rücker G, Nikolakopoulou A,et al. 
Balduzzi S, Rücker G, Nikolakopoulou A, Papakonstantinou T, Salanti G, Efthimiou O, Schwarzer G (2023). “netmeta: An R Package for Network Meta-Analysis Using Frequentist Methods.” Journal of Statistical Software, 106(2), 1–40. doi:10.18637/jss.v106.i02.
https://cran.r-project.org/web/packages/netmeta/

tcltk2 is a R package to provide interfaces for opening and saving files. https://cran.r-project.org/web/packages/tcltk2/
Grosjean P (2022). SciViews-R: A GUI API for R. UMONS, MONS, Belgium. http://www.sciviews.org/SciViews-R.

nma-with-netmeta is R scripts doing network meta-analysis using these packages. Thus, you need to install these two R packages before using to-nma-with-netmeta.R or nma-with-netmeta.R.

## Data entry
You have data for network meta-analyais in a format of one pair in one line in Excel. Please use netmeta-data-templates.xlsx to prepare your data by replacing the data. There are a sheet for binary outcome, a sheet for continuous outcome, and a sheet for hazard ration.

## Do network meta-analysis
Start R in your PC. Open the Excel file and select cell range, and  copy (ctr + C).  Then in R run to-nma-with-netmeta.R, which is only one line of scripts, one for Windows and another is for Mac. The nma-with-netmeta.R is downloaded from a server and automatically run in your PC with source function.
The data is transfered via clipboard to R.

You can run nma-with-netmeta.R locally, too. Then remove # at dat = read.delim(.....).

## Outputs
Outputs are network graphs, a forest plot, the results of node-splitting for direct, indirect, and network estimate comparisons, rankograms, SUCRA, and a table of pairwise interventions with direct, indirect, network estimates, and some more.
Net heat plots are not always produced. If the number of studies is too small or other conditions are not met, net heat plots are not available.
