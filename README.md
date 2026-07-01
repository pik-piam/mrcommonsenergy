# Preprocessing functions for REMIND and other energy models (no landuse data)

R package **mrcommonsenergy**, version **0.3.4**

   [![R build status](https://github.com/pik-piam/mrcommonsenergy/workflows/check/badge.svg)](https://github.com/pik-piam/mrcommonsenergy/actions) [![codecov](https://codecov.io/gh/pik-piam/mrcommonsenergy/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrcommonsenergy) [![r-universe](https://pik-piam.r-universe.dev/badges/mrcommonsenergy)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Preprocessing functions for REMIND and other energy models (buildings, transport, industry) not using landuse data.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("mrcommonsenergy")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Falk Benke <benke@pik-potsdam.de>.

## Citation

To cite package **mrcommonsenergy** in publications use:

Benke F (2026). "mrcommonsenergy: Preprocessing functions for REMIND and other energy models (no landuse data)." Version: 0.3.4, <https://github.com/pik-piam/mrcommonsenergy>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrcommonsenergy: Preprocessing functions for REMIND and other energy models (no landuse data)},
  author = {Falk Benke},
  date = {2026-06-29},
  year = {2026},
  url = {https://github.com/pik-piam/mrcommonsenergy},
  note = {Version: 0.3.4},
}
```
