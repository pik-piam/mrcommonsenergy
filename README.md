# Fundamental REMIND preprocessing functions

R package **mrenergycore**, version **0.2.0**

   [![R build status](https://github.com/pik-piam/mrenergycore/workflows/check/badge.svg)](https://github.com/pik-piam/mrenergycore/actions) [![codecov](https://codecov.io/gh/pik-piam/mrenergycore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrenergycore) 

## Purpose and Functionality

Provides useful functions and a common structure to all the
    input data required to run the REMIND of model input data.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("mrenergycore")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Falk Benke <benke@pik-potsdam.de>.

## Citation

To cite package **mrenergycore** in publications use:

Benke F (2026). "mrenergycore: Fundamental REMIND preprocessing functions." Version: 0.2.0, <https://github.com/pik-piam/mrenergycore>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrenergycore: Fundamental REMIND preprocessing functions},
  author = {Falk Benke},
  date = {2026-05-20},
  year = {2026},
  url = {https://github.com/pik-piam/mrenergycore},
  note = {Version: 0.2.0},
}
```
