### neatStats

This R package aims to give a most down-to-earth way possible to get clear, comprehensive, reportable stats out of data from simple (psychological) experiments. If you are looking for advanced and fancy methods, look elsewhere.

One main point is that all functions use data frames with single row per subjects. Not because "it's like that in SPSS", but because that's the clear and logical way, and because all the reshaping/melting/casting/dcasting is a tiresome fuss even for those not new to it, let alone novices, and leads to a lot of totally unnecessary confusion and problems.

### Installation in R

To install the stable version from [CRAN](https://cran.r-project.org/ "The Comprehensive R Archive Network"), just run:

```R
install.packages("neatStats")
```

Then load with: `library("neatStats")`

Alternatively, if you want to install the latest (and potentially unstable) version from this repository:

```
install.packages("devtools") # if "devtools" package is not yet installed
library("devtools")
install_github("gasparl/neatstats")
```

### Usage

See the [EXAMPLE.md](https://github.com/gasparl/neatstats/blob/master/EXAMPLE.md "EXAMPLE.md") for an example pipeline for every step from raw data to reportable statistics.

For details about each function, see [the manual](https://github.com/gasparl/neatstats/blob/master/neatStats.pdf "neatStats.pdf") (or enter `help(xy)` or `?xy` in R for any specific function).

### Support

* If you have any question about statistics, ask a statistician (not me).
* If you run into an error despite carefully following the [documentation](https://github.com/gasparl/neatstats/blob/master/neatStats.pdf "neatStats.pdf"), [open a new issue](https://github.com/gasparl/neatstats/issues "Issues") or [email me](mailto:lkcsgaspar@gmail.com).
* If you have sound reason to believe that some of the presented statistics (or functions) are really not optimal and/or could be improved in some plausible way, [email me](mailto:lkcsgaspar@gmail.com).

[![DOI](https://zenodo.org/badge/187226036.svg)](https://zenodo.org/badge/latestdoi/187226036) ![](http://cranlogs.r-pkg.org/badges/neatStats?color=8585ad "neatStats CRAN download count")
