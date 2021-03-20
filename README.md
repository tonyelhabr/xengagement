
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xengagement

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

`{xengagement}` is a package that predicts the amount of Twitter
engagement that [xGPhilosophy](https://twitter.com/xGPhilosophy)
receives with its end-of-match xG summary tweets. The predictions are
shared with automated tweets made by [a
bot](https://twitter.com/punditratio), occasionally including some
manually inserted commentary :grinning:.

Read [this Twitter
thread](https://twitter.com/TonyElHabr/status/1373277253572960258?s=20)
for a high-level discussion of how the package can be used to gain
insights. Also, see [this dashboard](https://xengagement.herokuapp.com/)
using outputs from this package. (Yes, that is a python-based web app
:snake: using outputs from an R package :laughing:)

## Installation

You can install the development version of `{xengagement}` from
[GitHub](https://github.com/) with:

``` r
# install.packages('remotes')
remotes::install_github('tonyelhabr/xengagement')
```
