ForSysR
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

<!--- README.md is generated from README.Rmd. Please edit that file -->

# prioritizr: <img src="man/figures/logo.png" align="right" style="height:90px!important;" />

# Systematic Conservation Prioritization in R

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-ubuntu](https://img.shields.io/github/workflow/status/prioritizr/prioritizr/Ubuntu/master.svg?label=Ubuntu)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-windows](https://img.shields.io/github/workflow/status/prioritizr/prioritizr/Windows/master.svg?label=Windows)](https://github.com/prioritizr/prioritizr/actions)
[![R-CMD-check-macos](https://img.shields.io/github/workflow/status/prioritizr/prioritizr/macOS/master.svg?label=macOS)](https://github.com/prioritizr/prioritizr/actions)
[![Documentation](https://img.shields.io/github/workflow/status/prioritizr/prioritizr/Documentation/master.svg?label=Documentation)](https://github.com/prioritizr/prioritizr/actions)
[![Coverage-Status](https://codecov.io/github/prioritizr/prioritizr/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/prioritizr?branch=master)
[![CRAN-Status-Badge](http://www.r-pkg.org/badges/version/prioritizr)](https://CRAN.R-project.org/package=prioritizr)
<!-- badges: end -->

The *prioritizr R* package uses mixed integer linear programming (MILP)
techniques to provide a flexible interface for building and solving
conservation planning problems. It supports a broad range of objectives,
constraints, and penalties that can be used to custom-tailor
conservation planning problems to the specific needs of a conservation
planning exercise. Once built, conservation planning problems can be
solved using a variety of commercial and open-source exact algorithm
solvers. In contrast to the algorithms conventionally used to solve
conservation problems, such as heuristics or simulated annealing, the
exact algorithms used here are guaranteed to find optimal solutions.
Furthermore, conservation problems can be constructed to optimize the
spatial allocation of different management actions or zones, meaning
that conservation practitioners can identify solutions that benefit
multiple stakeholders. Finally, this package has the functionality to
read input data formatted for the *Marxan* conservation planning
program, and find much cheaper solutions in a much shorter period of
time than *Marxan*.

## Installation

The latest official version of the *prioritizr R* package can be
installed from the [Comprehensive R Archive Network
(CRAN)](https://cran.r-project.org/) using the following *R* code.

Alternatively, the latest development version can be installed from
[GitHub](https://github.com/prioritizr/prioritizr) using the following
code. Please note that while developmental versions may contain
additional features not present in the official version, they may also
contain coding errors.

## Citation

Please cite the *prioritizr R* package when using it in publications. To
cite the latest official version, please use:

## Usage

Here we will provide a short example showing how the *prioritizr R*
package can be used to build and solve conservation problems. For
brevity, we will use one of the built-in simulated datasets that is
distributed with the package. First, we will load the *prioritizr R*
package.

We will use the sim_pu_polygons object to represent our planning units.
Although the *prioritizr R* can support many different types of planning
unit data, here our planning units are represented as polygons in a
spatial vector format (i.e. `SpatialPolygonsDataFrame`). Each polygon
represents a different planning unit and we have r
length(sim_pu_polygons) planning units in total. The attribute table
associated with this dataset contains information describing the
acquisition cost of each planning (“cost” column), and a value
indicating if the unit is already located in protected area (“locked_in”
column). Let’s explore the planning unit data.

## Learning resources

The [package website](https://prioritizr.net/index.html) contains
information on the *prioritizr R* package. Here you can find
[documentation for every function and built-in
dataset](https://prioritizr.net/reference/index.html), and [news
describing the updates in each package
version](https://prioritizr.net/news/index.html). It also contains the
following articles and tutorials.

-   [**Getting
    started**](https://prioritizr.net/articles/prioritizr.html): Short
    tutorial on using the package.
-   [**Package
    overview**](https://prioritizr.net/articles/package_overview.html):
    Introduction to systematic conservation planning and a comprehensive
    overview of the package.
-   [**Connectivity
    tutorial**](https://prioritizr.net/articles/connectivity_tutorial.html):
    Tutorial on incorporating connectivity into prioritizations.
-   [**Calibrating trade-offs
    tutorial**](https://prioritizr.net/articles/calibrating_trade-offs_tutorial.html):
    Tutorial on running calibration analyses to satisfy multiple
    criteria.
-   [**Management zones
    tutorial**](https://prioritizr.net/articles/management_zones_tutorial.html):
    Tutorial on incorporating multiple management zones and actions into
    prioritizations.
-   [**Gurobi installation
    guide**](https://prioritizr.net/articles/gurobi_installation_guide.html):
    Instructions for installing the *Gurobi* optimization suite for
    generating prioritizations.
-   [**Solver
    benchmarks**](https://prioritizr.net/articles/solver_benchmarks.html):
    Performance comparison of optimization solvers for generating
    prioritizations.
-   [**Publication
    record**](https://prioritizr.net/articles/publication_record.html):
    List of publications that have cited the package.

Additional resources can also be found in [online repositories under the
*prioritizr* organization](https://github.com/prioritizr). These
resources include [slides for talks and seminars about the
package](https://github.com/prioritizr/teaching). Additionally, workshop
materials are available too (e.g., the [Massey University 2021
workshop](https://prioritizr.github.io/massey-workshop/) and the
[PacMara 2019
workshop](https://prioritizr.github.io/PacMara_workshop/)).

## Getting help

If you have any questions about the *prioritizr R* package or
suggestions for improving it, please [post an issue on the code
repository](https://github.com/prioritizr/prioritizr/issues/new).
