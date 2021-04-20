
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Riex

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/TheEliteAnalyst/Riex.svg?branch=master)](https://travis-ci.com/TheEliteAnalyst/Riex)
<!-- badges: end -->

The main goal of ‘Riex’ is to efficiently retrieve financial and market
data using ‘IEX Cloud API’. In addition, provide robust tool to:

-   Enable data analysis and visualization
-   Monitor Account usage and alerts

Please make sure to review and acknowledge [IEX Terms of
Use](https://iexcloud.io/terms/) before using Riex.

Effective June 1st, 2019, Subscription will be required to access third
party data.

For Subscriptions details, visit [IEX - Flexible, scalable
pricing](https://iexcloud.io/pricing/).

-   Multiple tiers are available to users depending on their
    requirements with capability to upgrade
-   Usage is measured based on message counts which depends on API Call
    and associated weight
-   Example [Company - API Call](https://iexcloud.io/docs/api/#company)
    has a weight of 1 for each Symbol

Additional details about usage calculations available in [Data Weight -
section](https://iexcloud.io/docs/api/#how-credits-work) Best practice
about storing and sharing [Private & Publice Secret
Key](https://iexcloud.io/docs/api/#authentication)

## Installation

You can install the released version of Riex from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Riex")
```

## Example

This is a basic example which shows you how to retrieve Company info via
‘IEX Cloud API’:

iex.company(x, iex\_sk) requires 2 values:

-   x : A valid IEX Stock Symbol
-   iex\_sk : ‘IEX Cloud API’ Secret Key. It is available to use via
    Account Console.

Keep your secret token safe. Your secret token can make any API call on
behalf of your account, including changes that may impact billing such
as enabling pay-as-you-go charges

``` r
#Load Riex Package
library(Riex)
sk <- "[SECRET TOKEN]" 
x <- "TSLA"
TSLA_Co <- iex.company(x, sk)
TSLA_Co
```

For more details and additional examples, please review `Riex` vignette.
