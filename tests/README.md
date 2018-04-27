Tests and Coverage
================
27 April, 2018 22:06:07

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by [covrpage](https://github.com/yonicd/covrpage).

Coverage
--------

Coverage summary is created using the [covr](https://github.com/r-lib/covr) package.

| Object                        | Coverage (%) |
|:------------------------------|:------------:|
| mathpix                       |     9.68     |
| [R/connect.R](../R/connect.R) |     9.68     |

<br>

Unit Tests
----------

Unit Test summary is created using the [testthat](https://github.com/r-lib/testthat) package.

| file                               |    n|   time|  error|  failed|  skipped|  warning|
|:-----------------------------------|----:|------:|------:|-------:|--------:|--------:|
| [test\_api.R](testthat/test_api.R) |    5|  4.394|      0|       0|        0|        0|

| file                               | test                                        | context         | status |    n|   time|
|:-----------------------------------|:--------------------------------------------|:----------------|:-------|----:|------:|
| [test\_api.R](testthat/test_api.R) | eq1 returns correct LaTeX                   | Connect to API  | PASS   |    1|  1.066|
| [test\_api.R](testthat/test_api.R) | Travis successfully uses API key            | Connect to API  | PASS   |    1|  0.792|
| [test\_api.R](testthat/test_api.R) | Retrying image processing can be successful | Connect to API  | PASS   |    2|  2.533|
| [test\_api.R](testthat/test_api.R) | Missing image produces an error             | File processing | PASS   |    1|  0.003|
