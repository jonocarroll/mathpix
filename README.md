---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


![](./inst/logo_blue.png)

# mathpix

## Installation


```r
devtools::install_github("jonocarroll/mathpix")
```

## Authentication

Obtain a Mathpix API key by e-mailing [support@mathpix.com](mailto:support@mathpix.com) then set the following values in your `~/.Renviron` file


```r
MATHPIX_APP_ID=yourID
MATHPIX_APP_KEY=yourKEY
```

## Usage

If you have an image you would rather properly encode in LaTeX, for example

![](./inst/integral.jpg)

then simply calling


```r
mathpix("./integral.jpg")
```

(with the appropriate path to the file) will insert a LaTeX block into your document which will render what the image represents

```
$$
 \int \frac { 4 x } { \sqrt { x ^ { 2 } + 1 } } d x  
$$
```

which renders to


<img src="https://rawgit.com/jonocarroll/mathpix/master/inst/eq_no_02.png" alt="Equation Fail"height="20">


## API Documentation

Refer to http://docs.mathpix.com/
