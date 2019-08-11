
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wordstonumbers ![codecov.io](https://codecov.io/gh/fsingletonthorn/words_to_numbers/branch/master/graph/badge.svg) ![travis-ci.org](https://travis-ci.org/fsingletonthorn/words_to_numbers.svg?branch=master)

wordstonumbers is an R package for text analysis pre-processing that
transforms numbers written in words to numerics (e.g., “one hundred and
twenty three thousand” to “123000”). Works on numbers up to the
decillions.

## Installation

You can install wordstonumbers from github with:

``` r
# install.packages("devtools")
devtools::install_github("fsingletonthorn/words_to_numbers")
```

## Example

The words\_to\_numbers function deals with the most common ways that
numbers are reported as words. For example:

``` r
library(wordstonumbers)

words_to_numbers("ninety-nine red balloons")
#> [1] "99 red balloons"
```

``` r

words_to_numbers("The answer is forty-two")
#> [1] "The answer is 42"
```

``` r

words_to_numbers("The PRQ is a twelve-item, four-point Likert scale (from one = Never to four = Very Often) with three sub-scores.")
#> [1] "The PRQ is a 12-item, 4-point Likert scale (from 1 = Never to 4 = Very Often) with 3 sub-scores."
```

``` r
words_to_numbers("The Library of Babel (by Jorge Luis Borges) describes a library that contains all possible four-hundred and ten page books made with a character set of twenty five characters (twenty two letters, as well as spaces, periods, and commas), with eighty lines per book and forty characters per line.")
#> [1] "The Library of Babel (by Jorge Luis Borges) describes a library that contains all possible 410 page books made with a character set of 25 characters (22 letters, as well as spaces, periods, and commas), with 80 lines per book and 40 characters per line."
```

``` r
words_to_numbers("three hundred billion, one hundred and 79 cats")
#> [1] "300000000179 cats"
```

This function attempts to break numbers apart ‘intelligently’, guessing
which values are likely to represent separate numbers, e.g.:

``` r
words_to_numbers("one two three four")
#> [1] "1 2 3 4"
```

``` r
words_to_numbers("one hundred and seventeen one hundred")
#> [1] "117 100"
```

## Limitations

However, not all ways that numbers can reported in text are correctly
transformed at the moment.

For example, this function does not currently correctly process
magnitudes preceded by decimals, e.g.:

``` r
words_to_numbers("1.6 Billion")
#> [1] "1.6000000000"
```

It also only breaks apart values after decimals using the same rules as
for normal numbers, meaning that if people list numbers after decimals
they will be read as separate values.

``` r
words_to_numbers('three point one four one five nine two six')
#> [1] "3.1 4 1 5 9 2 6"
```

It also does not yet recognise that certain numbers are likely to
represent years.

``` r
words_to_numbers('nineteen twenty')
#> [1] "19 20"
```
