viewinteract: an R package for generating rules capturing cross-view interactions
=================================================================================

The main function is `get_rules()`. It takes a list of views as input; it outputs a list containing two matrices of 0-1 coded indicator variables (`rules_main`, which contains single-condition rules, that is: main effects; `rules_interact`, which contains two-condition rules for capturing cross-view interactions) and `dictionary`, which provides the conditions of the rules in the two matrices.

Example: Generating rules for two views of airquality data
----------------------------------------------------------

We first create a list of two views of the same observations from the airquality data:

``` r
viewA <- na.omit(airquality)[-1]
viewB <- log(viewA)
views <- list(viewA, viewB)
```

As the package currently supports only binary factor response variables, we create such a response variables. Note that the first level should correspond to the target class:

``` r
y <- na.omit(airquality)$Ozone
y <- factor(y < median(y))
levels(y) <- c("high", "low")
```

Now we use the `get_rules()` function to generate two new views: one containing main effects, one containing cross-view interactions. Note that the rule generation process involves random sampling of views and observations, so we first have to set the random seed:

``` r
library("viewinteract")
set.seed(42)
new_views <- get_rules(views = views, y = y)
```

The result is a list with three objects: matrices `rules_main` and `ruleS_interact` and a dataframe `dictionary`. The two matrices contain 0-1 coded indicator variables for different sets of rules, for the same number of observations:

``` r
dim(new_views$rules_main)
#> [1] 111  78
dim(new_views$rules_interact)
#> [1] 111 227
```

For example, the first three single-condition rules for the first six observations look as follows:

``` r
new_views$rules_main[1:6, 1:3]
#>      rule1 rule2 rule9
#> [1,]     1     0     1
#> [2,]     1     1     1
#> [3,]     1     1     1
#> [4,]     1     0     1
#> [5,]     1     0     1
#> [6,]     1     1     1
```

Similarly, the first three two-condition rules for the first six observations look as follows:

``` r
new_views$rules_interact[1:6, 1:3]
#>      rule4 rule5 rule11
#> [1,]     0     1      1
#> [2,]     1     0      1
#> [3,]     1     0      1
#> [4,]     0     1      1
#> [5,]     0     1      1
#> [6,]     1     0      1
```

The `dictionary` shows the conditions for each rule in `rules_main` and `rules_interact`:

``` r
head(new_views$dictionary, n = 8L)
#>      rule                description    ruletype view1 view2
#> 1   rule1                 Temp< 82.5        main     1     2
#> 2   rule2              Solar.R< 5.03        main     1     2
#> 4   rule4 Temp< 82.5 & Solar.R< 5.03 interaction     1     2
#> 5   rule5 Temp< 82.5 & Solar.R>=5.03 interaction     1     2
#> 9   rule9                Temp< 4.363        main     1     2
#> 11 rule11   Temp< 82.5 & Temp< 4.363 interaction     1     2
#> 12 rule12   Temp< 82.5 & Temp>=4.363 interaction     1     2
#> 15 rule15                 Temp< 77.5        main     1     2
```

Work to do
----------

-   When gradient boosting is not employed (i.e., `learnrate = 0`), perfectly pure nodes will yield an error
-   Rules that are perfectly negatively correlated (i.e., yield the exact opposite 0-1 pattern) should be identified and eliminated.
