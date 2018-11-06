viewinteract: an R package for generating rules capturing cross-view interactions
=================================================================================

The main function is `get_rules()`. It takes a list of views and a response variable as input; it outputs a list containing two lists: `rules` and `dictionaries`. The former contains three matrices of 0-1 coded indicator variables: `view1` and `view2`, which contain single-condition rules, based on the input variables from the respective views. If the views were supplied as a named list, these matrices will be names after the views they were generated from. Furthermore, `rules` contains a matrix `interact`, which contains rules with two conditions, each from a different view, thereby accomodating cross-view interactions. The `dictionaries` list contains objects with the same name and provides the conditions and further information on each of the rules in the `rules` matrices.

Example: Generating rules for two views of airquality data
----------------------------------------------------------

We first create a list of two views of the same observations from the airquality data:

``` r
viewA <- na.omit(airquality)[-1]
viewB <- cbind(log(viewA), apply(scale(viewA) > 0, 2, as.numeric))
names(viewB) <- paste0(names(viewA), rep(1:2, each = ncol(viewA)))
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

The result is a list with two objects: `rules` and `dictionaries`. The former contains three matrices, the first two containing main-effects rules, the last one (`interact`) containing cross-view interactions. The matrices contain 0-1 coded indicator variables for every rule:

``` r
dim(new_views$rules$view1)
#> [1] 111  40
dim(new_views$rules$view2)
#> [1] 111  39
dim(new_views$rules$interact)
#> [1] 111 223
```

For example, the first three single-condition rules from the first view for the first six observations look as follows:

``` r
new_views$rules$view1[1:6, 1:3]
#>      rule52 rule65 rule79
#> [1,]      1      1      1
#> [2,]      1      0      0
#> [3,]      1      0      0
#> [4,]      1      1      1
#> [5,]      1      1      1
#> [6,]      1      1      1
```

Similarly, the first three single-condition rules for the second view:

``` r
new_views$rules$view2[1:6, 1:3]
#>      rule1 rule2 rule9
#> [1,]     1     0     1
#> [2,]     1     1     1
#> [3,]     1     1     1
#> [4,]     1     0     1
#> [5,]     1     0     1
#> [6,]     1     1     1
```

Finally, the first three two-condition rules for the first six observations look as follows:

``` r
new_views$rules$interact[1:6, 1:3]
#>      rule4 rule5 rule11
#> [1,]     0     1      1
#> [2,]     1     0      1
#> [3,]     1     0      1
#> [4,]     0     1      1
#> [5,]     0     1      1
#> [6,]     1     0      1
```

The `dictionaries` provide the conditions for each rule in `rules`:

``` r
head(new_views$dictionaries$view1, n = 3L)
#>      rule description ruletype view1 view2
#> 52 rule52  Temp< 89.5     main     2     1
#> 65 rule65  Temp< 67.5     main     2     1
#> 79 rule79  Temp< 71.5     main     2     1
head(new_views$dictionaries$view2, n = 3L)
#>    rule    description ruletype view1 view2
#> 1 rule1     Temp< 82.5     main     1     2
#> 2 rule2 Solar.R1< 5.03     main     1     2
#> 9 rule9   Temp1< 4.363     main     1     2
head(new_views$dictionaries$interact, n = 3L)
#>      rule                 description    ruletype view1 view2
#> 4   rule4 Temp< 82.5 & Solar.R1< 5.03 interaction     1     2
#> 5   rule5 Temp< 82.5 & Solar.R1>=5.03 interaction     1     2
#> 11 rule11   Temp< 82.5 & Temp1< 4.363 interaction     1     2
```

Work to do
----------

-   When gradient boosting is not employed (i.e., `learnrate = 0`), perfectly pure nodes will yield an error
-   Rules that are perfectly negatively correlated (i.e., yield the exact opposite 0-1 pattern) should be identified and eliminated.
-   Duplicate rules are identified currently within views with main and interaction rules separately. Should we keep it like that? Or should a main effect rule be preferred over a perfectly correlated interaction effect rule?
