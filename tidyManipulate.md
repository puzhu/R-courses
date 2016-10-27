# R Notebook: Week 2
Hari Subhash  
`r Sys.Date()`  


<label for="tufte-mn-" class="margin-toggle">&#8853;</label><input type="checkbox" id="tufte-mn-" class="margin-toggle"><span class="marginnote">The world of R till recently, was a bit like the Wild West. There was a steady rush of R pioneers who developed packages that extended R's capacity. However, as the number of packages increased, the lack of an overarching guiding philosophy led to inconsistencies in design and syntax across packages that multiplied the time and effort users had to spend to learn each new package with its own set of rules and principles.</span>

<span class="newthought">The tidyverse</span> is an evolving attempt to create an overarching philosophy to guide the development of new packages in R. It is a suite of 18 packages (listed b# invalidate cache when the tufte version changes
elow) that have been developed to work in harmony with one another and follow a tidy philosophy. These packages cover about 95% of R functionality and more packages continue to be added to it. ^[[Tidyverse guiding principles](https://mran.microsoft.com/web/packages/tidyverse/vignettes/manifesto.html)] 

* Core packages: ggplot2, dplyr, tidyr, readr, purrr, tibble
* Specialized data manipulation: hms, stringr, lubridate, forcats
* Data import: DBI, haven, httr, jsonlite, readxl, rvest, xml2
* Modeling: modelr, broom

# How do we get started?
First thing to do is to get these packages installed. Luckily, we don't need to install them individually. 'tidyverse' is also a package that installs all of them in one shot. So go into your console and type in `install.packages(tidyverse)`. This will install all the packages in the tidyverse along with their dependencies. `library(tidyverse)` will load only the core packages in the tidyverse <label for="tufte-mn-" class="margin-toggle">&#8853;</label><input type="checkbox" id="tufte-mn-" class="margin-toggle"><span class="marginnote">i.e ggplot2, dplyr, tidyr, readr, purrr, tibble</span> and is an easy way to start all projects.

# Piping
`%>%` is a core feature of the tidyverse. It allows us to break complex bits of code into smaller bits that are easier to read and debug. It also comes with the added benefit of not storing too many intermediate variables. Say for instance, we want to compare to find the square root of the first 200 odd numbers. ^[[More on Piping](http://r4ds.had.co.nz/pipes.html)] 


```r
# Base version
sqrt(sum(seq(from = 1, to = 200, by = 2)))
```

```
## [1] 100
```

```r
# dplyr verstion
library(tidyverse)
seq(from = 1, to = 200, by = 2) %>%
        sum() %>%
        sqrt()
```

```
## [1] 100
```

The dplyr version is arguably easier to maintain and read.

# Data Manipulation with dplyr
dplyr is a package that provides 5 verbs and several other utility functions to perform common data manipulation tasks. First we will go through verbs individually using the flights dataset.






