ghgvcR
======

R implementation of the Greenhouse Gas Value Calculator

Citation: Kristina J. Teixeira and Evan H. Delucia 2011. The greenhouse gas value of ecosystems. Global Change Biology. 17(1):425–438 doi: 10.1111/j.1365-2486.2010.02220.x

-------

### Inputs

* `inst/config.xml` example input file
* `inst/extdata/ghgvc1.Rdata` all objects used and provided by ghgvc 1.0
* `inputs.Rdata` example of inputs as R objects for ghgvcR example (below)

### Outputs 

produced by example below:

* `inst/extdata/output.csv`
* `inst/extdata/output.json`

### Installing the ghgvcR package on the PEcAn 1.2.6 VM

```{bash eval = false}
sudo apt-get install git
sudo apt-get install libcurl4-openssl-dev # dependency of Rcurl, 

git clone https://github.com/dlebauer/ghgvcR.git ghgvcR
R 
```


```r
install.packages(c("devtools", "roxygen2"), repos = "http://cran.us.r-project.org")
```



now check it out:



### Plots:


```r
library(ggplot2)
# number of ecosystems:
n.ecosystems <- length(names(ecosystem_data))
for (i in 1:n.ecosystems) {
    result <- ecosystem_data[[i]]
    ecosystem.name <- result$name
    if (i == 1) {
        result.df <- as.data.frame(result)
        
    } else {
        result.df <- rbind(result.df, as.data.frame(result))
    }
    rownames(result.df)[i] <- gsub(" ", "", ecosystem.name)
}

# identify cols with numbers
result.num <- suppressWarnings(as.numeric(result))
num.logical <- !(is.na(result.num) | result.num == -9999)
result.df <- result.df[, !(result.num == -9999 | is.na(result.num))]

# transpose data.frame for plotting:
result.tdf <- cbind(variable = names(result.df), as.data.frame(t(result.df)))

forcings.index <- grepl("F", names(result.df))
forcings.names <- names(result.df)[forcings.index]

require(reshape)
```

```
## Loading required package: reshape
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
```

```r
forcings <- result.tdf[forcings.index, ]
forcings.long <- melt(forcings, id.vars = "variable")
colnames(forcings.long) <- c("variable", "ecosystem", "value")

```




```r
ggplot(data = forcings.long, aes(x = factor(variable), y = value, fill = ecosystem)) + 
    geom_bar(position = "dodge") + ggtitle(label = "Example plot: values of F for two ecosystems") + 
    xlab("Variable") + ylab("Units of F") + coord_flip()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

