ghgvcr
======

R implementation of the Greenhouse Gas Value Calculator

Citation: Kristina J. Teixeira and Evan H. Delucia 2011. The greenhouse gas value of ecosystems. Global Change Biology. 17(1):425438 doi: 10.1111/j.1365-2486.2010.02220.x

-------

### Inputs

* `inst/config.xml` example input file
* `inst/extdata/ghgvc1.Rdata` all objects used and provided by ghgvc 1.0
* `inputs.Rdata` example of inputs as R objects for ghgvcr example (below)
* `inst/extdata/multipft_input.xml`

### Outputs 

produced by example below:

* `inst/extdata/output.csv`
* `inst/extdata/output.json`

### Installing the ghgvcr package on the PEcAn 1.2.6 VM

The bash and R code snippets below install dependencies, and only need to be run once. 


```r
sudo apt-get install git
sudo apt-get install libcurl4-openssl-dev # dependency of Rcurl, 

git clone https://github.com/dlebauer/pecan.git pecan
git clone https://github.com/dlebauer/ghgvcR.git ghgvcR
R 
```



```r
install.packages(c("devtools", "roxygen2"), repos = "http://cran.us.r-project.org")
library(devtools)
install(ghgvcr)
install(pecan/utils)
```


### Example of how to run the calculator

* This can be run at the command line: `./src/ghgvc_script.R`


```
## Error: there is no package called 'ghgvcr'
```



```r

options(warn = FALSE)
# test('../ghgvcR') example(ghgvcr)


## the following is equivalent to
config.xml <- system.file("config.xml", package = "ghgvcr")
config.list <- xmlToList(xmlParse(config.xml))
```

```
Error: could not find function "xmlToList"
```

```r
ecosystem_data <- config.list$ecosystem_data
```

```
Error: object 'config.list' not found
```

```r

x <- ghgvcr::ghgvc(options = config.list$options, ecosystem_data = config.list$ecosystem_data)
```

```
Error: there is no package called 'ghgvcr'
```

```r


writeLines(x, "inst/extdata/output.json")
```

```
Error: object 'x' not found
```

```r
write.csv(as.data.frame(fromJSON(x)), "inst/extdata/output.csv")
```

```
Error: could not find function "fromJSON"
```



```r
multisite_config.xml <- system.file("multisite_config.xml", package = "ghgvcr")
multipft_config.list <- xmlToList(xmlParse(multipft_config.xml))
```

```
Error: could not find function "xmlToList"
```

```r

x2 <- ghgvcr::ghgvc2(multipft_config.list)
```

```
Error: there is no package called 'ghgvcr'
```

```r

writeLines(x2, "inst/extdata/multipft_output.json")
```

```
Error: object 'x2' not found
```

```r
write.csv(as.data.frame(fromJSON(x2)), "inst/extdata/multipft_output.csv")
```

```
Error: could not find function "fromJSON"
```


### Plots:


```r
library(ggplot2)
# number of ecosystems:
n.ecosystems <- length(names(ecosystem_data))
```

```
Error: object 'ecosystem_data' not found
```

```r
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
```

```
Error: object 'n.ecosystems' not found
```

```r

# identify cols with numbers
result.num <- suppressWarnings(as.numeric(result))
```

```
Error: object 'result' not found
```

```r
num.logical <- !(is.na(result.num) | result.num == -9999)
```

```
Error: object 'result.num' not found
```

```r
result.df <- result.df[, !(result.num == -9999 | is.na(result.num))]
```

```
Error: object 'result.df' not found
```

```r

# transpose data.frame for plotting:
result.tdf <- cbind(variable = names(result.df), as.data.frame(t(result.df)))
```

```
Error: object 'result.df' not found
```

```r

forcings.index <- grepl("F", names(result.df))
```

```
Error: object 'result.df' not found
```

```r
forcings.names <- names(result.df)[forcings.index]
```

```
Error: object 'result.df' not found
```

```r


forcings <- result.tdf[forcings.index, ]
```

```
Error: object 'result.tdf' not found
```

```r
forcings.long <- melt(forcings, id.vars = "variable")
```

```
Error: could not find function "melt"
```

```r
colnames(forcings.long) <- c("variable", "ecosystem", "value")
```

```
Error: object 'forcings.long' not found
```

```r

```




```r
ggplot(data = forcings.long, aes(x = variable, y = value, fill = ecosystem)) + 
    geom_bar(position = "dodge", stat = "identity") + ggtitle(label = "Example plot: values of F for two ecosystems") + 
    xlab("Variable") + ylab("Units of F") + coord_flip()
```

```
## Error: object 'forcings.long' not found
```

