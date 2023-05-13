# ```norm_help_func.R```

## Description
A simple function to illustrate the distinction between percentiles, z-scores, and quantiles in a normal distribution.

## Example output

```
norm_help(percentile = 11,
          z = NULL, 
          quantile = NULL,
          mean = 666,
          sd = 13,
          title = "Normal Distribution",
          colour = "#00BFFF",
          save = FALSE
          )
```

<img title="11th Percentile Plot.svg" src="11th Percentile Plot.svg">

## Loading the function
```source("https://raw.githubusercontent.com/STSx666/Dist_Help/main/norm_help_func.R")```

## Arguments

```percentile``` Percentile value displayed on plot.

```z``` Z-score value displayed on plot.

```quantile``` X-axis point-value displayed on plot.

```mean, sd``` Distribution's specified mean and standard deviation. Defaults to 0 and 1, respectively.

```title``` Plot title.

```colour``` Fill colour of percentile shading.

```save``` logical evaluating to ```TRUE``` or ```FALSE``` indicating if plot should be saved.  

## Details

If both ```percentile``` and ```z``` are specified, ```z``` is ignored.  

Plot saves as a SVG file.

The following CRAN packages are necessary to run this function and will be automatically installed and loaded when run:

  - ggplot2
  - svglite
  - ggrepel
