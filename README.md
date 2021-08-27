# vinv-R

This package imports, validates and makes `.vinv` (forestry virtual inventory) files readable.

## Install

Make shure devtools package is installed.

```R
install.packages("devtools")
```

Load the devtools package.

```R
library(devtools)
```

Install the vinvSchema package.

```R
install_github("vinv-group/vinv-R")
```

## Use

```R
library(vinvSchema)

vinv <- vinvSchema::import('path/to/your/inventory.vinv', pretty = TRUE)

print(vinv$inventory$tree_status) # outputs a list of trees
```
