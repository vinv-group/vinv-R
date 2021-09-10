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

Install the vinv-R package.

```R
install_github("vinv-group/vinv-R")
```

## Use

```R
library(vinvSchema)

vinv <- vinvSchema::import('path/to/your/inventory.vinv', pretty = TRUE)

print(vinv$inventory$tree_status)
# list of trees
```

## Example

You can find example applications and scripts in the [./examples](../blob/master/examples)
 folder.

1. [Demo](https://gruenecho.shinyapps.io/vinv-R-example/) application for validation `.vinv` using vinv-R, displaying forest inventory data and do basic analysis.

## Contribute

You like forest, forest inventory and data? Get involved and drive the development, it's free!

1. If you like vinv, start this repository
2. Join the vinv-group's [**Slack** Workspace](https://join.slack.com/t/vinv-space/shared_invite/zt-somm549g-OuITyP9Yuk3o2cadnUPxhA)
3. 💚

## Issues

You can create feature requests or bug reports via an [issue](https://github.com/vinv-group/vinv-R/issues).

We appreciate it. 

## License

[MIT](https://opensource.org/licenses/MIT)