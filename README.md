# finto

[![R-CMD-check](https://github.com/ake123/finto/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rOpenGov/finto/actions/workflows/check-standard.yaml)
[![issues](https://img.shields.io/github/issues/ake123/finto)](https://github.com/ake123/finto/issues)
[![pulls](https://img.shields.io/github/issues-pr/ake123/finto)](https://github.com/ake123/finto/pulls)


The `finto` package provides tools to access the serivice for interoperable thesauri, ontologies and classification schemes for different subject areas. 

## Installation instructions


The devel version of finto can be installed from GitHub as follows:

``` r
# Install finto if not already installed
if (!requireNamespace("finto", quietly = TRUE)) {
  remotes::install_github("rOpenGov/finto")
}
```

``` r
remotes::install_github("rOpenGov/finto")
```

## Example
The basic functionality of 'finto' can be explored as follows:


``` r
# Load the package
library(finto)
# Perform a simple search and print a table

concepts <- search_concepts("sibelius")
head(concepts)
```
| uri                                    | type                                                   | prefLabel        | altLabel | lang | vocab |
|----------------------------------------|--------------------------------------------------------|------------------|----------|------|-------|
| [http://www.yso.fi/onto/koko/p91024](http://www.yso.fi/onto/koko/p91024) | skos:Concept, http://www.yso.fi/onto/mero-meta/Concept | Sibelius (junat) | NA       | fi   | koko  |
| [http://www.yso.fi/onto/mero/m11156](http://www.yso.fi/onto/mero/m11156) | skos:Concept, http://www.yso.fi/onto/mero-meta/Concept | Sibelius (junat) | NA       | fi   | liiko |
     


## Contribute

Contributions are very welcome:

- [Use issue tracker](https://github.com/ropengov/finto/issues) for feedback and bug reports.
- [Send pull requests](https://github.com/ropengov/finto/)
- [Star us on the Github page](https://github.com/ropengov/finto/)

### Disclaimer

This package is in no way officially related to or endorsed by Finto.

When using metadata retrieved from Finto database in your work, please
indicate that the metadata source is Finto. If your re-use involves some
kind of modification to data or text, please state this clearly to the
end user. See Finto policy on [copyright and free re-use of data
](https://api.finto.fi/) for more detailed information and certain exceptions.
