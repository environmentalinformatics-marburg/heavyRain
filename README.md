# heavyRain

Download and pre-process (partly) satellite-based [CHIRPS](http://chg.geog.ucsb.edu/data/chirps/) and [TRMM](https://trmm.gsfc.nasa.gov/) rainfall data sets in R.

<hr>

##### Note: 'gdalUtils'

Package **gdalUtils** has been 
[archived](https://cran.r-project.org/web/packages/gdalUtils/index.html) on 
2022-04-18 and is no longer available from the CRAN repository. You can use 

```r
remotes::install_version(
  "gdalUtils"
  , version = "2.0.3.2"
)
```

if encountering package not available warnings during install.
