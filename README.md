<!-- badges: start -->
[![build](https://github.com/JGCRI/osiris/workflows/build/badge.svg)](https://github.com/JGCRI/osiris/workflows/build/badge.svg?branch=main)
[![test_coverage](https://github.com/JGCRI/osiris/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/osiris/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/JGCRI/osiris/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/osiris) 
[![docs](https://github.com/JGCRI/osiris/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/osiris/actions/workflows/docs.yaml)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05226/status.svg)](https://doi.org/10.21105/joss.05226)
<!-- badges: end -->


<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`osiris` is R package to process climate impacts on agricultural yields for the Global Change Analysis Model (GCAM).

<br>

<p align="center">
<a href="https://jgcri.github.io/osiris/articles/vignette.html" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_user_guide.PNG?raw=true" alt="https://jgcri.github.io/osiris/articles/vignette.html" height="60"/></a> 
</p>

<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>
Ahsan, H., Khan, Z., Snyder, A., Kyle, P., & Vernon, C.R. (2023). osiris: An R package to process climate impacts on agricultural yields for the Global Change Analysis Model. Journal of Open Source Software, 8(85), 5226. https://doi.org/10.21105/joss.05226 

<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/) (Optional)
    
    
2. In R or R studio:

```r
install.packages("devtools")
devtools::install_github("JGCRI/osiris")
```

Additional steps for UBUNTU from a terminal
```
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-dev libproj-dev libavfilter-dev
```

Additional steps for MACOSX from a terminal
```
brew install pkg-config
```


<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Related Publications
<!-- ------------------------>
<!-- ------------------------>

- Müller, C., Franke, J., Jägermeyr, J., Ruane, A.C., Elliott, J., Moyer, E., Heinke, J., Falloon, P.D., Folberth, C., Francois, L. and Hank, T., 2021. Exploring uncertainties in global crop yield projections in a large ensemble of crop models and CMIP5 and CMIP6 climate scenarios. Environmental Research Letters, 16(3), p.034040. https://doi.org/10.1088/1748-9326/abd8fc
- Franke, J.A., Müller, C., Elliott, J., Ruane, A.C., Jägermeyr, J., Snyder, A., Dury, M., Falloon, P.D., Folberth, C., François, L. and Hank, T., 2020. The GGCMI Phase 2 emulators: global gridded crop model responses to changes in CO 2, temperature, water, and nitrogen (version 1.0). Geoscientific Model Development, 13(9), pp.3995-4018. https://doi.org/10.5194/gmd-13-3995-2020
- Snyder, A., Calvin, K.V., Phillips, M. and Ruane, A.C., 2019. A crop yield change emulator for use in GCAM and similar models: Persephone v1. 0. Geoscientific Model Development, 12(4), pp.1319-1350. https://doi.org/10.5194/gmd-12-1319-2019

  
