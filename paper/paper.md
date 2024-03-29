---
title: 'osiris: An R package to process climate impacts on agricultural yields for
  the Global Change Analysis Model'
tags:
- R
- climate change
- agriculture
- GCAM
date: "13 February 2023"
output: pdf_document
authors:
- name: Hamza Ahsan
  orcid: "0000-0003-1631-1789"
  affiliation: 1
  corresponding: yes
- name: Zarrar Khan
  orcid: "0000-0002-8147-8553"
  affiliation: 1
- name: Abigail Snyder
  orcid: "0000-0002-9034-9948"
  affiliation: 1
- name: Page Kyle
  orcid: "0000-0002-1257-8358"
  affiliation: 1
- name: Chris Vernon
  orcid: "0000-0002-3406-6214"
  affiliation: 2
bibliography: paper.bib
affiliations:
- name: Joint Global Change Research Institute, Pacific Northwest National Laboratory,
    College Park, MD, USA
  index: 1
- name: Pacific Northwest National Laboratory, Richland, WA, USA
  index: 2
---

# Summary

`osiris` is an R package that couples climate change projections with socioeconomic agricultural assumptions used in the Global Change Analysis Model (GCAM). Specifically, this package allows users to examine the implications of changes to regional or global temperature and precipitation for agricultural crop production using GCAM. `osiris` is designed for modularity to allow the user to explore alternative climate projections, crop model emulators, and parameters relating to agricultural yield. The development of this package is part of a broader effort to provide reproducible, updatable data processing for analyzing the impacts of climate [@Jones2023] and socioeconomic change on the coevolution of future water, energy, and land systems using GCAM. Figure 1 shows a generic workflow where climate data are processed by `osiris` and a set of agricultural productivity change files are generated as inputs for GCAM.

![Climate data are processed to generate agricultural productivity change inputs for GCAM.](osiris_workflow.png)

# Statement of need

Changes in temperature, precipitation, length of growing seasons, and *CO~2~* concentration have been shown to have a potential impact on agricultural crop yields in the future [@Asseng2015]. Studies suggest that although the projections of future yields are uncertain, they may affect major agricultural producers in terms of crop production quantity and composition, as well as global trade and market value [@Snyder2020; @Baker2018]. Central to the analysis of the impact of climate on agriculture are tools that can efficiently process large data sets from various sources and generate reproducible results that can be used within a modeling framework.

`osiris` is an R package developed with the aim to process climate impacts on agricultural yields for the GCAM model [@Calvin2019]. This package is a modular data pipeline that can take climate data at high spatiotemporal resolution (e.g., hourly meteorological variables at 12 km resolution over continental United States (CONUS)) and gridded crop yield data from existing emulators [@Franke2020], and transforms the gridded yield time series into yield impacts for the agricultural commodities modeled in GCAM. From a set of gridded crop yield time series (e.g., from the emulator stage), the data processing steps to GCAM input files are relatively agnostic of the source of the gridded yield time series. The gridded yield time series to GCAM input file portion of the data processing pipeline in `osiris` follows the approach taken in past studies examining agricultural impacts in GCAM [@Kyle2014; @Snyder2020; @Graham2020; @Zhao2021]. The inclusion of these operations in the `osiris` package as functions rather than static scripts that must be updated by project or hardcoded for processing of specific yield scenarios in the `gcamdata` system [@Lamberty2019], is an obvious improvement in the adherence of GCAM studies to FAIR data principles (Findable, Accessible, Interoperable, and Reusable), in addition to offering flexibility for future updates to be integrated into GCAM runs more easily.

The unique capabilities that this package offers are:

1.  Processing of CMIP style data to relevant growing season metrics allows users to explore alternative growing season decisions.
2.  The use of available crop yield emulators means users are not restricted to specific scenarios and can still have confidence in projections.
3.  The modular design allows for updates in a more efficient fashion as new tools (updated emulators, etc.) become available.
4.  Designed for interoperability with the GCAM data system without adding additional large data files.

# Functionality

`osiris` features four main functions for processing climate impacts on agricultural yields. The GitHub page for this package includes a detailed [User Guide](https://jgcri.github.io/osiris/articles/vignette.html) with sample code and references for the data used in the functions. The key functions in this package, which are run in series, are:

1.  `calculate_deltas_from_climate`: Processes gridded temperature and precipitation data for each crop and irrigation type. The growing season-average precipitation and temperature data is calculated and smoothed to obtain the long term growing season average values. Then the change relative to some baseline is calculated to obtain changes in long term growing season average values required by the currently included emulator functions.
2.  `grid_to_basin_yield`: Uses a global gridded crop model emulator to generate crop yield which is then aggregated from the grid cell level to the GCAM land unit level. The crop yield is calculated using a polynomial equation [@Franke2020] which is a function of precipitation, temperature, *CO~2~* and nitrogen. The yields are then aggregated to GCAM basins using MIRCA2000 [@Portmann2010] harvested area for weighting.
    -   the option to bypass emulation when a standard scenario's crop yield files are already available as netcdfs (e.g., from ISIMIP-2b) is included as an argument in this function. In this use case, the previous step processing temperature and precipitation data is unnecessary to run.
3.  `yield_to_gcam_basin`: Converts the agricultural crop yield data to yield multipliers, which is the smoothed (rolling average) future yields divided by the baseline period yields. These are then converted to multipliers for each GCAM commodity-irrigation-basin based on GTAP harvested area weights [@Lamberty2019].
4.  `create_AgProdChange_xml`: Applies the multipliers to the GCAM agricultural productivity change assumptions and creates an updated file, which can be used as an input for a GCAM scenario.

# Acknowledgements

This research was supported by the US Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830. The views and opinions expressed in this paper are those of the authors alone.

# References
