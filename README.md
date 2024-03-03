# tlbparam
Generate Summary Information for Thermal Liquid Biopsy (tlb) Data

# Last Update
February 2024 - updated and improved several parameters included in the analysis

## Contributors

Dr. Robert Buscaglia (robert.buscaglia@nau.edu)*

Avery Bell

Scooter Nowak (scooternowak@gmail.com)

Dr. Nichola Garbett (nichola.garbett@louisville.edu)

Please contact corresponding author* for any questions regarding this package.

## Primary Public Commit

Package was made public for download December 14th, 2022. Package contains two primary functions:

**load_thermograms()** - generates properly formatted data.frame for use by clean_thermograms()

**clean_thermograms()** - generates thermogram parameters for all provided samples.

Several other depedent functions are present with vignette descriptions.

### Thermogram Parameters and Descriptions

The thermogram parameters generated by clean_thermograms() are:

**$Peak/; 1$**: Height of peak corresponding to Peak 1 temperature region (60 - 66 C)

**$Peak/; 2$**: Height of peak corresponding to Peak 2 temperature region (67 - 73 C)

**$Peak/; 3$**: Height of peak corresponding to Peak 3 temperature region (73 - 81 C)

**$T_{Peak} 1$**: Temperature of peak corresponding to Peak 1.

**TPeak 2**: Temperature of peak corresponding to Peak 2.

**TPeak 3**: Temperature of peak corresponding to Peak 3.

**Peak 1/Peak 2**: Peak 1 to Peak 2 ratio.

**Peak 1/Peak 3**: Peak 1 to Peak 3 ratio.

**Peak 2/Peak 3**: Peak 2 to Peak 3 ratio.

**V1.2**: Valley (minimum amplitutde) between Peak 1 and Peak 2.

**TV1.2**: Temperature of Valley between Peak 1 and Peak 2.

**V1.2/Peak 1**: Ratio of Valley between Peak 1 and Peak 2 to the amplitude of Peak 1.

**V1.2/Peak 2**: Ratio of Valley between Peak 1 and Peak 2 to the amplitude of Peak 2.

**V1.2/Peak 3**: Ratio of Valley between Peak 1 and Peak 2 to the amplitude of Peak 3.

**Max**: Maximum observed excess heat capacity

**Tmax**: Temperature corresponding to maximum height.

**TFM**: Temperature corresponding to the first moment.

**Width**: Full width at half max

**Area**: Total area under the thermogram signature

Metrics below are available but were not used in most recent (Myocardial Injury 2024) work.

**$min**: Minimum observed excess heat capacity.

**$tmin**: Temperature corresponding to minimum observed excess heat capacity.

**$median**: Median observed excess heat capacity

**$tpeakf**: Temperature of peak corresponding to Fibrinongen temperature region (47 - 60 C)

**$peakf**: Height of peak corresponding to Fibrinongen temperature region (47 - 60 C)


## Melanoma Data Example

To obtain the thermogram metrics for the Melanoma data presented in "Plasma Thermogram Parameters Differentiate Status and Overall Survival of Melanoma Patients. 2023. Current Oncology, 30(7), 6079-6096.", please download the required packages files and run the R script 'Melanoma_Parameters_Example.R'. Metrics have been updated since completion of this manuscript.

## Publications

The tlbparam package has been used in the following publications. Package has been updated for current submission of Myocardial Injury manuscript.

  1. Characterization of Myocardial Injury Phenotype by Thermal Liquid Biopsy. 2024. Frontiers in Cardiology. Submitted.

  2. Plasma Thermogram Parameters Differentiate Status and Overall Survival of Melanoma Patients. 2023. Current Oncology, 30(7), 6079-6096.

### Package Limitations and Future Updates

Function manuals and parameter descriptions within R are out dated.  Functionality is moving toward online Python-based web application for more flexible usage.  Please contact corresponding author for any information regarding current version metric formulas.

Some dependencies still exists for data collected specific to the Garbett laboratory.  Please email any questions or suggestions to the corresponding author.
