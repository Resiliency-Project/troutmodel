# Repository for predictive trout model
A repository for all code and necessary data for the predictive model for trout populations in the Au Sable River, part of the Au Sable Resilience Project. The first version of the model was completed in April 2026. The model, an individual-based model adapted from [Flinn et al., 2015](https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/csp2.70234) projects brook and brown trout populations to the year 2080 under 10 unique climate and management scenarios. 

## File structure
There are two folders within this repository: 'Data' and 'Scripts'. The Data file contains all necessary data to run the model. There are three sub-folders: Fish, Habitat, and Spatial. The Scripts folder contains all scripts, and dependent scripts required to run the model. Users only need to access the *model.R* script to run the model. It can be found in *Scripts/Model/model.R* All other scripts for the model are called within *model.R* Ussers can select the scenario they are interested in running by commenting out code lines for scenarios that are not of interest. There are comments throughout all scripts describing what each line of code will execute. The location, name, and description for all scripts can be found in Table 1. The location, name, and description for all data files can be found in Table 2.

**Table 1.** R Script name, file location, and description.
| Script name | File path | Description |
| :--- | :--- | :--- |
|_habitatscoring.R_| Scripts/Inputs/ | Calculates habitat suitability scores for each reach and social ecological unit (SEU) |
| _initialN.R_ | Scripts/Inputs/ | Calculates initial population size for both species, total and by SEU |
| _nochange_scenario.R_ | Scripts/Inputs/ | Loads objects to keep habitat suitability constant through time |
| _recruitment_K.R_ | Scripts/Inputs/ | Calculates necessary recruitment and carrying capacity inputs based changes in habitat suitability information |
| _temp_scenarios.R_ | Scripts/Inputs/ | Loads data for all 3 future temperature scenarios |
| _functions_bothspp.R_ | Scripts/Model/ | Functions for individual life history steps |
| _model_objects.R_ | Scripts/Model/ | Empty objects for the model to fill |
| ***model.R*** | **Scripts/Model/** | **Principal script to run the model. Users only need access this script** |
| _change_visualizations.R_ | Scripts/Outputs/ | Creates tables and plots of change in population overtime by species and SEU |
| _habitatsummarystats.R_ | Scripts/Outputs/ | Summary statistics and visualizations of habitat suitability scores by SEU |
| _initialN_plots.R_ | Scripts/Outputs/ | Visualizations of initial population size |
| _scenario_comparison.R_ | Scripts/Outputs/ | Visualizations comparing results of simulations of future scenarios |



***Table 2.*** *Data file name, file location, and description.*
| File name | File path | Description |
| :--- | :--- | :--- |
| _density_2010presentBKT.csv_ | Data/Fish/Density_data/ | Historic data (2010-present) of calculated brook trout densities in the Au Sable. Rows represent unique density estimations. |
| _density2010presentBNT.csv_ | Data/Fish/Density_data/ | Historic data (2010-present) of calculated brown trout densities in the Au Sable. Rows represent unique density estimations.  |
| _nlslengthparametersBKT.csv_ | Data/Fish/Length_data/ | Bootstrapped parameters for a Von-Bertalanffy growth curve for brook trout in the Au Sable River |
| _nlslengthparametersBNT.csv_ | Data/Fish/Length_data/ | Bootstrapped parameters for a Von-Bertalanffy growth curve for brown trout in the Au Sable River |
| _mortality_rates.csv_ | Data/Fish/ | Mortality rates for brook trout, brown trout, and brook trout in the presence of brown trout from [Zorn et al., 2020](https://onlinelibrary.wiley.com/doi/epdf/10.1111/eff.12563) |
| _habitatdata_clean.csv_ | Data/Habitat/ | Habitat data (gradient, network catchment area, low flow yield, mean July temperature, and median percent wood) for each reach (COMID) in the Au Sable River. Rows represent individual reaches. |
| _lfy_density_projections.RData_ | TBD | .RData file containing 3-dimensional array _dN_lfy_ with projections of trout density (number/acre) for each reach based on its low flow yield. Rows are individual reaches, columns are years in the projection, and matrices are simulations. |
| _lfy_reduced_density_projections.RData_ | TBD |  .RData file containing 3-dimensional array _dN_lfy_ with projections of trout density (number/acre) for each reach based on its reduced low flow yield value in the maximum water withdrawal scenario. Rows are individual reaches, columns are years in the projection, and matrices are simulations. |
| _temp_density_predictions.RData_ | TBD | .RData file containing threee, 3-dimensional arrays _d2.5_, _d3.8_, and _d4.8_. Each array contains  projections of trout density (number/acre) for each reach based on its mean July temperature, for scenarios 2.5 (low temperature change), 3.8 (moderate temperature change), and 4.8 (high temperature change), respectively. Rows are individual reaches, columns are years in the projection, and matrices are simulations.  |
| _temp_projetions.RData_ | TBD | .RData file containing three, 3-dimenaitonal arrays _temps2.5_, _temps3.8_, and _temps4.8_. Each array contains predictions of future July mean temperaure for scenarios 2.5 (low temperature change), 3.8 (moderate temperature change), and 4.8 (high temperature change), respectively. Rows are individual reaches, columns are years in the projection, and matrices are simulations. |
| _wood.clean.csv_ | Data/Habitat/ | Data file with dimension (width, length) and percent area of river with woody debris by reach (COMID). Data collected by Michigan Trout Unlimited.  |
| _area.data.csv_ | Data/Spatial/ | Dimensional information for each reach (COMID) in the Au Sable. *unit* columns indicates which social ecological unit (SEU) a reach is within. |
| _reach.units -.dbf,-.prj,-.shp,-.shx_| Data/Spatial/Untitled | Spatial data files for SEUs |


