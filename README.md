# BIKE2.2

## BIKE model version 2.2
Foodborne exposure assessment model for biological and chemical hazards

This is a Bayesian model for estimating foodborne exposures based on dietary data on food consumption frequencies and amounts, and corresponding occurrence data on hazard concentrations (chemical and microbiological) and hazard prevalence (microbiological). The model can be used as a shiny app providing a user interface. Data need to be specified in files that are read by R-code, which then creates the necessary BUGS code of the Bayesian model which is simulated using OpenBUGS in the background. Results are processed in R and presented in the shiny app. Users need to have both R and OpenBUGS installed.

## Instructions
1. First install R, RStudio and OpenBUGS to your computer.

https://posit.co/download/rstudio-desktop/

OpenBUGS is no longer on this site:
https://www.mrc-bsu.cam.ac.uk/software/bugs/openbugs/
But it could be downloaded from here:
https://openbugs.software.informer.com/download/?cf25c9ce#downloading
And the file OpenBUGS323setup.exe is also copied in this BIKE2.1 folder for convenience. 

2. Download the BIKE-code files and unzip in local folder.
3. Open the app.R file in RStudio.
4. Install R-packages as needed.
5. Run the app.

# 
#### Original publication:
Ranta J, Mikkelä A, Suomi J, Tuominen P. BIKE: Dietary Exposure Model for Foodborne Microbiological and Chemical Hazards. Foods. 2021; 10(11):2520. https://doi.org/10.3390/foods10112520 

#### Cite BIKE app as:
Ranta J, Marinova-Todorova M, Mikkelä A, Suomi J, Tuominen P 2023. BIKE foodborne exposure model - A graphical user interface for the Bayesian dietary exposure assessment model for microbiological and chemical hazards (BIKE). Finnish Food Authority, Helsinki, Finland. Available at https://bike-expo-shiny.rahtiapp.fi/

#### Updates:

Food Frequency Questionnaire data (FFQ) is allowed as another data format (previously only food diary format). In FFQ data, average long term daily consumptions of the foods are given for each individual. This implies that there is no model for day-to-day (or serving-to-serving) variation in FFQ-model. Also, FFQ-model has no model for the error or those average daily consumptions. They are taken as exact values. Consequently, the FFQ-model cannot produce exposure distributions for single random exposures, but only for the distribution of average exposures over the population. While this may be relevant for chemical exposure analysis, it is less relevant for microbiological. With chemical exposure analysis, the FFQ-model compares to the results from the food diary data, although they also differ because the models are different: diary model has model parameters for day-to-day variation, and the FFQ model has not. Also, FFQ model assumes the long term average consumptions are known data points, and the diary model will estimate this as an unknown model parameter. Hence, the data, the uncertainty, and the model parameters differ.    
 
