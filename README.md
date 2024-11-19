# Decoding-Landslide-Hazard-Assessment
Last modified: October 2024.

The codes have been implemented using R 4.4.0.

A Dynamic Landslide Hazard Assessment has been conducted using a Generalized Additive Model (GAM). The results of the GAM are also compared with standard machine learning algorithms (MLs): NNET, RF, LDA, xgBoost, and SVM.

The code was jointly developed by Dewan Haque and Ritu Roy, with collaboration from many others. The GAM code is an update from the study published by Fang et al. (2023) (https://doi.org/10.1016/j.jag.2023.103631), adapted to apply it across settings. The ML code has been developed from scratch.

The required data from intensive fieldwork and satellite image analysis is uploaded here to reproduce the results. Additionally, R Markdown files are provided.
The inputs can be accessed on Zenodo (DOI:) to generate maps from the machine learning models (MLs).

I:

| Type          | Code          | Input                      |
| ------------- |:-------------:| --------------------------:|
| R Script      | GAM_Pyrates   | Geopackage Files           |
| R Script      | SizeFit_Oct24 | Theend17082023 - Copy.txt  |
| R Script      | ML_Pyrates    | Theend17082023csv.csv      | 

II: 

| Type          | Code          | Input                      |
| ------------- |:-------------:| --------------------------:|
| R Markdown    | GAM_LSA       | Geopackage Files           |
| R Markdown    | ML_LSA        | Theend17082023csv.csv      |

III: 

| Type          | Code                  | Input              |
| ------------- |:---------------------:| ------------------:|
| Python Script | Landslide_MonteCarlo  | .ipynb file        |

The code is distributed "as is" WITH NO WARRANTY whatsoever!

