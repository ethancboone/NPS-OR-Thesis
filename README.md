# NPS-OR-Thesis
This is the code for my thesis to fulfill the requirements for my  M.S. Operations Research at the Naval Postgraduate School. The thesis is titled:
> Using Supervised Machine Learning Methods to Identify Factors that Influence the Probability of Future Terrorist Attacks

The code in this repository contains only the information required to provide the predictions for one month ahead. Functions in this repository were used to migrate the thesis findings onto a dashboard to delivered to the project stakeholders. 

## Installation
There are no specific installation instructions for this project. The main executable is the deliverable.R file, which does everything from reading in the data to providing the one month ahead prediction. 

## Project Goal 
This project attempts to predict the number of violent events with fatalities in different areas of the world. Both classification and regression prediction models were developed.

## Methodology
### Data Selection 
There were three main datasources for this project:
- Global Terrorism Database (GTD)
- Armed Conflict and Location Event Data (ACLED)
- Global Database of Events, Language, and Tone (GDELT)

The ACLED dataset was chosen for the thesis due to its weekly updates and overall cleanliness and usability of the data. 

### Data Structure
How the data was going to be structured was the first step in creating the prediction models. Outside of the administrative regions in the ACLED dataset, the next lowest resolution for the data was by country. A higher degree of resolution was desired, which led to the development of a grid structure to overlay on the globe.



The blowout of the grid in the image above shows the contents of each grid. This is what was ultimatly fed into the prediction models. 

### Predictors 
There were several open-source databases considered when collecting predictors. These included the World Bank and the Yale Geographically based Economic data (G-Econ). Feature engineering from the ACLED dataset was also conducted. 

### Predicion Models 
Two main models were used in this thesis:
- Generalized Autoregresive Network (GNAR) Model
- Ensemble Model

The GNAR model showed poor results across several models with varying parameters. This led the pivot to an ensemble model. The ensemble model consisted of two main parts: 
- Time-series forecasting of predictors
- Prediction Analysis
  - Classification 
  - Regression

