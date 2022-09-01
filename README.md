# TFM-Stochastic-simulation-of-successive-waves-of-COVID-19-in-the-province-of-Barcelona
The pandemic outbreak has been one of the most unexpected events of recent times. Its study and understanding are essential in order to provide healthcare organizations with the necessary tools to make decisions in future pandemics.

This work is dedicated to the study of the evolution of the pandemic in the autonomous community of Catalunya. It focuses on the analysis of mobility data collected from mobile phone users, the study of the evolution of the real number of confirmed positive cases in Catalunya, and the elaboration of a detailed description of the full catalan population using the latest available Census from 2011 with the granularity of "Arees Basiques de Salut" (ABS). Mobility data and other sets of data are integrated to provide the best possible description of the full population.

The elaboration of this detailed model of the population is needed for an accurate simulation of the propagation of the first five pandemic waves in Catalunya.

This project has been created to upload the code used for the realisation of this work.

The code is organised by JupyterNotebooks and R scripts.

The following list is a description of each file:

* CreateWorld-AO-NewCensus2011CAT_TFM.ipynb: Python notebook file reproduces the full population of Catalunya. Additionally, it contains the validation of the disintegration from aggregated ABS to individual ABS, the validation of the assignment of workers to a working region.
* PCRdistributionTFM.ipynb: Python notebook file all the analysis from Padris database, this is: real evolution of first five waves of confirmed positive cases in Catalunya, analysis of simulated data for first two waves in the province of Barcelona. and real evolution of confirmed positive cases for the first two waves in the province of Barcelona.
* abs_list_v2.R: R script produces the list of destination ABS for each source ABS, which is used in the file CreateWorld-AO-NewCensus2011CAT_TFM.ipynb
* distancedistributionanalysis.ipynb: This file produces the comparison between the ABS of reference from the algorithm created in CreateWorld-AO-NewCensus2011CAT_TFM.ipynb that assigns distances to workers based on their mean of transport and time to work, and the obtained distances from BSC using all day or morning data.
* tamanys_empreses.ipynb: Python notebook file that intends to recreate categorical company size distribution using a random gamma function for each economic sector
* whole_population_2.R: R script that analyses variations of population in Catalunya.
