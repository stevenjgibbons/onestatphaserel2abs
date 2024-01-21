# onestatphaserel2abs

onestatphaserel2abs v1.0.0 is permanently archived on Zenodo on:  
https://zenodo.org/doi/10.5281/zenodo.10546470  
with DOI: 10.5281/zenodo.10546470

[![DOI](https://zenodo.org/badge/746230372.svg)](https://zenodo.org/doi/10.5281/zenodo.10546470)


Program to convert a set of event-vs-event times to one time per event (modified VanDecar Crosson)

We have a list of lines of the following format  

```
H01  H02  2007-08-15T08:00:27.857  2007-08-15T12:00:28.110  ARE0   P1  0.828
H01  H02  2007-08-15T08:00:48.693  2007-08-15T12:00:48.956  ARE0   S1  0.755
H01  H02  2007-08-15T08:00:32.148  2007-08-15T12:00:32.407  KEV    P1  0.808
H01  H02  2007-08-15T08:00:55.371  2007-08-15T12:00:55.641  KEV    S1  0.796
H01  H02  2007-08-15T08:00:19.109  2007-08-15T12:00:19.299  LP34   P1  0.850
H01  H02  2007-08-15T08:00:33.100  2007-08-15T12:00:33.286  LP34   S1  0.798
H01  H02  2007-08-15T08:00:08.467  2007-08-15T12:00:08.709  LP53   P1  0.926
H01  H02  2007-08-15T08:00:15.228  2007-08-15T12:00:15.476  LP53   S1  0.813
H01  H02  2007-08-15T08:00:11.736  2007-08-15T12:00:11.945  LP61   P1  0.678
H01  H02  2007-08-15T08:00:21.048  2007-08-15T12:00:21.232  LP61   S1  0.731
H01  H02  2007-08-15T08:00:09.053  2007-08-15T12:00:09.243  SGF    P1  0.658
H01  H02  2007-08-15T08:00:16.574  2007-08-15T12:00:16.744  SGF    S1  0.702
```
  
where we have evID1, evID2, time_evID1, CC_time_evID2, station, phase, CC_coefficient  

We have lines like this for all events H01 through to H55.  

We want to solve for one time for each station and phase for each event which are most consistent with the pairwise time-delay measurements.  

We use the algorithm of 

```
title = {Determination of teleseismic relative phase arrival times using multi-channel cross-correlation and least squares},
journal = {Bulletin of the Seismological Society of America},
author = {van Decar, J C and Crosson, R S},
year = {1990},
volume = {80},
pages = {150--169},
```

with a few modifications to allow for the fact that the times are associated with different events rather than different stations for the same event (since this can involve huge time-differences between events).  

The onestatphaserel2abs program finds the set of absolute times that best fit the relative time measurements.  

The program takes 3 arguments:  

(1) The station name  
(2) The phase name  
(3) One of the following keywords:  Bayesloc, Residual, Replace, Display, Allpairs  

with the times file provided as standard input.  

There is an example in each of the directories Finland_example and DPRK_example.


