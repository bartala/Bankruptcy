# Persistence of Collective Memory

## Overview
Collective attention and memory involving significant events have been quantitatively studied using large-scale social media data. 
Previous studies analyzed user attention to static events that do not change post-event (e.g., a celebrity's death), and assume universal public attention patterns.
However, many significant events are dynamic, involving ongoing updates. 
Additionally, individuals may exhibit different attention patterns.
We investigate collective memory of U.S. companies that filed for Chapter 11 bankruptcy and were mentioned on Twitter.
Unlike static events, a company's financial status under Chapter 11 bankruptcy is dynamic, as the company typically remains operational.
These continuous updates can affect public attention over time following bankruptcy events.
We collected 248,936 Twitter mentions of 74 companies one month before and after each bankruptcy event.
We observed a sharp spike in attention after bankruptcy, and Low or High persistence level of attention compared with attention level prior to bankruptcy.
We fit two bi-exponential models to the tweeting patterns of Low and High attention levels.
Importantly, we successfully (F1-score of 0.81) predicted the post-bankruptcy persistence level of public attention as of the day of bankruptcy.
Studying bankruptcy events via social media coverage reveals varying attention patterns, informs how pre-bankruptcy attention shapes the remembrance of a company post-bankruptcy, and offers novel insights involving collective memory of dynamic events.

## Running the code

`CollectTweets` -- collect tweets that mention company name 30 days before and 30 days after the finantial event (bankruptcy or stock crash)

`data_analysis_.ipynb` -- analyze mention time series.

## Miscellaneous
Please send any questions you might have about the code and/or the algorithm to alon.bartal@biu.ac.il.


## Citing
If you find this code useful for your research, please consider citing us:
```
@article{Jagodnik2023,
  title     = {Collective Memory of Dynamic Events: Analyzing Persistence of Attention to Bankruptcy Events},
  author    = {Jagodnik, Kathleen M., Dekel, Sharon, and Bartal, Alon},
  journal   = {},
  volume    = {},
  number    = {},
  pages     = {from page– to page},
  year      = {2023}
}
