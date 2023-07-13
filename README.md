# Persistence of Collective Memory

## Overview
Collective memory and attention surrounding significant events have been quantitatively studied using large-scale social media data. 
Previous studies typically analyzed user attention to static events that do not alter post-event (e.g., publishing a paper), and assume universal public attention.
However, many significant events have ongoing updates. 
Additionally, individuals may exhibit different attention patterns.
We investigate collective memory of U.S. companies that are active on Twitter and filed for Chapter 11 bankruptcy.
As opposed to static events, a company's financial status under Chapter 11 bankruptcy is dynamic as the company continues to operate.
These continuous updates can cause differing public attention over time to bankruptcy events.
We collected 248,936 Twitter mentions of 74 companies one month before and after each bankruptcy event.
Attention sharply spikes after bankruptcy, revealing High, or Low persistence level of attention compared with prior bankruptcy averaged  attention.
We fit two bi-exponential models to the tweeting patterns of High, and Low attention levels.
Additionally, we accurately (F1-score of 0.81) predicted the post-bankruptcy persistence level of public attention on the day of bankruptcy.
Studying dynamic bankruptcy events reveals varying attention patterns, a novel perspective of collective memory, and how pre-bankruptcy attention shapes company remembrance.

## Motivation

## Results

## Running the code

`YahooFinance.ipynb` -- download stock values over time.

`CollectTweets(NewBIUBankruptcy)` -- collect tweets that mention company name 30 days before and 30 days after the finantial event (bankruptcy or stock crash)

`Low_stocks.r` -- find the date when the stock price crashed in terms of $Min${(`Adj Close`$[t])$/(`Adj Close`$[t-1]) - 1$}.

`combine_data.R` -- combine collected Twitter datasets on companies that declared chapter 11 bankruptcy between 2012 to 2022.

`data_analysis_.ipynb` -- analyze mention time series.

## Miscellaneous
Please send any questions you might have about the code and/or the algorithm to alon.bartal@biu.ac.il.


## Citing
If you find this code useful for your research, please consider citing us:
```
@article{Jagodnik2023,
  title     = {Persistence of collective memory about bankruptcy events declared by U.S. companies.},
  author    = {Jagodnik, Kathleen M. and Bartal, Alon},
  journal   = {},
  volume    = {},
  number    = {},
  pages     = {from page– to page},
  year      = {2023}
}
