# Persistence of Collective Memory

## Overview
Public memory and attention to significant events have been studied as collective memory composed of (i) communicative memory (individual-level communication), and (ii) cultural memory (cultural formations such as rites and monuments).
In previous work, large-scale social media data was used to quantitatively study collective memory. 
Most of these studies analyze user attention to single discrete events, e.g., celebrity deaths, and assume universal attention patterns.
However, many significant events may reoccur or have ongoing updates. 
Additionally, individuals may exhibit differing attention patterns.
Furthermore, the ability of an event originator to shape collective memory by, for example, attempting to influence media reports was not analyzed.
In this study, we investigate collective memory of publicly traded U.S. companies that filed for Chapter 11 bankruptcy from 2012 to 2022.
These companies continue to operate, and thus, another bankruptcy may reoccur to the same company, while exhibiting different public attention characteristics.
Additionally, we address each company's crisis management strategy that can shape collective memory.
We collected 248,936 Twitter mentions of 58 companies one month before and after each bankruptcy event.
After each event, collective attention sharply spikes, followed by High or Low persistence level of attention compared with prior averaged attention for each company.
We fit two bi-exponential models to the twetting patterns of High vs. Low attention levels.
Given an event, we successfully (F1-score 0.85) predict on bankruptcy day the company's pre-bankruptcy persistence level of public attention.
Studying bankruptcies with varying attention patterns provides a novel perspective of collective memory and how pre-bankruptcy attention shapes company remembrance.

## Motivation

## Results

## Running the code

`YahooFinance.ipynb` -- download stock values over time.

`CollectTweets(NewBIUBankruptcy)` -- collect tweets that mention company name 30 days before and 30 days after the finantial event (bankruptcy or stock crash)

`Low_stocks.r` -- find the date when the stock price crashed in terms of $Min${(`Adj Close`$[t])$/(`Adj Close`$[t+1]) - 1$}.

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
