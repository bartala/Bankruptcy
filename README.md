# Persistence of Collective Memory

## Overview
Bankruptcy prediction using Twitter data

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
