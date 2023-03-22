# Bankruptcy paper

## Overview
Bankruptcy prediction using Twitter data

## Motivation

## Results

## Running the code

`YahooFinance.ipynb` -- download stock values over time for the year 2020.

`CollectTweets(NewBIUBankruptcy)` -- collect tweets that mention company name 30 days before and 30 days after the finantial event (bankruptcy or stock crash)

`Low_stocks.r` -- find the date when the stock price crashed in terms of (`Adj Close`$[t])$/(`Adj Close`$[t+1]) - 1$


## Miscellaneous
Please send any questions you might have about the code and/or the algorithm to alon.bartal@biu.ac.il.


## Citing
If you find this code useful for your research, please consider citing us:
```
@article{Jagodnik2023,
  title     = {HetIG-PreDiG: A Heterogeneous Integrated Graph Model for Predicting Human Disease Genes Based on Gene Expression.},
  author    = {Jagodnik, Kathleen M. and Bartal, Alon},
  journal   = {},
  volume    = {},
  number    = {},
  pages     = {from page– to page},
  year      = {2023}
}
