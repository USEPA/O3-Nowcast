# O3-Nowcast

## Background
For more detail on why this repo exists as well as methods used see the attached [whitepaper](./WhitePaper.pdf).

## Setup
This repo has a few dependencies which can be installed by running.
```
Rscript Setup.r
```

## File overview
- Example.r
    - Top level script that performs some overhead and calls the other pieces to run on example dataset

- NowcastFun.r
    - Functions related to calculations are defined here.  As of 5/2/19 these include:  The PLS Nowcast functions, the surrogate by linear regression, and counting max consecutive NA's in the data stream.

- GetNowcast.r
    - Contains the GetNowcast() function.  This goes through a decision tree based on properties of the data to decide whether a Nowcast is obtained via the PLS model, the surrogate regression, or 0 or previous hours' Nowcast value are used.  Output is a single row'ed data.frame containing the date and Nowcast of the current hour.  Calls on functions defined in NowcastFun.r.

- RunNowcast_local_v2.r
    - Contains the run.nowcast() function to apply the Nowcast calculation to a data stream, exemplified in Example.r.

- ExampleData.csv
    - Example data upon which the Nowcast is calculated by Example.r
