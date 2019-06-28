File descriptions in this directory

PlsNowcastAirnow_simulate_operations.r:  

Code sent by STI, the latest version of their version of running O3 PLS Nowcast, I will modify this to add imputation to the hourly datastream to be used in the X-block of PLS, and remove the row-wise mean substitution.

PlsNowcastAirnow_simulate_operations_AR.r:

Modificaitons of PlsNowcastAirnow_simulate_operations.r I made to add imputation of data and a couple other things to what STI put together.

#### The files below take apart PlsNowcastAirnow_simulate_operations_AR.r to make the code more modular.  Then, I can make updates to the PLS part in a more focused way.

O3Nowcast.r:  Top level script that performs some overhead and calls the other pieces

NowcastFun.r:  Functions related to calculations are defined here.  As of 5/2/19 these include:  The PLS Nowcast functions, the surrogate by linear regression, and counting max consecutive NA's in the data stream.

GetNowcast.r:  Contains the GetNowcast() function.  This goes through a decision tree based on properties of the data to decide whether a Nowcast is obtained via the PLS model, the surrogate regression, or 0 or previous hours' Nowcast value are used.  Output is a single row'ed data.frame containing the date and Nowcast of the current hour.  Calls on functions defined in NowcastFun.r.

RunNowcast_local.r: Reads in an example data file associated with 1 hour of data and its previous 2 weeks worth of hours, and runs GetNowcast() to obtain the Nowcast value for the example data file. 

RunNowcast_STI.r: Production version of the RunNowcast_local.r file.  Connects to STI's database and performs associated overhead.  Draws data needed to calculated Nowcast for the current hour and performs the calculation by running GetNowcast() on the current data.  Sends the resulting 1-row data.frame() back to the data.base for public reporting.  Calculates time to perform the calculation.



