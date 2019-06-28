 
code.dir<-'/home/areff/NowCast/Code/O3PlsNowcast/'


# define calculation functions, including PLS
source(paste0(code.dir,"NowcastFun_Upd.r"))

# define decision function
source(paste0(code.dir,"GetNowcast.r"))

# Grab data, and get the Nowcast value.
# My "local" version uses a local file for testing 
# and ends at the Nowcast calculation
# Uncomment the following for local testing of Nowcast calculations
source(paste0(code.dir,"RunNowcast_local.r"))

# STI's version connects to the database to get data
# then generates a Nowcast and sends it back to the database
# Uncomment the following for production use
# source(paste0(code.dir,"RunNowcast_STI"))

