
# ---- Presenting MSE Results using Slick -----

## ---- Load Functions and Required Packages ----

# Set the working directory to your local copy of MSE Workshop Materials
setwd("C:/BlueSharkMSEWorkshop/BreakoutMaterial")
setwd("G:/Shared drives/BM shared/1. Projects/TOF Advisory/Blue Shark MSE Workshop/BreakoutMaterial")

# Load workshop functions
source('Functions.R')

# Install (if necessary) and load openMSE and other packages
LoadPackages() 


## ---- Load MSE Output ----
Stocks()

# Specify your stock of interest: 
stock <- "SouthAtlantic"

# List of MSE Objects that have been created:
MSEFiles <- list.files(file.path('MSE_Objects', stock))

MSEList <- list()
for (i in seq_along(MSEFiles))
  MSEList[[i]] <- readRDS(file.path('MSE_Objects', stock, MSEFiles[i]))


# Info about the MSE 
nSim <- MSEList[[1]]@nsim       # number of simulations
nOM <- length(MSEList)          # number of discrete OMs
nMP <- length(MSEList[[1]]@MPs) # number of cMPs
MPNames <- MSEList[[1]]@MPs     # names of the cMPs
nyears <- MSEList[[1]]@nyears   # number of historical years
proyears <- MSEList[[1]]@proyears # number of projection years


# ---- Make the Slick Object ----

# The information is stored in an object of class `Slick`:
Slick <- Slick()

?Slick # https://slick.bluematterscience.com/reference/Slick-class.html


# ---- Metadata ----

# Metadata are optional, but can provide useful context for when the results are
# viewed in the Slick App:

Title(Slick) <- paste("A theoretical MSE for", stock, 'Blue Shark')
Author(Slick) <- c('Author 1', 'Author 2')
Email(Slick)
Institution(Slick)

Introduction(Slick) <- "
This MSE was produced at the Global BSH MSE Workshop (Rome, Italy -  October 2025).

It is an example of ...


"

# ---- MPs ----

# Information on the candidate management procedures is stored in an object of 
# class `MPs`

?MPs # https://slick.bluematterscience.com/reference/MPs-methods.html

MP_Code <- MPNames
MP_Label <- MPNames

# replace with brief description of your CMPs
MP_Description <- paste('Description of MP', seq_along(MP_Code), collapse=NULL) 

MPs(Slick) <- MPs(Code=MP_Code,
                  Label=MP_Label,
                  Description=MP_Description
                  )


MPs(Slick)

# ---- OMs ----

# The `OMs` slot contains information describing the structure of the operating 
# models include in the Slick object.

OMs <- OMs()

OMs

?OMs # https://slick.bluematterscience.com/reference/OMs-class.html; 
     # https://slick.bluematterscience.com/reference/OMs-methods.html

# There are three things to populate for an `OMs` object: 

# 1. Factors - a data.frame describing Factor, Level, and Description

# Modify the following code to match the OM Factors and Levels that were
# modeled in "1. MSE Breakout Session.R"

Factors(OMs) <- data.frame(Factor='M',
                           Level=c('Base', 'Low M', 'High M'),
                           Description=c('Base Case',
                                         'Lower Natural Mortality',
                                         'Higher Natural Mortality')
)

# 2. Design - an `nOM` row and `nFactor` column matrix
# The Design matrix is used to map the Factor/Levels onto each OM included in the
# Slick object

# There should be `nFactor` columns, with the column names matching the `Factor`
# in `Factors(OMs)` and `nOM` rows with either numeric values matching the order
# of `Level`, or the actual names in `Level`:

# Here we have one factor with three levels: i.e., 3 OMs:
Design(OMs) <- data.frame(M=c('Base', 'Low M', 'High M'))

# Names for the OMs can (optionally) be added as rownames:
rownames(Design(OMs)) <- c('Base OM ', 'Low M OM ', 'High M OM')


# 3. Preset - a named list 
# Preset is a named list specifying how the results will be aggregated over OMs
# in the Slick App. 
#
# By default the results will be averaged over all OMs (unless viewed by OM).

Preset(OMs) <- list('Base Case'=list(1),
                    'Low M' = list(2),
                    'High M' = list(3),
                    'All'= list(1:3)
)

# Assign our `OMs` object to our `Slick` object:
OMs(Slick) <- OMs

# ---- Slick Plots ----

# A Slick Object can include the following plots:
#   
# - Time Series: Plots the time series of a user-specified variable (e.g., 
#   yield, biomass) during the historical period and the projection period for
#   each management procedure.
#
# - Boxplot: Displays the minimum, first quartile, median, third quartile, and
#   maximum from a set of results. Includes an option to display the data as a 
#   violin plot.
#
# - Kobe: Compares the performance of MPs with respect to biomass (on the 
#   x-axis) and fishing mortality (on the y-axis). Includes an option to display 
#   the data as a Kobe Time plot, which shows the percentage of runs that fall 
#   in each of the Kobe quadrants in each projection year.
#
# - Quilt: A table of performance indicators for each management procedure, 
#   with colored shading to indicate better/worse performance.
# 
# - Spider: Shows results for three or more performance indicators, each 
#   represented on an axis starting from the same center point. Sometimes also 
#   called a Radar plot.
#
# - Tradeoff: Scatter plot comparing two performance indicators.
#
# A valid Slick Object must include at least one Slick Plot.

# The following provides example code for create Time Series, Kobe, and Quilt
# plots.
# If there is sufficient time, you can create other plots of your choice.

## ---- Time Series ----

# Makes time-series plots of:
# - SB/SBMSY
# - F/FMSY
# - Landings
#
# Modify as necessary 

timeseries <- Timeseries()
timeseries # an object of class `Timeseries`
?Timeseries # https://slick.bluematterscience.com/reference/Timeseries-methods.html

# Details of the Performance Indicators to show in the time series plot:
Metadata(timeseries) <- data.frame(
  Code=c('SB/SBMSY', 'F/FMSY', 'Landings'),
  Label=c('SB/SBMSY', 'F/FMSY', 'Landings'),
  Description=c('Spawning biomass relative to equilibrium spawning biomass corresponding with maximum sustainable yield (MSY)',
                'Fishing mortality relative to F_MSY',
                'Landings')
)

# Last historical time step:
TimeNow(timeseries) <- MSEList[[1]]@OM$CurrentYr[1]

# Historical and Projection time steps (Years)
Time(timeseries) <- c(seq(TimeNow(timeseries), by=-1, length.out=nyears) |> sort(),
                      seq(TimeNow(timeseries)+1, by=1, length.out=proyears)
)


# Create empty array with correct dimensions
nPI <- timeseries |> Code() |> length() # number of Performance Indicators
Value(timeseries) <- array(NA, dim=c(nSim, nOM, nMP, nPI, nyears+proyears))

# Any relevant target and limit reference points for time series plots:
Target(timeseries) <- c(1, NA, NA)
Limit(timeseries) <- c(0.4, 1, NA)

# Loop over MSE objects (i.e. discrete OMs in our case) and populate `Value`:
#
# NOTE: the order of the MSE objects must match the order of the OMs described 
# in `Design(Slick)`, otherwise the labels and filtering will be incorrect!


for (i in 1:nOM) {
  MSE <- MSEList[[i]]
  
  # SB_SBMSY
  HistSB <- apply(MSE@Hist@TSdata$SBiomass, 1:2, sum) 
  HistSB <- replicate(nMP, HistSB) |> aperm(c(1,3,2))
  ProjSB <- MSE@SSB
  SB <- abind::abind(HistSB, ProjSB, along=3)
  SB_SBMSY <- SB/MSE@RefPoint$SSBMSY 
  Value(timeseries)[,i,,1, ] <- SB_SBMSY
  
  # F_FMSY
  HistF <- MSE@Hist@TSdata$Find
  HistF <- replicate(nMP, HistF) |> aperm(c(1,3,2))
  ProjF <- MSE@FM
  Fmort <- abind::abind(HistF, ProjF, along=3)
  F_FMSY <- Fmort/MSE@RefPoint$FMSY 
  Value(timeseries)[,i,,2, ] <- F_FMSY
  
  # Catch
  HistCatch <- apply(MSE@Hist@TSdata$Landings, 1:2, sum)
  HistCatch <- replicate(nMP, HistCatch) |> aperm(c(1,3,2))
  ProjCatch <- MSE@Catch
  catch <- abind::abind(HistCatch, ProjCatch, along=3)
  Value(timeseries)[,i,,3, ] <- catch
}

# Add the populated `Timeseries` object to the `Slick` object:
Timeseries(Slick) <- timeseries

# The time series plots can be plotted directly from the console:

?plotTimeseries # https://slick.bluematterscience.com/reference/plotTimeseries.html

plotTimeseries(Slick) # this is showing average across the OMs
plotTimeseries(Slick, byMP=TRUE) # this is showing average across the OMs
plotTimeseries(Slick, OM=1, byMP=TRUE) # only for OM 1 

plotTimeseries(Slick, byOM=TRUE)
plotTimeseries(Slick, byMP=TRUE, byOM=TRUE)

plotTimeseries(Slick, 2, byMP=TRUE, byOM=TRUE)
plotTimeseries(Slick, 3, byMP=TRUE, byOM=TRUE)

plotTimeseries(Slick, 1, 
               includeHist = FALSE,
               includeQuants = FALSE)


# Now is a good time to test our Slick object in the App:
#App(slick=Slick)

## ---- Kobe ----

# Details of the Performance Indicators to show in the Kobe plot
# (almost alway SB/SBMSY and F/FMSY):

kobe <- Kobe(Code=c('SB/SBMSY', 'F/FMSY'),
             Label=c('SB/SBMSY', 'F/FMSY'),
             Description = c('Spawning biomass relative to SB_MSY',
                             'Fishing mortality relative to F_MSY')
)
kobe
?Kobe # https://slick.bluematterscience.com/reference/Kobe-methods.html

# Projection time steps
Time(kobe) <- seq(MSEList[[1]]@OM$CurrentYr[1], by=1, length.out=proyears)

# Create empty array with correct dimensions
nPI <- kobe |> Code() |> length()
Value(kobe) <- array(NA, dim=c(nSim, nOM, nMP, nPI, proyears))

# Loop over MSE objects (i.e. discrete OMs in our case) and populate `Value`:
#
# NOTE: the order of the MSE objects must match the order of the OMs described 
# in `Design(Slick)`, otherwise the labels and filtering will be incorrect!

for (i in 1:nOM) {
  MSE <- MSEList[[i]]
  Value(kobe)[,i,,1, ] <- MSE@SB_SBMSY
  Value(kobe)[,i,,2, ] <- MSE@F_FMSY
}

# Add the populated `Kobe` object to the `Slick` object:
Kobe(Slick) <- kobe

# The Kobe plots can be plotted directly from the console:
?plotKobe # https://slick.bluematterscience.com/reference/plotKobe.html

plotKobe(Slick) # note this is averaged over all OMs
plotKobe(Slick, Time=TRUE) # note this is averaged over all OMs

plotKobe(Slick, OM=1) # Kobe for only the first OM
plotKobe(Slick, OM=1, Time=TRUE) # Kobe for only the first OM


## ---- Quilt ----

quilt <- Quilt()
quilt
?Quilt # https://slick.bluematterscience.com/reference/Quilt-methods.html


# Source the custom performance metric functions
source('CustomPMs.R')

# List of available PM functions:
avail('PM')

# Modify as required:
Quilt_Code <- c('Status', 'Safety', 'P100', 'PNOF', 'Catch_ST', 'Catch_LT')
Quilt_Label <- c('Status', 'Safety', 'Prob SB>SBMSY', 'Prob. Not Overfishing', 
                   'Short-term Catch', 'Long-term Catch')
Quilt_Description <- c(
  'Probability of being in the Kobe green quadrant',
  'Probability of SB>0.4SBMSY',
  'Probability of SB>SBMSY',
  'Probablity of F<FMSY',
  'Average catch in first 10 years',
  'Average catch in last 10 years'
)

Metadata(quilt) <- data.frame(
  Code=Quilt_Code,
  Label=Quilt_Label,
  Description=Quilt_Description
)


# Create empty array with correct dimensions
nPI <- quilt |> Code() |> length() # number of Performance Indicators
Value(quilt) <- array(NA, dim=c(nOM, nMP, nPI))

# Loop over MSE objects (i.e. discrete OMs in our case) and populate `Value`:
#
# NOTE: the order of the MSE objects must match the order of the OMs described 
# in `Design(Slick)`, otherwise the labels and filtering will be incorrect!
for (i in 1:nOM) {
  MSE <- MSEList[[i]]
  Value(quilt)[i,,1] <- Status(MSE)@Mean
  Value(quilt)[i,,2] <- Safety(MSE)@Mean
  Value(quilt)[i,,3] <- P100(MSE)@Mean
  Value(quilt)[i,,4] <- PNOF(MSE)@Mean
  Value(quilt)[i,,5] <- Catch_ST(MSE)@Mean
  Value(quilt)[i,,6] <- Catch_LT(MSE)@Mean
}

# Add the populated `Quilt` object to the `Slick` object:
Quilt(Slick) <- quilt

# The Quilt plots can be plotted directly from the console:
?plotQuilt # https://slick.bluematterscience.com/reference/plotQuilt.html

plotQuilt(Slick) # note this is averaged over all OMs
plotQuilt(Slick, OM=1) # only the first OM
plotQuilt(Slick, OM=1, kable=TRUE) # static table

# ---- Save the Slick Object ----
#Save(Slick, file.path('Slick_Objects', paste0(stock, '.slick')))

# ---- Explore the Slick Plots ----
App(slick=Slick)

