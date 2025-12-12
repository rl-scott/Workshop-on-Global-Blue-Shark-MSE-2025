
# ---- Introduction -----

# This R script provides the materials needed for the openMSE breakout session.
#
# There are 3 Parts to this breakout session:
# 
# Part 1: Overview of openMSE
# - import operating model from SS3 output
# - explore `MOM` and `OM` objects
# - introduce the `Data` object
# - example management procedures
# - run the forward projections
# - example the MSE output
# - calculate performance metrics
# - develop custom performance metric functions


# Part 2: Blue Shark Case Study: Base Case
# - define custom management procedures
# - calculate the TAC in first projection year
# - modify the CMPs as needed
# - run forward projections
# - calculate CMP performance

# Part 3: Alternative Operating Models: Reference and Robustness OMs
# - build alternative OMs
# - run forward projections for alternative OMs
# - calculate CMP performance for alternative OMs

# Outcomes:
#
# The expected outcomes of this breakout session are:
# - brief overview of the characteristics of the selected blue shark case study 
# - description of the performance metrics used to summarize CMP performance
# - description of the CMPs evaluated in the analysis
# - completion of the Base MSE run 
# - completion of MSE runs for at least two alternative OMs
#
# In the next breakout session we will explore using the `Slick` R package to 
# visualize, explore, and compare the performance of the CMPs


## ---- Load Functions and Required Packages ----

# Set the working directory to your local copy of MSE Workshop Materials
setwd("C:/BlueSharkMSEWorkshop/BreakoutMaterial")

# Load workshop functions
source('Functions.R')

# Install (if necessary) and load openMSE and other packages
LoadPackages() 

## ---- Set OM and MSE Parameters ----

# The number of simulations (typically 100+ but keep small for faster computations)
nsim <- 12

# The management interval - how often the MP will be applied
interval <- 3

# The number of projection years 
proyears <- 30

# Other things not considered:
# - data lags: model assumes data provided to MPs is available up to the
#   year before the management advice is implemented. 


## ---- Select a Stock for Analysis and Load SS3 Output ----

# SS3 Output is available for 5 Blue Shark Stocks:
Stocks() 

# Specify your stock of interest: 
stock <- "SouthAtlantic"

# Load the SS3 Output 
SSOutput <- GetSSOutput(stock)

# ---- Part 1. Overview of openMSE ---- 

## ---- Import MOM from SS3 Output ----

# Import SS3 Output into a Multi-Stock/Fleet Operating Model
MOM <- SS2MOM(SSOutput, nsim=nsim, proyears=proyears, interval = interval) 

# `MOM` is a multi-stock/fleet operating model (OM):
class?MOM # https://msetool.openmse.com/reference/MOM-class.html

# It includes both female & male stocks (if applicable) and all fishing fleets
# included in the assessment model.
#
# The operating model should match the fishery dynamics described in the 
# assessment model.
#
# This can be comparing the simulating the historical fishery described in the OM
# with the SS3 output.

# First, simulate the historical fishery described in the OM:
multiHist <- Simulate(MOM)

# Then compare the OM Dynamics with the SS3 Output:
plot_SS2MOM(multiHist, SSOutput)

# Hopefully the OM matches the SS3 output! 

## ---- Aggregate Stocks and Fleets: Import OM from SS3 Output ----

# As mentioned above, the MOM object includes information on both female & male
# stocks (if applicable) and all fishing fleets included in the assessment model.
#
# Unless there is a need to specify fleet-specific management regulations, such 
# as different TAC allocations than were used in the path, or fleet-specific 
# size regulations, it is more computationally efficient to combine the 
# fleet dynamics together into an Aggregate Fleet.
#
# Similarly, if the female and male stock dynamics are the same, the two stocks
# can be combined together.
# 
# Aggregating the multi-stock/fleet operating model into a single stock/fleet
# OM requires a lot less computations and makes the MSE process much quicker.

# Import single-stock/fleet OM from SS3:
OM <- SS2OM(SSOutput, nsim=nsim, proyears=proyears, interval=interval)

OM # an object of class `OM`

class?OM # https://msetool.openmse.com/reference/OM-class.html

slotNames(OM)

# Simulate Historical Fishery:
Hist <- Simulate(OM)

# Compare OM Dynamics with SS3 Output
plot_SS2OM(Hist, SSOutput)

# In many cases, the aggregated-stock/fleet OM has identical fishery dynamics to
# those described in the female/male + multi-fleet SS3 model. 
#
# One exception is that the spawning biomass in the OM will be twice as high
# because there is only one stock - combined female + male. 
# However, the reference points (SB0, SBMSY, etc) are adjusted by same amount 
# so that is inconsequential in terms of calculating metrics such as SB/SB0, 
# SB/SBMSY, etc
#
# If there are sex-specific differences in the Female-Male MOM (such as 
# differences in the growth curves, natural mortality, etc) the fishery dynamics
# described in the aggregated-stock/fleet OM may differ from those described in
# the SS3 output. The difference between the OM and MOM depends on the degree of 
# difference between the stocks parameters.
#
# Except for the North Pacific case study (see note below), the fishery dynamics
# in the aggregated-stock/fleet OM and identical to the SS3 output. 
#
# For the purposes of this workshop, we will use the single-stock/fleet version
# of the Operating Model. For real applications, you may wish to use the 
# multi-stock/fleet MOM.
#
# If there is sufficient time, you can repeat these analyses using the 
# multi-Stock/Fleet model (class `MOM`)
#
## ---- Note for North Pacific Blue Shark ------------------------------------##
# The aggregated-stock dynamics for the `NorthPacific` case study are 
# quite different to the multi-stock model/SS3 output. This is due to the 
# significant differences in natural mortality, weight-at-age and recruitment 
# deviations between the Female and Male stocks.
#
# Our suggestion is to initially ignore these differences and complete the work
# with the aggregated stock/fleet model. The analysis can then be repeated with 
# the multi-stock/fleet operating model (this will take more time to run)
#
## ---- End Note for North Pacific Blue Shark --------------------------------##



## ---- Recap of Steps so Far -----

# No need to re-run:

nsim <- 12 # set number of stochastic simulations
interval <- 3 # set the management cycle (how often the CMPs are applied)
proyears <- 30 # set the number of projection years
SSOutput <- GetSSOutput(stock) # Import the SS3 output
OM <- SS2OM(SSOutput, nsim, proyears, interval=interval) # Create OM from SS3 output
Hist <- Simulate(OM) # Simulate Historical Fishery

# Alternative for multi-stock/fleet model:
# MOM <- SS2MOM(SSOutput, nsim, proyears, interval = interval) # Create MOM from SS3 output:
# multiHist <- Simulate(MOM) # Simulate Historical Fishery


## ---- Explore the Data Object ----

# The OM contains the observed fishery data that was used in the SS3 assessment:
plot(OM@cpars$Data)

OM@cpars$Data

# The fishery data stored in an object of class `Data`
class?Data # https://msetool.openmse.com/reference/Data-class.html

# The observed `Data` object is returned by the `Hist` object and will 
# be made available to the MPs in the projection period:

Data <- Hist@Data

# In this workshop we will only be using the Catch and Index data used in the 
# SS3 Models

# Catch data in the `Data` object:
CatchDF <- data.frame(Year=Data@Year, Catch=Data@Cat[1,]) 

ggplot(CatchDF, aes(x=Year, y=Catch)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


# The indices of abundance (e.g. CPUE) used in the SS3 model are stored in the 
# `AddInd` slot:
Hist@Data@AddInd[1,,] |> t()

IndexDF <- Array2DF(Hist@Data@AddInd[1,,])

ggplot(IndexDF, aes(x=Year, y=Value)) +
  facet_wrap(~Fleet) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

# Management procedures built into openMSE may have problems if all index values
# in the terminal year are NAs. 
# 
# Here we are replacing any NAs in the terminal year of Data@AddInd
# with the mean of the previous 3 years.

Hist@Data <- ImputeIndices(Hist@Data) # Historical Data
Hist@Data@AddInd[1,,] |> t()


## ---- Management Procedures ----

# The `avail` function lists the available `MP` functions:
avail('MP') |> sort()

# Help documentation is available for each MP:
?AvC # Average Catch - https://dlmtool.openmse.com/reference/AvC.html
?FMSYref # Fishing at FMSYref - https://msetool.openmse.com/reference/FMSYref.html
?Itarget1 # Incrementally adjust TAC - https://dlmtool.openmse.com/reference/Itarget1.html


# Management procedures are R functions that have been assigned a class `MP`
# and take a `Data` object as the primary input:
class(AvC)
args(AvC)

# Arguments:
# x: simulation number - 1:OM@nsim
# Data: an object of class `Data` containing real and simulated fishery data
# reps: number of stochastic samples (if there is uncertainty in observed data)
# plot: internal use only

# Apply the `AvC` MP to the historical data:
Rec <- AvC(1, Hist@Data, reps=1) 

# The MPs return an object of class `Rec`
Rec
class(Rec)
class?Rec # https://msetool.openmse.com/reference/Rec-class.html

mean(Hist@Data@Cat[1,], na.rm = TRUE) # average historical catch
Rec@TAC # MP TAC


## ---- Forward Projections ----

# Select some MPs for the Forward Projections:
MPs <- c('AvC', 'FMSYref', 'Itarget1')

# Run the forward projections:
MSE <- Project(Hist, MPs=MPs)

# The `Project` function returns an object of class `MSE`:
MSE # An object of class `MSE`
class?MSE # https://msetool.openmse.com/reference/MSE-class.html

## ---- Explore the MSE Object ----

# The performance of the cMPs can be calculated from the MSE output:

# Landings: mean over simulations
apply(MSE@Catch, 2:3, mean) |> t()

# Removals (Landings + Dead Discards): mean over simulations:
apply(MSE@Removals, 2:3, mean) |> t()

# TAC: mean over simulations:
apply(MSE@TAC, 2:3, mean) |> t() 

# SB/SBMSY: mean over simulations
apply(MSE@SB_SBMSY, 2:3, mean) |> t() |> round(2) 

# F/FMSY: mean over simulations
apply(MSE@F_FMSY, 2:3, mean) |> t() |> round(2) 


# The MSE results can be summarized by using some of the plotting routines
# included in openMSE:

Splot(MSE) # standard summary plot
Tplot(MSE) # trade-off plot

# or with custom plots:
SB_SBMSY <- apply(MSE@SB_SBMSY, 2:3, mean) |> Array2DF() 

ggplot(SB_SBMSY, aes(x=Year, y=Value, color=MP)) +
  geom_line() +
  expand_limits(y=0) +
  labs(x='Projection Year', y=bquote(SB/SB[MSY])) +
  theme_bw()


## ---- Calculate Performance Metrics ----

# openMSE includes several built-in functions for calculating performance 
# metric. 
# These are R functions of class `PM` that take an `MSE` class object as the
# primary argument. See also https://openmse.com/features-calculating-performance/ 

# List available PM functions: 
avail('PM')

class(P100)
?P100 # https://msetool.openmse.com/reference/PerformanceMetric.html

# Example applying `P100` to the MSE output:
p100 <- P100(MSE) 
p100

# Same as calculating mean SB/SBMSY directly:
apply(MSE@SB_SBMSY > 1, 2, mean) |> round(2)

# The advantage of using the PM functions is that the returned object contains
# additional information relating to the calculated metric:
p100@Name
p100@Caption
p100@Mean


## ---- Custom Performance Metrics ----

# Custom performance metric functions can be developed following the same 
# structure as the built-in funcitons. 
#
# The 'CustomPMs.R' script contains some example custom PM functions that can
# be modified or extended based on your application:

source('CustomPMs.R')

Safety(MSE)
Status(MSE)

Catch_ST(MSE)
Catch_LT(MSE)


# Questions to Consider: 
#
# 1. What types of Performance Metrics (PMs) would you expect to need for your 
#    blue shark MSE?
#
#    Modify the PMs in CustomPMs.R as required or construct your own following
#    the same structure.
#
# 2. The `AAVY` (?AAVY) PM calculates the probability that the average annual
#    variability in the catch is below some value.
#    If the management cycle is greater than one year, the managers may be 
#    interested in the average absolute change in TAC between management cycles,
#    rather than the mean over all years. 
#    How would you calculate the Probability that the Average Variability in TAC 
#    between managements cycles is less than X%? 


# ---- Part 2. Blue Shark Case Study: Base Case ----

## ---- Define Custom Management Procedures ----

# Management procedures can vary a great deal, from a simple rule fixing the 
# future TAC to something link the average historical catch, to much more complex
# approaches like running an age-structured assessment model and applying a 
# harvest control rule.

# In this workshop we are using two relatively simple, but highly flexible, 
# management procedures:
# - Emp
# - Spict

### ---- Empirical Management Procedure: Emp ----

# A flexible empirical management procedure:
?Emp

# Some Choices:
# 1. Which indices to use  (`Inds`)
# 2. Weighting of the indices to calculate a mean index (`I_wt`)
# 3. Any minimum or maximum TACs (`TACrng` argument)
# 4. Any constraints in upward or downward changes in TAC (`delta_down` and `delta_up`)
# 5. Locations of the operational control points (`HCR_CP_B` and `HCR_CP_TAC`)

# See "Emp.html" or "Emp.pdf" for guide of customizing Emp function.


### ---- Model-Based Management Procedure: spict ----

# Spict: A stochastic surplus production model in continuous time
# Pederson & Berg (2016) https://onlinelibrary.wiley.com/doi/abs/10.1111/faf.12174

# Lots of customization options:

# https://github.com/DTUAqua/spict

?MSEextra::spict

## ---- Calculate the First TAC ----

# Two variants of the `Emp` and `spict` have been included in "CustomMPs.R":
source('CustomMPs.R')

# These MPs can be modified as needed, or new MP functions can be created 
# following the same structure.

# Define the CMPs to evaluate in the closed-loop simulation testing
MPs <- c('Emp1', 'Emp2', 'Spict1', 'Spict2') 


# Calculate TAC in the first year (this is done internally in the MSE)
TACs <- applyMP(Hist@Data, MPs=MPs, reps=1, nsims=1)[[2]]@TAC

TACDF <- data.frame(Year=max(Hist@Data@Year)+1, MP=MPs, TAC=TACs)

TACDF

# Plot historical catches and MP-generated TAC for the first projection year:


ggplot() +
  geom_line(data=data.frame(Year=Hist@Data@Year, Catch=Hist@Data@Cat[1,]),
            aes(x=Year, y=Catch)) +
  geom_point(data=TACDF, aes(x=Year, y=TAC, color=MP)) +
  expand_limits(y=0) +
  theme_bw()

# Depending on how the MP-generated TACs for the first projection year look 
# compare to your expectations, you may wish to modify some of the properties
# of the MPs defined in "CustomMPs.R" 


## ---- Forward Projections ----

# After the candidate management procedures have been developed, the forward
# projections can be run:

MSE <- Project(Hist, MPs=MPs)

# Examine MSE Output:
Splot(MSE)

# Calculate Performance Metrics:
Safety(MSE)
Status(MSE)
Catch_ST(MSE)
Catch_LT(MSE)

# Save the MSE object:
Save(MSE, file.path('MSE_Objects', stock, 'Base.mse'))


# ---- Part 3. Alternative Operating Models: Reference and Robustness ----

# So far we have conducted the MSE on an Operating Model based on the MLE estimates
# from the Base Case Stock assessment.
#
# In this section we will build some additional OMs that account for some 
# uncertainties in the knowledge about the fishery dynamics.
#
# The code below builds two operating models that use different values for 
# natural mortality (lower and higher than the Base run) 

## ---- Lower and Higher Natural Mortality ----

# Natural Mortality Rate in the Base OM:
OM@M[1]

### ---- OM with lower natural mortality rate ----

delta <- 0.8 # multiplicative adjustment to M

# Modify SS3 files and re-run SS3 model:
ModifyNaturalMortality_SS3(stock, delta)

# See `ModifyNaturalMortality_SS3` in `Function.R` to see how M was modified in the SS3 
# control files

# Import the OM: 
SSdir <- file.path('Alternative_OMs', stock, paste0('M_', delta))
OM_LowerM <- SS2OM(SSdir, nsim=nsim, proyears = proyears, interval=interval)

# Run the forward projections for this OM:
MSE <- runMSE(OM_LowerM, MPs=MPs)

# Save the MSE object
Save(MSE, file.path('MSE_Objects', stock, paste0('M_', delta, '.mse')))

### ---- OM with higher natural mortality rate ----

delta <- 1.25 # multiplicative adjustment to M

# Modify SS3 files and re-run SS3 model:
ModifyNaturalMortality_SS3(stock, delta)

# Import the OM: 
SSdir <- file.path('Alternative_OMs', stock, paste0('M_', delta))
OM_HigherM <- SS2OM(SSdir, nsim=nsim, proyears = proyears, interval=interval)

# Run the forward projections for this OM:
MSE <- runMSE(OM_HigherM, MPs=MPs)

# Save the MSE object
Save(MSE, file.path('MSE_Objects', stock, paste0('M_', delta, '.mse')))


# ---- !! Questions for Reporting: !! ---- 

# 1. Provide a brief summary on the fishery dynamics predicted by the SS3 model
#    e.g., historical trend in spawning biomass, catch (discards), fishing
#    mortality, SB/SBMSY, F/FMSY (see code below for examples)
# 2. Briefly describe the Candidate Management Procedures evaluated in MSE
# 3. What are the main properties of the CMPs that would be considered in your
#    MSE?
# 4. What PMs are used to summarize the performance of the CMPs
# 5. Provide a table of summary PM values for the CMPs evaluated for the Base
#    and Alternative OMs 
# 6. How robust are the cMPs to the uncertainties considered in the Base and 
#    Alternative OMs 
# 7. What additional uncertainties would you consider for your case study


# Values can be obtained from the SS3 Output:

plot(SSOutput$timeseries$Yr, SSOutput$timeseries$Bio_all)

# Or the `Hist` object:
Years <- Hist@Data@Year
lines(Years, rowSums(Hist@TSdata$Biomass[1,,]))

SBMSY <- Hist@Ref$ReferencePoints$SSBMSY[1]
SB <- rowSums(Hist@TSdata$SBiomass[1,,]) # sum over areas (areas not used in this model)

plot(Years, SB/SBMSY, ylim=c(0,4))

SB0 <- Hist@Ref$ReferencePoints$SSB0[1]
SB <- rowSums(Hist@TSdata$SBiomass[1,,]) # sum over areas (areas not used in this model)

plot(Years, SB/SB0, ylim=c(0,1))











