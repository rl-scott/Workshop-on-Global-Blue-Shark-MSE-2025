

Safety <- function(MSE, Ref=0.4, Yrs=NULL) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ", Yrs[1], 
                            " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@SB_SBMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Safety) <- 'PM'

Status <- function(MSE, Yrs=NULL) {
  Yrs <- ChkYrs(Yrs, MSE)
  
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability Stock is in Green Kobe"
  
  PMobj@Caption <- paste0('Prob. Green Kobe (Years ', Yrs[1], ' - ', Yrs[2], ')')
  PMobj@Stat <- round(MSE@SB_SBMSY[,,Yrs[1]:Yrs[2]],2) >= 1 & 
    round(MSE@F_FMSY[,,Yrs[1]:Yrs[2]],2) <= 1
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MSE) 
  PMobj@Mean <- calcMean(PMobj@Prob) 
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Status) <- 'PM'

Catch_ST <- function(MSE, Yrs=10) {
  Yrs <- ChkYrs(Yrs, MSE)
  
  PMobj <- new("PMobj")
  PMobj@Name <- "Average Short-Term Catch"
  
  PMobj@Caption <- paste0('Average Catch (Years ', Yrs[1], ' - ', Yrs[2], ')')
  PMobj@Stat <- MSE@Catch[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MSE) 
  PMobj@Mean <- calcMean(PMobj@Prob) |> round()
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Catch_ST) <- 'PM'

Catch_LT <- Catch_ST
formals(Catch_LT)$Yrs <- -10
class(Catch_LT) <- 'PM'