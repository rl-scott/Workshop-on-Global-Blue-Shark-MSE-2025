
dd <- dim(Hist@Data@AddInd[1,,])
nIndices <- dd[1]

# ---- Empirical MPs ----

# see ?Emp for help documentation 


Emp1 <- Emp
formals(Emp1)$Inds <- 1:nIndices # use all indices
formals(Emp1)$I_wt <- rep(1, nIndices) # equally weighted
formals(Emp1)$delta_down <- c(0.01, 0.1) # maximum TAC decrease 10%
formals(Emp1)$delta_up <- c(0.01, 0.1) # maximum TAC increase 10%
class(Emp1) <- 'MP'

# Second variant
Emp2 <- Emp1
formals(Emp2)$delta_down <- c(0.01, 0.3) # maximum TAC decrease 30%
formals(Emp2)$delta_up <- c(0.01, 0.3) # maximum TAC increase 30%
formals(Emp2)$Ind_fac <- 1
class(Emp2) <- 'MP'


# ---- Model-Based MPs ----

# ... Additional arguments to pass to `spict`. See ?MSEextra::spict

Spict1 <- function(x, Data, I_wt = NA,
                   inp_args = list(priors=list(logr=c(log(0.8), 0.01, 1))),
                   MaxChange=0.2, ...) {
  
  # Generate a single index by weighted average of fleet-specific CPUEs:
  AddInd <-  Data@AddInd[x,,]
  nIndices <- nrow(AddInd)
  if (any(is.na(I_wt)))
    I_wt <- rep(1, nIndices)
  
  Ind <- apply(AddInd, 2, weighted.mean, w=I_wt, na.rm=TRUE)
  Data@Ind[x,] <- Ind # replace the simulated values in `Ind` with the CPUE index
  
  # Run the spict model
  runspict <- MSEextra::spict(x, Data, inp_args, ...)

  if (all(tail(!is.finite(runspict@F_FMSY)))) {
    # SP model failed to converge
    rec <- new("Rec")
    return(rec) # return empty Rec object (previous TAC will be used)
  }

  # apply a harvest control rule
  Rec <- SAMtool::HCR80_40MSY(runspict)
  TAC <- Rec@TAC
  
  # apply a constraint on the maximum absolute change in TAC
  TACratio <- TAC/Data@MPrec[x]

  if (TACratio > 1+MaxChange) 
    TAC <- Data@MPrec[x] * 1+MaxChange
  
  if (TACratio < 1-MaxChange) 
    TAC <- Data@MPrec[x] * 1-MaxChange
  
  
  Rec@TAC <- TAC
  
  Rec
}
class(Spict1) <- 'MP'


# different HCR - Fishes at 0.5FMSY
Spict2 <-  function(x, Data, I_wt = NA,
                    inp_args = list(priors=list(logr=c(log(0.8), 0.01, 1))),
                    MaxChange=0.2, ...) {
  
  # Generate a single index by weighted average of fleet-specific CPUEs:
  AddInd <-  Data@AddInd[x,,]
  nIndices <- nrow(AddInd)
  if (any(is.na(I_wt)))
    I_wt <- rep(1, nIndices)
  
  Ind <- apply(AddInd, 2, weighted.mean, w=I_wt, na.rm=TRUE)
  Data@Ind[x,] <- Ind # replace the simulated values in `Ind` with the CPUE index
  
  # Run the spict model
  runspict <- MSEextra::spict(x, Data, inp_args, ...)
  
  if (all(tail(!is.finite(runspict@F_FMSY)))) {
    # SP model failed to converge
    rec <- new("Rec")
    return(rec) # return empty Rec object (previous TAC will be used)
  }
  
  # apply a harvest control rule
  Rec <- SAMtool::HCR_MSY(runspict, MSYfrac=0.5)
  TAC <- Rec@TAC
  
  # apply a constraint on the maximum absolute change in TAC
  TACratio <- TAC/Data@MPrec[x]
  if (TACratio > 1+MaxChange) 
    TAC <- Data@MPrec[x] * 1+MaxChange
  
  if (TACratio < 1-MaxChange) 
    TAC <- Data@MPrec[x] * 1-MaxChange
  
  Rec@TAC <- TAC
  Rec
}
class(Spict2) <- 'MP'

