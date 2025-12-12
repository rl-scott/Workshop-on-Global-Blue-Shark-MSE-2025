


GetSSOutput <- function(stock, dir='SS3_Output') {
  path <- file.path(dir, stock)
  cli::cli_alert_info('Reading SS3 output from {.val {path}}')
  output <- r4ss::SS_output(path, verbose=FALSE, printstats=FALSE)
  class(output) <- 'ss3output'
  output
}

print.ss3output <- function(x, ...) {
  cli::cli_h1("SS3 Output List")
  cli::cli_text("A named list containing the SS3 output from Report file: {.file {x$inputs$repfile}}")
}

# Additional Functions used for the Blue Shark MSE Workshop
RequiredPackages <- data.frame(Package=c('pak',
                                         'r4ss', 
                                         'MSEtool',
                                         'SAMtool', 
                                         'DLMtool', 
                                         'openMSE',
                                         'Slick',
                                         'MSEextra',
                                         'spict'),
                               Version=c('0.9.0',
                                         '1.52', 
                                         '3.7.5', 
                                         '1.8.2', 
                                         '6.0.6',
                                         '1.0.1', 
                                         '1.0.1',
                                         '0.3.0',
                                         '1.3.8'))

LoadPackages <- function() {
  Packages <- RequiredPackages
  for (i in 1:nrow(Packages)) {
    pkg <- Packages$Package[i]
    version <- try(packageVersion(pkg), silent=TRUE)
    if (inherits(version, 'try-error')) {
      message("Installing package: ", pkg)
      if (pkg=='pak') {
        install.packages('pak')
      } else if (pkg=='r4ss') {
        pak::pkg_install('r4ss/r4ss')
      } else if (pkg=='Slick') {
        pak::pkg_install('blue-matter/Slick', dependencies=TRUE, upgrade=TRUE)
      } else if (pkg=='MSEextra') {
          pak::pkg_install('blue-matter/MSEextra')
      } else if (pkg=='spict') {
        pak::pkg_install('DTUAqua/spict/spict')
      } else {
        pak::pkg_install(pkg)
      }
    }
    
    if (packageVersion(pkg) < Packages$Version[i]) {
      message("Updating package: ", pkg)
      remove.packages(pkg)
      pak::pkg_install(pkg)
    }
    library(pkg, character.only=TRUE)
  }
  library(ggplot2)
}





Stocks <- function(dir='SS3_Output') {
  list.dirs(dir, recursive = FALSE, full.names = FALSE)
}
  



ModifyNaturalMortality_SS3 <- function(stock, delta) {
  
  Name <- paste0('M_', delta)
  
  if (!dir.exists('Alternative_OMs'))
    dir.create('Alternative_OMs')
  
  if (!dir.exists(file.path('Alternative_OMs', stock)))
    dir.create(file.path('Alternative_OMs', stock))
  
  base.dir <- file.path('SS3_Output', stock)
  dir <- file.path('Alternative_OMs', stock, Name)
  
  cli::cli_alert('Copying SS3 files to {.val {dir}}')
  
  if (!dir.exists(dir))
    dir.create(dir)
  
  fls <- list.files(dir)
  file.remove(file.path(dir, fls))
  
  dat <- r4ss::SS_readdat_3.30(file.path(base.dir, 'data.ss'), verbose = FALSE)
  ctl <- r4ss::SS_readctl_3.30(file.path(base.dir, 'control.ss'), datlist=dat, verbose = FALSE)
  starter <- r4ss::SS_readstarter(file.path(base.dir, 'starter.ss'), verbose = FALSE)
  
  cli::cli_alert('Modifying `M` from base value by a factor of {.val {delta}}')
  
  ind <- grepl('NatM', rownames(ctl$MG_parms)) |>  which()
  if (length(ind)>0) {
    for (i in seq_along(ind)) {
      ctl$MG_parms[ind[i], c("INIT", "PRIOR")] <- ctl$MG_parms[ind[i], c("INIT", "PRIOR")] * delta
      ctl$MG_parms[ind[i], c("PHASE")] <- -3 
    }
  }
  
  if (!is.null(ctl[['natM']])) {
    for (i in 1:nrow(ctl$natM)) {
      ctl$natM[i,] <- ctl$natM[i,] * delta
    }
  }
  
  starter$ctlfile <- 'control.ss'
  starter$datfile <- 'data.ss' 
  starter$init_values_src <- 0 # use control file for initial values
  starter$run_display_detail <- 0
  starter$cumreport <- 0
  
  SS_writectl_3.30(ctl, file.path(dir, 'control.ss'), verbose=F, overwrite=TRUE)
  SS_writedat_3.30(dat, file.path(dir, 'data.ss'), verbose=F, overwrite=TRUE)
  SS_writestarter(starter, file.path(dir), verbose=F, overwrite=TRUE)
  
  fls <- c('forecast.ss', 'ss.exe')
  file.copy(file.path(base.dir, fls),
            file.path(dir, fls))
  
  
  cli::cli_alert('Running SS3 in {.val {dir}} ...')
  r <- r4ss::run(dir, 'ss', show_in_console=FALSE, skipfinished =FALSE, extras="-nohess", verbose = FALSE)
  if (r == 'ran model')
    cli::cli_alert_success('SS3 run successful. To import OM use:\n {.var SS2OM(SSdir="{dir}", nsim=nsim, proyears = proyears, interval=interval)}')
}

ImputeIndices <- function(Data, MeanYears=3) {
  type <- 1 
  if (inherits(Data, 'Hist')) {
    type <- 2
    Hist <- Data
    Data <- Hist@Data
  }
  
  AddInd <- Data@AddInd
  dd <- dim(AddInd)
  
  TerminalValues <- AddInd[,,dd[3]]
  
  if (!any(is.na(TerminalValues)))
    return(Data)
  
  for (x in 1:nrow(TerminalValues)) {
    ind <- which(is.na(TerminalValues[x,]))
    for (i in seq_along(ind)) {
      meanValue <- tail(AddInd[x,ind[i],], MeanYears+1) |> mean(na.rm=TRUE)
      if (!is.finite(meanValue))
        meanValue <- NA
      AddInd[x,ind[i],dd[3]] <- meanValue
    }
  }
  
  Data@AddInd <- AddInd
  
  if (type==2) {
    Hist@Data <- Data
    return(Hist)
  }
  Data
}




MakeFactor <- function(x) {
  factor(x, ordered = TRUE, levels=unique(x))
}

Array2DF <- function(array) {
  if (!inherits(array, c('matrix', 'array'))) 
    cli::cli_abort('`array` in not class `matrix` or `array`')
  
  df <- array2DF(array)
  nms <- colnames(df)
  if ('Sim' %in% nms)
    df$Sim <- as.numeric(df$Sim)
  if ('Age' %in% nms)
    df$Age <- as.numeric(df$Age)
  if ('Class' %in% nms)
    df$Class <- as.numeric(df$Class)
  if ('Stock' %in% nms)
    df$Stock <- MakeFactor(df$Stock)
  if ('Fleet' %in% nms)
    df$Fleet <- MakeFactor(df$Fleet)
  # if ('MP' %in% nms)
  #   df$MP <- MakeFactor(df$MP)
  if ('Year' %in% nms)
    df$Year <- as.numeric(df$Year)
  if ('Value' %in% nms)
    df$Value <- as.numeric(df$Value)
  df
}


Save <- function(object, path=NULL, overwrite=FALSE, ...) {
  if (is.null(path))
    path <- tempfile()
  
  CreateDir(dirname(path))
  
  if (file.exists(path) && !overwrite)
    cli::cli_abort(c('File {.file {path}} already exists',
                     'i'='Use `overwrite=TRUE` to overwrite existing file'))  
  
  name <- deparse(substitute(object))
  cli::cli_alert_info('Saving {.val {name}} of class {.val {class(object)}} to {.file {path}}')
  
  saveRDS(object, path, ...)
  invisible(path)
}


CreateDir <- function(path) {
  paths <- strsplit(path, '/')[[1]]
  for (i in seq_along(paths)) {
    dir <- paste0(paths[1:i], collapse = '/')
    if (i==1) {
      dir <- paste0(dir,'/')
    }
    if (!dir.exists(dir))
      dir.create(dir)
  }
}

