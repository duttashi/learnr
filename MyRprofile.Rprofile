
## local creates a new, empty environment
## This avoids polluting the global environment with
## the object r
local({
  r = getOption("repos")             
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
})

#############
## OPTIONS ##
#############

# c'mon now
options(stringsAsFactors=FALSE)

options(max.print=100)

options(scipen=10)

options(editor="vim")

options(warn = 2)

options(useFancyQuotes = FALSE)

options(prompt="R> ", digits=4, show.signif.stars=FALSE)

options(Ncpus = 2) #ncpus is for improving the package installation speed. See this post http://blog.jumpingrivers.com/posts/2017/speed_package_installation/

# I'll keep this here for posterity but
# I keep it commented because ever since
# 1st grade I used number of stars as a
# proxy for my worth as a person
# options(show.signif.stars=FALSE)

# ain't nobody got time for Tk to load
options(menu.graphics=FALSE)

# the non-standard continuation prompt
# helps me tell when I accidentally fail
# to close a paren, etc..
options(prompt="> ")
options(continue="... ")

# I like to keep my console width to
# 80 characters. Works well with reading
# and tmux panes
options(width = 80)

# Override q() to not save by default.
# Same as saying q("no")
q <- function (save="no", ...) {
  quit(save=save, ...)
}

# tab-complete package names
utils::rc.settings(ipck=TRUE)

.First <- function(){
  if(interactive()){
    library(utils)
    # puts a timestamp in my command history
    # file if R_HISTFILE is in environment
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
    
  }
}

.Last <- function(){
  if(interactive()){
    # saving my interactive command history
    # is a life saver and saved me many hours
    # of work. This will save my history to
    # the file that the environment variable
    # $R_HISTFILE points to, if it doesn't exist,
    # use ~/.RHistory
    hist_file <- Sys.getenv("R_HISTFILE")
    if(hist_file=="") hist_file <- "~/.RHistory"
    savehistory(hist_file)
  }
}

# get noisy package imports to shut up
#   we have to jump through hoops to get the
#   call to "library()" to work because it
#   will try to load a package even if it is not
#   a string literal
sshhh <- function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))
}

# list of packages to auto-load if interactive
auto.loads <-c("tidyverse")

# auto-load dplyr and ggplot2
if(interactive()){
  invisible(sapply(auto.loads, sshhh))
}


######################
## CUSTOM FUNCTIONS ##
######################


# better defaults for write.csv
# write.csv <- function(adataframe, filename, ...){
#   outfile <- file(filename, "w", encoding="UTF-8")
#   utils::write.csv(adataframe, outfile, row.names=FALSE, ...)
#   close(outfile)
# }


.env <- new.env()

# stolen from Stephen Turner (http://gettinggeneticsdone.blogspot.com.es/2013/07/customize-rprofile.html)
.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

# stolen from Dason Kurkiewicz (https://github.com/Dasonk/Dmisc/blob/master/R/unfactor.R)
.env$unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}

# provides a way to open a data frame in a spreadsheet application (Unix only!)
.env$look <- function(df, n=99, sleep=5, short.sleep=1){
  thename <- deparse(substitute(df))
  if(n > 0 && n < nrow(df)){
    df <- df[1:n,]
  }
  fname <- paste0("/tmp/", thename, ".csv")
  write.csv(df, fname)
  # give R time to finish writing file before trying to open it
  Sys.sleep(short.sleep)
  system(paste0("open ", fname))
  # give the application time to open it before destroying file
  Sys.sleep(sleep)
  system(paste0("rm ", fname))
}

attach(.env)



# this is also useful for warning myself if I meant
# to run vanilla R
message("\n*** Successfully loaded .Rprofile ***\n")