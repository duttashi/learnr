## My R profile
## Location of R profile file in Windows OS is at "C:\Program Files\R\R-3.5.1\etc" where `R-3.5.1` should be your current R version.
## Actual name of this file is, `Rprofile.site`.
## To edit this file, browse to the file location and open the file with `Notepad++` in administrative mode. Copy paste this file into your `Rprofile.site` file.
## last updated on: 22/Sep/2018 10:54am MYT

## set up local cran mirror native to your current geographic location
local({r <- getOption("repos")
      r["CRAN"] <- "https://wbc.upm.edu.my/cran/"
      options(repos=r)}
	  )
## set up the way R prompt should look
options(prompt="R> ", digits=4)
## set up the default application program to open pdf files
options("pdfviewer"="foxit")

## set up function to load libraries or display friendly message on startup
.First <- function(){
 library(ggplot2)
 cat("\nWelcome Ashish at", date(), "\n")
 ## show message on which libraries have been loaded on startup. 
 cat("\nAshish, I've loaded the ggplot2 library for you.","\n")
}
## set up function to show friendly message on exit
.Last <- function(){ 
 cat("\nGoodbye Ashish at ", date(), "\n")
  if (!any(commandArgs()=='--no-readline') && interactive()){
                require(utils)
                try(savehistory(Sys.getenv("R_HISTFILE")))
        }
}


## Reference: SO post https://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile