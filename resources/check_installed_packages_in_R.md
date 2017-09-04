To check for installed packages in R, I found this handy script from [here](https://www.r-bloggers.com/list-of-user-installed-r-packages-and-their-versions/)

  > ip <- as.data.frame(installed.packages()[,c(1,3:4)])
  
  > rownames(ip) <- NULL
  
  > ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
  
  > print(ip, row.names=FALSE)
  
                    Package     Version
                      abind       1.4-5
                    acepack       1.4.1
                       ade4       1.7-8
                     Amelia       1.7.4
                  animation         2.5
