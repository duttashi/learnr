# Objective: Connect RStudio with local instance of MS SQL Server
# Reference: http://db.rstudio.com/odbc/

# Script create date: 28/5/2018
# Script update date: 21/6/2018

# install the required package if not already installed
install.packages("odbc", dependencies = TRUE)
install.packages("DBI", dependencies = TRUE)

# load the package
library(odbc) # for dbConnect()
library(DBI) # for dbReadTable()
# create a database connection and open it.
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "ASHOO-PC\\SQLEXPRESS",
                 Database = "DB2018")

# list all tables in the database
dbListTables(con)
# List tables beginning with g
dbListTables(con, table_name = "g%")

rs<- dbGetQuery(con, "SELECT * from groceries")

# Reading: dbReadTable() will read a full table into an R data.frame().
grocery.data<- dbReadTable(con, 'groceries')

# close the database connection

