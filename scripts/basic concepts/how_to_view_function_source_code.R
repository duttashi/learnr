# Reference: https://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function
# To view the source code of function
getAnywhere(apply)
# To redirect the source code of a function to an external file
capture.output(getAnywhere(colnames), file = "scripts/basic concepts/source_code.txt")
