# load gglasso library
library(gglasso)
library(data.table)
library(glmnet)
library(ggplot2)

# load data
# ytable has the average outcome of samples grouped by strains
qtltable <- fread("C:/Users/Jennifer Chen/Desktop/honors project/data/qtltable.csv", header = TRUE)
x <- as.matrix(qtltable[,-1])
ytable <- fread("C:/Users/Jennifer Chen/Desktop/honors project/data/ytable_bw.csv", header = TRUE) 
yBW <- as.matrix(ytable$Dosing.BW)
ytable <- fread("C:/Users/Jennifer Chen/Desktop/honors project/data/ytable.csv", header = TRUE)
yAST <- as.matrix(ytable$AST)
yALT <- as.matrix(ytable$ALT)