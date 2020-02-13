#.dat files
senic<-read.table("~/Dropbox/Teaching/STAT525-2020/stat525/content/project/data/senic.dat")
insurance<-read.table("~/Dropbox/Teaching/STAT525-2020/stat525/content/project/data/insurance.dat", header=T)

#.txt files
flovote<-read.table("~/Dropbox/Teaching/STAT525-2020/stat525/content/project/data/flovote.txt")

#.xls files
install.packages("gdata")
library(gdata)
crime<-read.xls("~/Dropbox/Teaching/STAT525-2020/stat525/content/project/data/crime.xls")

