
filenames = dir(pattern="*multi.csv")
for(i in 1:length(filenames))
{
  
  test1<-read.csv2(file=filenames[i], header = T, fill = T, sep=",")
  test2<-reshape(test1,idvar=c("Well","Well.Position"),timevar="Cycle.Number",direction="wide")
  resolved="Resolved"
  newfn=paste(resolved,filenames[i])
  write.csv(test2,newfn,row.names = FALSE) 
  
  
}

