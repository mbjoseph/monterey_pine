
data <- read.csv('pinedataD.csv',header=TRUE,sep=",")

data <- data[data$spp == 'P',] #remove nonpines
data <- data[data$yr <= data$YearDead,] #remove observations of trees already dead

anndata <- NULL # to store annual transitions
sites <-  unique(data$Site)

sapply(data, class)
years = sort(unique(data$yr))
site_tagnum=unique(data$site_tag)

yrcode = data$yr-2001 #this is a number from 1 to the last year (2014)

fatemx = matrix(0,length(site_tagnum),max(yrcode)) #matrix of the data
#sitetagdata=as.factor(matrix(NA,length(site_tagnum),2))
totnn=length(data[,1]) # number lines
#totnn=10
Line = 1
for (nn in 1:totnn) {
print(c(nn,Line))
  id = data$site_tag[nn]
  yy = yrcode[nn]
  if (data$yr[nn] <data$YearDead[nn]) { fatemx[Line,yy] = data$tdbh[nn]} 
  
  if (data$yr[nn] ==data$YearDead[nn]) { fatemx[Line,yy] = -200 }
  if (identical(data$site_tag[nn+1], data$site_tag[nn]) == FALSE) {
    Line = Line +1}
  #print(identical(data$site_tag[nn+1], data$site_tag[nn]))
}

