wd <- 'C:/Users/KLN/Documents/projects/sermonDatabase/bible_coding'; setwd(wd)
url <- 'http://stats.stackexchange.com/questions/3539/inter-rater-reliability-for-ordinal-or-interval-data'
library(XLConnect)
library(irr)
# import data
c1.t <- readWorksheet(loadWorkbook(paste(wd,'/table1.xlsx',sep='')),sheet=1)
data1 <- c1.t[3:26,3:17]
c2.t <- readWorksheet(loadWorkbook(paste(wd,'/table2.xlsx',sep='')),sheet=1)
data2<- c2.t[4:27,3:14]
c3.t <- readWorksheet(loadWorkbook(paste(wd,'/table3.xlsx',sep='')),sheet=1)
data3 <- c3.t[3:26,3:11] 
# aggreement and iir for ordinal/interval data and k raters
irr.f <- function(data){
agree.l = list()
kendall.l <- list()
idx <- seq(1,ncol(data),by = 3)
for (i in  1:length(idx)){
  tmp <- data[,idx[i]:(idx[i]+2)]
  agree.l[[i]] <- agree(tmp)
  kendall.l[[i]] <- kendall(tmp, correct = F)
}
return(list('agreement' = agree.l, 'irr' = kendall.l))
}
### wrapper
res <- irr.f(data1)
res$agreement
res$irr
