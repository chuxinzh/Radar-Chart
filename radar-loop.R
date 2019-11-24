library(data.table)
library(plyr)
library(dplyr)
library(ggraph)
library(stringr)
library(fmsb)
library(scales)
library(readxl)

##############################

takesame <- function(df){
  dflst <- c()
  for (i in (1:ncol(df))){
    if (df[1,i] != df[2,i]){
      dflst <- append(dflst,i)
    }
  }
  df <- df[,dflst]
  return(df)
}

count <- function(df){
  c <- c()
  for (a in (1:ncol(df))){
    if (df[1,a] != '¡ð' && df[1,a] != '-' && df[1,a] != '¡ñ'){
      c <- c(c,a)
    }
  }
  return(c)
}


fac.to.chr <- function(df){
  for (a in (1:ncol(df))){
    if (is.factor(df[[a]])){
      df[[a]] <- as.character(df[[a]])
    }
  }
  return(df)
}


split <- function(df,c){
  for (a in c){
    if (grepl( ',',df[1,a],fixed=TRUE)){
      b <- strsplit(df[1,a],',')
      for (i in (1:length(b[[1]]))){
        d <- b[[1]][i]
        d1 <- gsub('¡ð','',d)
        d1 <- gsub('¡ñ','',d1)
        colNam <- paste(colnames(df[a]),d1,sep='-')
        df <- df %>% mutate(colNam := d)
        f <- ncol(df)
        names(df)[f] <- colNam
      }
    } else {
      d <- df[1,a]
      d1 <- gsub('¡ð','',d)
      d1 <- gsub('¡ñ','',d1)
      colNam <- paste(colnames(df[a]),d1,sep='-')
      df <- df %>% mutate(colNam := d)
      f <- ncol(df)
      names(df)[f] <- colNam
    }
  }
  return(df)
}

omitchr <- function(df){
  for (a in (1:ncol(df))){
    if (grepl( '¡ð',df[1,a],fixed=TRUE)){
      df[1,a] <- '¡ð'
    }
    if (grepl( '¡ñ',df[1,a],fixed=TRUE)){
      df[1,a] <- '¡ñ'
    }
  }
  return(df)
}

outputnum <- function(df){
  df2 <- data.frame(matrix(NA,ncol=0,nrow=2))
  for (a in (1:ncol(df))){
    countN <- paste(colnames(df)[a],sep='')
    value1 <- str_count(df[1,a],"¡ñ")*2+ str_count(df[1,a],"¡ð")*1+ str_count(df[1,a],"-")*0
    value2 <- str_count(df[2,a],"¡ñ")*2+ str_count(df[2,a],"¡ð")*1+ str_count(df[2,a],"-")*0
    value <- c()
    value <- append(value,value1)
    value <- append(value,value2)
    df2 <- df2 %>% mutate(countN := value)
    i <- ncol(df2)
    names(df2)[i] <- countN
  }
  return(df2)
}

tochr <- function(dt){
  for (a in (1:ncol(dt))){
    if (is.factor(dt[,a]) == TRUE){
      dt[,a] <- as.character(dt[,a])
    }
  }
  return(dt)
}

radar.dt <- function(dt){
  dt <- rbind(rep(2,20),
              rep(0,20),
              rep(1,20),rep(2,20),
              dt)
  return(dt)
}

radar <- function(dt){
  par(bg = "#D5E4EB")
  par(mar = c(2,0,2,0))
  radarchart(dt, axistype=1, seg = 2, 
             #custom polygon
             pcol=c("#808080","#808080",NA,NA), 
             pfcol=c(NA,NA, rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)), 
             plwd=c(1,1.8,1,1) , plty=c(2,1,1,1), pty=c(1,16,32,32),
             #custom the grid
             cglcol="grey", cglty=3, axislabcol="grey50", caxislabels=c('-','Optional','Standard'), cglwd=0.9,
             #custom labels
             vlcex=0.8
  )
  
  title(main = title,adj = 0.1,cex.main=1.5)
  
  # Add a legend
  legend(x=1.1, y=1, legend = rownames(dt[-c(1,2,3,4),]), bty = "n", pch=20 , col=c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)) , text.col = "black", cex=0.8, pt.cex=3)
  
}

expc.lst <- c("TYPE1","BRAND","MODEL","Model2_ENG",
              "MODEL2","MSRP","OEM","FBRAND_ENG","FMODEL_ENG",
              "FBRAND","FMODEL","Model Year")

#################
setwd("D:\\image\\")

A1 <- read_xlsx("D:\\Radar Sample.xlsx",sheet = 1)
A <- read_xlsx("D:\\Radar Sample.xlsx",sheet = 2)

A1 <- tochr(A1)

timestart <- Sys.time()
outputdata <- data.frame()
for (i in (1:nrow(A1))){
  M1name <- A1[['A_MODEL2']][i]
  M2name <- A1[['B_MODEL2']][i]
  model1 <- data.frame(A[which(A$MODEL2 == M1name),])
  model2 <- data.frame(A[which(A$MODEL2 == M2name),])
  M1name <- model1[['Model2_ENG']][1] #Change to ENG NAME
  M2name <- model2[['Model2_ENG']][1]
  M1name.title <- model1[['FMODEL_ENG']][1]
  M2name.title <- model2[['FMODEL_ENG']][1]
  modelcom <- bind_rows(model1[nrow(model1),],model2[nrow(model2),])
  modelcom <- fac.to.chr(modelcom)
  modelcom[is.na(modelcom)] <- ''
  modelcom <- modelcom[, colSums(is.na(modelcom)) != nrow(modelcom)]
  
  modeldf <- takesame(modelcom)
  
  for (a in expc.lst){
    if (is.na(match(a,colnames(modeldf))) == FALSE){
      modeldf <- modeldf %>% select(-a)
    }
  }
  if (length(modeldf) == 0){
    next
  }
  
  for (h in (1:nrow(modeldf))){
    for (s in (1:ncol(modeldf))){
      modeldf[h,s] <- gsub("[0-9]{4,}", "", modeldf[h,s])
    }
  }
  
  modeldf.a <- data.frame(modeldf[1,,drop = FALSE],stringsAsFactors = FALSE)
  c <- count(modeldf.a)
  modeldf.a <- fac.to.chr(modeldf.a)
  modeldf.a <- split(modeldf.a,c)
  if (is.null(c) == FALSE){
    modeldf.a <- data.frame(modeldf.a[,-c,drop = FALSE])
  }
  modeldf.a <- omitchr(modeldf.a)
  
  modeldf.b <- data.frame(modeldf[2,,drop = FALSE],stringsAsFactors = FALSE)
  c <- count(modeldf.b)
  modeldf.b <- fac.to.chr(modeldf.b)
  modeldf.b <- split(modeldf.b,c)
  if (is.null(c) == FALSE){
    modeldf.b <- data.frame(modeldf.b[,-c,drop = FALSE])
  }
  modeldf.b <- omitchr(modeldf.b)
  
  modeldf.g <- bind_rows(modeldf.a,modeldf.b)
  
  modeldf.g[is.na(modeldf.g)] <- '-'
  modeldf.g[modeldf.g=='']<-'-'
  modeldf.g <- takesame(modeldf.g)
  if (length(modeldf.g) < 3){
    modeldf$MODEL2 <- c(M1name,M2name)
    outputdata <- rbind.fill(outputdata,modeldf)
    next
  }
  modeldf.g2 <- outputnum(modeldf.g)
  modeldf.g2 <- takesame(modeldf.g2)
  modeldf.g3 <- data.frame(matrix(numeric(0),nrow=2))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '2' & modeldf.g2[2,] == '1')]))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '2' & modeldf.g2[2,] == '0')]))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '1' & modeldf.g2[2,] == '0')]))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '1' & modeldf.g2[2,] == '2')]))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '0' & modeldf.g2[2,] == '1')]))
  modeldf.g3 <- cbind(modeldf.g3,
                      select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '0' & modeldf.g2[2,] == '2')]))
  
  rownames(modeldf.g3) <- c(M1name,M2name)
  
  if (ncol(modeldf.g3) > 24){
    num <- floor(ncol(modeldf.g3)/20)
    e <- 1
    startcol <- 0
    for (c in (1:num)){
      endcol <- c * 20
      title <- paste(M1name.title,'-',M2name.title,'-Specification Comparison-','Part',c,sep='')
      filename <- gsub("[[:punct:]]", "", paste(M1name.title,'-',M2name.title,'-Specification Comparison',sep=''))
      filename <- paste(filename,'-',A1$ID[[i]],'-Part',c,".png",sep="")
      modeldf.g4 <- modeldf.g3[,((startcol+1):endcol)]
      modeldf.g4 <- radar.dt(modeldf.g4)
      png(filename,width = 1500, height = 1000,res=100)
      radar(modeldf.g4)
      dev.off()
      startcol <- e * 20
      e <- e + 1
    }
      startcol <- startcol - 20
      diff <- ncol(modeldf.g3) - endcol
      if (diff > 4){
        c <- c+1
        title <- paste(M1name.title,'-',M2name.title,'-Specification Comparison-','Part',c,sep='')
        filename <- gsub("[[:punct:]]", "", paste(M1name.title,'-',M2name.title,'-Specification Comparison',sep=''))
        filename <- paste(filename,'-',A1$ID[[i]],'-Part',c,".png",sep="")
        modeldf.g4 <- modeldf.g3[,((endcol+1):ncol(modeldf.g3))]
        modeldf.g4 <- radar.dt(modeldf.g4)
        png(filename,width = 1500, height = 1000,res=100)
        radar(modeldf.g4)
        dev.off()
      } else{
        title <- paste(M1name.title,'-',M2name.title,'-Specification Comparison-','Part',c,sep='')
        filename <- gsub("[[:punct:]]", "", paste(M1name.title,'-',M2name.title,'-Specification Comparison',sep=''))
        filename <- paste(filename,'-',A1$ID[[i]],'-Part',c,".png",sep="")
        modeldf.g4 <- modeldf.g3[,((startcol+1):ncol(modeldf.g3))]
        modeldf.g4 <- radar.dt(modeldf.g4)
        png(filename,width = 1500, height = 1000,res=100)
        radar(modeldf.g4)
        dev.off()
      }
      print(paste(M1name,M2name,'Total',c,'Part',sep=' '))
  } else {
    title <- paste(M1name.title,'-',M2name.title,'-Specification Comparison',sep='')
    filename <- gsub("[[:punct:]]", "", paste(M1name.title,'-',M2name.title,'-Specification Comparison',sep=''))
    filename <- paste(filename,'-',A1$ID[[i]],".png",sep="")
    modeldf.g4 <- radar.dt(modeldf.g3)
    png(filename,width = 1500, height = 1000,res=100)
    radar(modeldf.g4)
    dev.off()
    print(paste(M1name,M2name,sep='-'))
    }
}
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
  













