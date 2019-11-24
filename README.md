R Plot created by Chuxin
# Building Radar Chart to Compare Car Type Specification
Using Radar Chart to compare two different car type's specification (such as Standard Keyless Entry system or Not Standard Keyless Entry system). The chart can help customers to learn the difference in a pair of competitor quickly and visually. It can be also used in other mechandises that have multiple equipments or devices.


## What can this plot do?

*  Compare a pair of competitors's specification and list their difference
* If there are some subclasses in one classfication (e.g. In the Seat Material class, we have Fabric material and Imitation leather), the programm would also show these subclasses difference.(e.g. We could check that Car Model A has Fabric seat material but Model B has Imitation leather.)
*  Viewing their difference visually
* Automatically Spilt by the number of difference to omit too much information on one graph:
    * If the number of difference on equipments over 24, it will automatically diverse to multiple parts.
    * When over 24, every 20 items would output as one part of graph.
    * If the last part of every 20 segmentations smaller than 4, the resting items would merge into the last graph. (eg. Having 63 items need to specify, it will spilt into 20-20-23, but not 20-20-20-3, which would be too narrow to a graph.)
* The final graph would on a certain order to make the visuialization more clear (e.g. The equipments that A model has will in the former, then will be the equipments that B model has. )
* Able to batch output grouping by a long lists of pair competitors.

Sample Plot:

![image](https://github.com/czhang02-su/Image-set/blob/master/sample%20graph.jpg)

## Sample Data

Sheet1 Competitor List: A-MODEL2 and B-MODEL2 is a pair of competitor which need to compare. 

![image](https://github.com/czhang02-su/Image-set/blob/master/competitor%20data.jpg)

Sheet2 Specification of Car Models: Listing all equipments of this car
* '-' means NOT STANDARD
* '○' means OPTIONAL
* '●' means STANDARD

![image](https://github.com/czhang02-su/Image-set/blob/master/specification%20dt.jpg)

## Description
### Preparing Data for Ploting

Comparing Difference
```R
A1 <- read_xlsx("D:\\Radar Sample.xlsx",sheet = 1) #Competitor list
A <- read_xlsx("D:\\Radar Sample.xlsx",sheet = 2) #Specification Data
M1name <- A1[['A_MODEL2']][i] #i is the row number in a for loop
M2name <- A1[['B_MODEL2']][i]
model1 <- data.frame(A[which(A$MODEL2 == M1name),]) #Selecting the specification
model2 <- data.frame(A[which(A$MODEL2 == M2name),])
modelcom <- bind_rows(model1[nrow(model1),],model2[nrow(model2),]) #Bind them in a dataset

#Building a function to compare A Model and B Model difference
takesame <- function(df){
  dflst <- c()
  for (i in (1:ncol(df))){
    if (df[1,i] != df[2,i]){
      dflst <- append(dflst,i) #dflst is a list that to select different column
    }
  }
  df <- df[,dflst]
  return(df)
}
modeldf <- takesame(modelcom)

#expc.lst is the list that select the columns that are not equipment items
expc.lst <- c("TYPE1","BRAND","MODEL","Model2_ENG",
              "MODEL2","MSRP","OEM","FBRAND_ENG","FMODEL_ENG",
              "FBRAND","FMODEL","Model Year")

#Only select equipment coulmns from expc.lst
  for (a in expc.lst){
    if (is.na(match(a,colnames(modeldf))) == FALSE){
      modeldf <- modeldf %>% select(-a)
    }
  }


```
modeldf
![image](https://github.com/czhang02-su/Image-set/blob/master/modeldf.jpg)

### To deal with some sub classes
We can see some of coulmns, like Back Front Handrail has other sub-equipments value rather than simple '-','○','●'. 

insert back front handrail 

In this siuation, we will automatically seperate them to twwo columns: Back Front Handrail-Front and Back Front Handrail-Back. Finally we should get a data set that only include '-','○','●' these three values. 

```R
 modeldf.a <- data.frame(modeldf[1,,drop = FALSE],stringsAsFactors = FALSE) #Handling row by row for convenience

#Build a function that select those columns having value other than '-','○','●'.
 count <- function(df){
  c <- c()
  for (a in (1:ncol(df))){
    if (df[1,a] != '○' && df[1,a] != '-' && df[1,a] != '●'){
      c <- c(c,a)
    }
  }
  return(c)
}
 
  c <- count(modeldf.a) #Run the function

#Build a function choosing 
  split <- function(df,c){
  for (a in c){
    if (grepl( ',',df[1,a],fixed=TRUE)){ #To check if it has comma that including multiple subclasses
      b <- strsplit(df[1,a],',')
      for (i in (1:length(b[[1]]))){
        d <- b[[1]][i]
        d1 <- gsub('○','',d)
        d1 <- gsub('●','',d1) #extract the subclass name
        colNam <- paste(colnames(df[a]),d1,sep='-') #Combine the subclass name to the main class
        df <- df %>% mutate(colNam := d) #Added the new column to the end of the data
        f <- ncol(df)
        names(df)[f] <- colNam #rename the new-added column
      }
    } else {
      d <- df[1,a]
      d1 <- gsub('○','',d)
      d1 <- gsub('●','',d1)
      colNam <- paste(colnames(df[a]),d1,sep='-')
      df <- df %>% mutate(colNam := d)
      f <- ncol(df)
      names(df)[f] <- colNam
    }
  }
  return(df)
}

   modeldf.a <- split(modeldf.a,c) #Run the function

  if (is.null(c) == FALSE){
    modeldf.a <- data.frame(modeldf.a[,-c,drop = FALSE]) #Delete columns having other value, since we've already added them with subclasses name
  }

  #Build a function that only leave value of '○','●'.
omitchr <- function(df){
  for (a in (1:ncol(df))){
    if (grepl( '○',df[1,a],fixed=TRUE)){
      df[1,a] <- '○'
    }
    if (grepl( '●',df[1,a],fixed=TRUE)){
      df[1,a] <- '●'
    }
  }
  return(df)
}
  modeldf.a <- omitchr(modeldf.a) #Run the function

```
Run the same steps as the second row. 
```R
  modeldf.b <- data.frame(modeldf[2,,drop = FALSE],stringsAsFactors = FALSE)
  c <- count(modeldf.b)
  modeldf.b <- fac.to.chr(modeldf.b)
  modeldf.b <- split(modeldf.b,c)
  if (is.null(c) == FALSE){
    modeldf.b <- data.frame(modeldf.b[,-c,drop = FALSE])
  }
  modeldf.b <- omitchr(modeldf.b)
```

Set other values such as null value as '-', which represents NOT STANDARD
```R
  modeldf.g[is.na(modeldf.g)] <- '-'
  modeldf.g[modeldf.g=='']<-'-'
  modeldf.g <- takesame(modeldf.g) #Delete same value again
```
modeldf.g
![image](https://github.com/czhang02-su/Image-set/blob/master/modeldf.g.jpg)

Convert the value to the number which can reflect on the radar chart.
```R
#Creating Function, "●" = 2, "○" = 1, "-" = 0
outputnum <- function(df){
  df2 <- data.frame(matrix(NA,ncol=0,nrow=2))
  for (a in (1:ncol(df))){
    countN <- paste(colnames(df)[a],sep='')
    value1 <- str_count(df[1,a],"●")*2+ str_count(df[1,a],"○")*1+ str_count(df[1,a],"-")*0
    value2 <- str_count(df[2,a],"●")*2+ str_count(df[2,a],"○")*1+ str_count(df[2,a],"-")*0
    value <- c()
    value <- append(value,value1)
    value <- append(value,value2)
    df2 <- df2 %>% mutate(countN := value)
    i <- ncol(df2)
    names(df2)[i] <- countN
  }
  return(df2)
}

modeldf.g2 <- outputnum(modeldf.g) #Run the function
```

![image](https://github.com/czhang02-su/Image-set/blob/master/modeldf.g2.jpg)

Reorder the data
```R
  modeldf.g3 <- cbind(modeldf.g3,select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '2' & modeldf.g2[2,] == '1')]))
  modeldf.g3 <- cbind(modeldf.g3, select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '2' & modeldf.g2[2,] == '0')]))
  modeldf.g3 <- cbind(modeldf.g3,select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '1' & modeldf.g2[2,] == '0')]))
  modeldf.g3 <- cbind(modeldf.g3,select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '1' & modeldf.g2[2,] == '2')]))
  modeldf.g3 <- cbind(modeldf.g3,select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '0' & modeldf.g2[2,] == '1')]))
  modeldf.g3 <- cbind(modeldf.g3,select(modeldf.g2,names(modeldf.g2)[which(modeldf.g2[1,] == '0' & modeldf.g2[2,] == '2')]))
  
  rownames(modeldf.g3) <- c(M1name,M2name) #Change row name to model name
```
![image](https://github.com/czhang02-su/Image-set/blob/master/modeldf.g3.jpg)

### Draw the plot

Building Radar plot function
```R
#Set the min/max value to build the base of radar chart
radar.dt <- function(dt){
  dt <- rbind(rep(2,20),
              rep(0,20),
              rep(1,20),rep(2,20),
              dt)
  return(dt)
}
#Radar plot function
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

```
Set if/else condition to adapt different number of columns, automatically seperating parts by every 20 columns. 

```R
  if (ncol(modeldf.g3) > 24){ # If the number of difference bigger than 24
    num <- floor(ncol(modeldf.g3)/20) #Seperate by every 20
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
    #If the remainder part is smaller than 4, then combine in the last part
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
```

![image](https://github.com/czhang02-su/Image-set/blob/master/AuchanKai%20Chen%20M50VSpecification%20Comparison--Part1.png)
![image](https://github.com/czhang02-su/Image-set/blob/master/AuchanKai%20Chen%20M50VSpecification%20Comparison--Part2.png)












