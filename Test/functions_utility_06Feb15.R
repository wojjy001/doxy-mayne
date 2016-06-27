
#Extension for model directories made by WfN
  dir.extension <- ".nm7"
 

#################################################################################################
#Metadata functions
#---------------------------------------------------------------  
#Define metadata for Script - copy and paste 

# #Comment
metacomment <- NULL
metacomment <-  "Pimobendan in dogs"

# #Generating script - this is added using a macro defined in Notepad++ - invoked using ctl-alt-M
metafilepath <- NULL
metafilepath <- "D:/Foster/Canine_Pimobendan/PK_Modelling/Process_VPC_PK_all_data.R"
   scriptname <- gsub(".R","",basename(metafilepath))
   working.dir <- dirname(metafilepath)

# #Last run-time of script
metadatetime <- NULL
metadatetime <- Sys.time()

metadata1 <- paste("#Comment: ",metacomment,sep="")
metadata2 <- paste("#Generating script: ",metafilepath,sep="")
metadata3 <- paste("#Script run: ",metadatetime,sep="")


#---------------------------------------------------------------  
#Writing metadata to *.csv files

#Function to write a numeric dataframe with metadata commented out using # (the default comment character)
  writem.csv <- function(df,filename,...)
    {
     write(c(metadata1,metadata2,metadata3),file=filename)
     write("",file=filename, append=T)
     names(df)[1] <- paste(",",names(df)[1])  #insert comma as space for row names column
     write.table(df, filename, append=TRUE, sep=",", quote=F)
    }

  
  

#---------------------------------------------------------------  
#Reading metadata in *.csv files
#Default comment character is "#"  

#Function to read a numeric dataframe with metadata commented out using # (the default comment character)
  readm.csv <- function(filename,...)
    {
     df <- read.table(filename, sep=",", header=T)
     df
    }

 
#---------------------------------------------------------------  
#Function to write a numeric dataframe in with numbers trimmed to 3 significant figures
#Function to check if something is numeric, and if it is format as a character for a table

#is.numeric tests a whole vector for being numeric
#need a function to test a whole vector and see if it is all whole numbers
  is.wholenumber <- function(x) 
   {
    tol <- .Machine$double.eps^0.5 
    result <- (FALSE %in% (abs(x - round(x)) < tol))
    !result
   }

 
 formatT <- function(x) 
   {
   x <- as.character(x)  #factors to character
   if(is.na(as.numeric(x))==F) x <- as.numeric(x)  #resurrect any numbers from characters
     if (is.numeric(x)==T) {  #format any resurrected numbers
      if (is.wholenumber(x)==F)  #leave whole numbers alone
       {
        x <- formatC(signif(x,digits=3), digits=3, format="fg", flag="#")
        x  <- gsub("[.]*$", "", x)  #remove trailing decimal place
        }} else x <- x  #or leave alone

     x <- as.character(x)
     x
   }



  write3.csv <- function(df,filename,...)
    {
      #colwise is from plyr
      df <- colwise(formatT)(df)
      write.csv(df,file=filename, row.names=F)
    }

 
#---------------------------------------------------------------  
#Function to write a numeric dataframe in with  metadata and numbers trimmed to 3 significant figures
   writem3.csv <- function(df,filename,...)
     {
       #colwise is from plyr
       df <- colwise(formatT)(df)
       writem.csv(df,file=filename)
     }


 
#---------------------------------------------------------------  
to.png <- function(plotobj.in, maintext.in, pointsizein=14)
 {
  png.file.name <- paste(maintext.in,".png",sep="")
  png(file=png.file.name, width=900, height=700, pointsize=pointsizein)
  print(plotobj.in)
  
  #Stamp the plot
  grid.text(paste(metadata2,"  #Filename: ",maintext.in,".png  ",metadata3, sep=""),
            x = unit(0.025,"npc"), y = unit(0.015, "npc"),
            just = "left", gp = gpar(col = "black", fontsize = 8))

  dev.off()
 }
 
to.png.sqr <- function(plotobj.in,maintext.in,pointsizein=14)
 {
  png.file.name <- paste(maintext.in,".png",sep="")
  png(file=png.file.name, width=650, height=500, pointsize=pointsizein)
  print(plotobj.in)
   
  #Stamp the plot
  grid.text(paste(metadata2,"  #Filename: ",maintext.in,".png  ",metadata3, sep=""),
            x = unit(0.025,"npc"), y = unit(0.015, "npc"),
            just = "left", gp = gpar(col = "black", fontsize = 8))
   
  dev.off()
 }    


to.png.tiny <- function(plotobj.in,maintext.in,pointsizein=12)
 {
  png.file.name <- paste(maintext.in,".png",sep="")
  png(file=png.file.name, width=250, height=250, pointsize=pointsizein)
   print(plotobj.in)
   
  #Stamp the plot
  #grid.text(paste(metadata2,"  #Filename: ",maintext.in,".png  ",metadata3, sep=""),
  #          x = unit(0.025,"npc"), y = unit(0.015, "npc"),
  #          just = "left", gp = gpar(col = "black", fontsize = 8))
   
   dev.off()
 }      

 
 to.png.wide <- function(plotobj.in,maintext.in,pointsizein=14)
 {
  png.file.name <- paste(maintext.in,".png",sep="")
  png(file=png.file.name, width=960, height=500, pointsize=pointsizein)
   print(plotobj.in)
   
  #Stamp the plot
  grid.text(paste(metadata2,"  #Filename: ",maintext.in,".png  ",metadata3, sep=""),
            x = unit(0.025,"npc"), y = unit(0.015, "npc"),
            just = "left", gp = gpar(col = "black", fontsize = 8))
   
  dev.off()
 }      

 
to.png.long <- function(plotobj.in,maintext.in,pointsizein=14)
 {
  png.file.name <- paste(maintext.in,".png",sep="")
  png(file=png.file.name, width=900, height=1200, pointsize=pointsizein)
  print(plotobj.in)
  
  #Stamp the plot
  grid.text(paste(metadata2,"  #Filename: ",maintext.in,".png  ",metadata3, sep=""),
            x = unit(0.025,"npc"), y = unit(0.015, "npc"),
            just = "left", gp = gpar(col = "black", fontsize = 8))

  dev.off()
 } 
 
 
 
  
#-----------------------------------------------------------------------------------------
#Usage: multiplot(plotobj1, plotobj2, plotobj3, plotobj4, plotobj5, plotobj6, cols=3) for 6 plots in a 2x3 grid

	multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#-----------------------------------------------------------------------------------------
###ggplot2 specific functions###
count.unique <- function(x) length(unique(x))

#Function to count numbers in a boxplot
boxplot.give.n <- function(x)
{
   return(c(y = median(x), label = length(x)))
}

 
#--------------------------------------------------------------- 
#Data summary functions

#Convenience function for summarising a dataframe
    headtail <- function(x)
      {
       print(dim(x))
       print(head(x))
       print(tail(x))
      }  


 #90% confidence interval functions
  CI90lo <- function(x) quantile(x, probs=0.05)
  CI90hi <- function(x) quantile(x, probs=0.95)


#Define a function for length without NA's
   lengthNA <- function(x) length(na.omit(x))
  
 
#Define a function for geometric mean
  geomean <- function(x, na.rm=F)
  {  
  if (na.rm==T) x <- x[is.na(x)==F]
  exp(mean(log(x)))
  }
  #Note x cannot be negative, zero 


#Median and 90% tolerance intervals
  sumfunc90 <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  quantile(x, probs=0.05, na.rm=T, names=F)  #90%CI
    stat3 <-  quantile(x, probs=0.95, na.rm=T, names=F)
    stat4 <-  lengthNA(x)
    result <- c("median"=stat1, "low90"=stat2, "hi90"=stat3, "n"=stat4)
    result
  }
  
 #Median and 95% tolerance intervals
  sumfunc95 <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  quantile(x, probs=0.025, na.rm=T, names=F)  #95%CI
    stat3 <-  quantile(x, probs=0.975, na.rm=T, names=F)
    stat4 <-  lengthNA(x)
    result <- c("median"=stat1, "low95"=stat2, "hi95"=stat3, "n"=stat4)
    result
  }
  
  
#Mean, sd and CV
  sumfuncCV <- function(x)
  {
    stat1 <-  mean(x, na.rm=T)
    stat2 <-  sd(x, na.rm=T)  
    stat3 <-  stat2/stat1*100
    stat4 <-  lengthNA(x)
    result <- c("mean"=stat1, "sd"=stat2, "cv"=stat3, "n"=stat4)
    result
  }
  
  
#Geomean, mean, sd, CV, min, max, n - for Millenium MLN 8237
  sumfuncMLN <- function(x)
  {
    stat1 <-  geomean(x, na.rm=T)
    stat2 <-  mean(x, na.rm=T)
    stat3 <-  sd(x, na.rm=T)  
    stat4 <-  stat3/stat2*100
    stat5 <-  min(x, na.rm=T)  
    stat6 <-  quantile(x, probs=0.05, na.rm=T, names=F)  #90%CI
    stat7 <-  quantile(x, probs=0.95, na.rm=T, names=F)
    stat8 <-  max(x, na.rm=T) 
    stat9 <-  lengthNA(x)
    result <- c("gmean"=stat1, "mean"=stat2, "sd"=stat3, "cv"=stat4, "min"=stat5, "lo90"=stat6, "hi90"=stat7, "max"=stat8, "n"=stat9)
    result
  }
  
 
#Geomean, mean, sd, CV, min, max, n - for CBIO
  sumfuncCBIO <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  mean(x, na.rm=T)
    stat3 <-  sd(x, na.rm=T)  
    stat4 <-  stat3/stat2*100
    stat5 <-  min(x, na.rm=T)  
    stat6 <-  quantile(x, probs=0.05, na.rm=T, names=F)  #90%CI
    stat7 <-  quantile(x, probs=0.95, na.rm=T, names=F)
    stat8 <-  max(x, na.rm=T) 
    stat9 <-  lengthNA(x)
    result <- c("median"=stat1, "mean"=stat2, "sd"=stat3, "cv"=stat4, "min"=stat5, "lo90"=stat6, "hi90"=stat7, "max"=stat8, "n"=stat9)
    result
  } 
  

#median, mean, sd, CV, 95%CI - for bootstrap parameter summary
  sumfuncBOOT <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  mean(x, na.rm=T)
    stat3 <-  sd(x, na.rm=T)  
    stat4 <-  stat3/stat2*100
    stat5 <-  quantile(x, probs=0.025, na.rm=T, names=F)  #95%CI
    stat6 <-  quantile(x, probs=0.975, na.rm=T, names=F)
    result <- c("median"=stat1, "mean"=stat2, "sd"=stat3, "cv"=stat4, "lo95"=stat5, "hi95"=stat6)
    result
  } 
     
  
 #Summarize distribution by percentiles
  sumfuncPercentile <- function(x)
  {
    stat1 <-  quantile(x, probs=0.05, na.rm=T, names=F) 
    stat2 <-  quantile(x, probs=0.10, na.rm=T, names=F) 
    stat3 <-  quantile(x, probs=0.25, na.rm=T, names=F) 
    stat4 <-  quantile(x, probs=0.50, na.rm=T, names=F) 
    stat5 <-  quantile(x, probs=0.75, na.rm=T, names=F)  
    stat6 <-  quantile(x, probs=0.90, na.rm=T, names=F)  
    stat7 <-  quantile(x, probs=0.95, na.rm=T, names=F)
    result <- c("05perct"=stat1, "10perct"=stat2, "25perct"=stat3, "50perct"=stat4, "75perct"=stat5, "90perct"=stat6, "95perct"=stat7)
    result
  } 

  
#Mean, sd, min and max & n
  sumfuncRange <- function(x)
  {
    stat1 <-  mean(x, na.rm=T)
    stat2 <-  sd(x, na.rm=T)  
    stat3 <-  min(x, na.rm=T)  
    stat4 <-  max(x, na.rm=T)  
    stat5 <-  lengthNA(x)
    result <- c("mean"=stat1,"sd"=stat2, "min"=stat3, "max"=stat4, "n"=stat5)
    result
  }

  
#Median, Mean, sd, min and max & n
  sumfuncCov <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  mean(x, na.rm=T)
    stat3 <-  sd(x, na.rm=T)  
    stat4 <-  min(x, na.rm=T)  
    stat5 <-  max(x, na.rm=T)  
    stat6 <-  as.integer(lengthNA(x))
    result <- c("median"=stat1,"mean"=stat2,"SD"=stat3, "minimum"=stat4, "maximum"=stat5, "n"=stat6)
    result
  }

#Median etc for boxplot
  sumfuncBOX <- function(x)
  {
    stat1 <-  median(x, na.rm=T)
    stat2 <-  quantile(x, probs=0.025, na.rm=T, names=F) 
    stat3 <-  quantile(x, probs=0.25, na.rm=T, names=F)
    stat4 <-  quantile(x, probs=0.75, na.rm=T, names=F)
    stat5 <-  quantile(x, probs=0.975, na.rm=T, names=F)
    result <- c("median"=stat1, "q025"=stat2, "q25"=stat3, "q75"=stat4, "q975"=stat5)
    result
  }
    
  
 
  
  
#--------------------------------------------------------------- 
#Utility function to bind a list of dataframes - must have matching columns
  bind.list <- function(x)
   {
    #Bind a list of smaller dataframes into one big dataframe
    #Access the 2nd level of the list with [[x]]
   alldata <- x[[1]]
   for (i in 2:length(x))
    {
    alldata <- rbind( alldata, x[[i]] )
    as.data.frame(alldata)
    }
   alldata 
   }
   
   
 
 
#-----------------------------------------------------------------------------------------------
###NCA related functions###
 #see AUCtest.R for an evaluation of the trapz function

oneperID <- function(x)
{ # returns a single value for each ID of dataframe
  ID <- head(x, n=1)
  ID
}


AUCtrapz2 <- function(x, y)
{ # computes the integral of y with respect to x using trapezoidal integration - does not handle missing data
  n <- length(x)
  idx <- 2:n
  AUC0t <- (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
  data.frame(AUC0t,n)
}


AUCinfinity2 <- function(x, y, ntail)
{ # computes an AUC to infinity - does not handle missing data
  
  AUC0t <- AUCtrapz2(x,y)
  AUC0t <- as.numeric(AUC0t[1])  #AUC only
    
  taildatax <- tail(x,ntail)
  taildatay <- tail(y,ntail)
  tailfit <- lm(log(taildatay) ~ taildatax)
  
  k <- -1*tailfit$coefficients["taildatax"]
  thalfz <- log(2)/k
  R2 <- summary(tailfit)["r.squared"]
  
   Clast <- tail(y,1)
   AUCexp <- Clast/k
   AUCinf <- AUC0t+AUCexp
    
   result <- data.frame(AUCinf,k,thalfz,R2)
     #exclude bad extrapolation
     result$AUCinf[result$k < 0 |  result$r.squared < 0.75] <- NA
     result$thalfz[result$k < 0 |  result$r.squared < 0.75] <- NA

   result
}


Cmax <- function(x, y)
{ # computes the Cmax
 cmax <- max(y, na.rm=T)
 tindex <- which(y==cmax)
 tmax <- x[tindex]
 unique(cmax)[1] #as there can be 2 or more equal Cmax's, chose the first
}


Cmin <- function(x, y)
{ # computes the Cmin
 cmin <- min(y, na.rm=T)
 tindex <- which(y==cmin)
 tmax <- x[tindex]
 unique(cmin)[1] #as there can be 2 or more equal Cmin's, chose the first
}


tmax <- function(x, y)
{ # computes the time of Cmax
 cmax <- max(y,na.rm=T)
 tindex <- which(y==cmax)
 tmax <- x[tindex]
 head(tmax, n=1)   #as there can be 2 or more equal Cmax's, choose the first
}


NCAcalc <- function(x, y, ntail)
{
  #Wrapper for various NCA functions
  AUC0t <- AUCtrapz2(x,y)
  AUCinf <- AUCinfinity2(x, y, ntail)
  Cmax <- Cmax(x,y)
  tmax <- tmax(x,y)
  Cmin <- Cmin(x, y)

  NCAResult <- data.frame(AUC0t,AUCinf,Cmax,tmax,Cmin)
}


NCAcalcbasic <- function(x, y)
{
  #Wrapper for various NCA functions
  AUC0t <- AUCtrapz2(x,y)
  Cmax <- Cmax(x,y)
  tmax <- tmax(x,y)
 
  NCAResult <- data.frame(AUC0t,Cmax,tmax)
}


#Function for taking hydraulic rate constants and calculating half-lives (2 compartment)    
    HydraulicToHalflife <- function(CL, V1, Q, V2)
        {
        k10 <- CL/V1
        k12 <- Q/V1
        k21 <- k12*V1/V2

        beta <- 0.5*(k12+k21+k10-sqrt((k12+k21+k10)^2-4*k21*k10))
        alpha <- k21*k10/beta

        thalfAlpha <- log(2)/alpha
        thalfBeta <- log(2)/beta

        result <- c("k10"=k10,"k12"=k12,"k21"=k21,"alpha"=alpha,"beta"=beta,"thalfa"=thalfAlpha,"thalfb"=thalfBeta)
        result
        }
  
  
################################################################################
# Some functions used via R2HTML to comment an R script and produce html output
# needs library(R2HTML)
# Very simple, uncluttered way of converting R script into commented HTML

#HTML command functions are:
#HE(n) - echos the last "n" R commands and writes them to an HTML file
#HEO(n) - echos the last "n" R commands and their output and writes them to an HTML file
#HC(Hlevel,text) - writes a series of comments to an HTML file - heading level is specified by Hlevel, the text is a string
#HG(file.name) - inserts a jpeg graph file into the html file
#HGG2(plotobj,maintext) - creates a jpeg file of a ggplot2 object, then inserts the jpeg into the html file - maintext becomes the both the name of the graph and title of the file
#HT(file.name) - retrieves a *.csv file as a dataframe, then writes the dataframe as an html table
#HL(file.name) - inserts a hyperlink to a file into the html file


#Initiate the R2HTML file
#CSS.file <- ("R2HTML.css")
#HTMLInitFile(outdir=working.dir, filename="example",  HTMLframe=F, CSSFile=CSS.file)  #don't add extension

#code goes here
#HC(1,"PAGANZ 10 Workshop - Predictive Checks using in NM7, WfN and R")
#HC(2,"Richard Upton")
#HC(3,"This example illustrates the power of using a scripting language like R")
#HC(5,"The analysis draws on the principles of reproducible research, as discussed in this paper by Gentleman and Lang:")
#HL("Gentleman_2004_Reproducible_Research.pdf")

#Some html tags are allowed, e.g <br>  <b>Bold</b>

#HTMLEndFile(file="example.html")   #add extension

#Standard R2HTML commands can also be used:
 #exampledata <- read.csv("testdata_ALBWTcov2.csv")
 #HTML(exampledata)


#Echos and command only in the HTML file!
HE <- function(num.commands)
#HTML ECHO of command - echos the last "n" R commands and writes them to an HTML file
  {
   savehistory()
   command.history <- readLines(".Rhistory")
   n <- num.commands
   l <- length(command.history)
   last.commands <- command.history[(l-n):(l)]
   for (i in 1:n)
   {
   HTML(paste(">",last.commands[i],sep=""))
   HTML("")
   }
  }

#Echos and command AND output in the HTML file - brilliant!
HEO <- function(num.commands)
#HTML ECHO of command - echos the last "n" R commands and writes them to an HTML file
  {
   savehistory()
   command.history <- readLines(".Rhistory")
   n <- num.commands
   l <- length(command.history)
   last.commands <- command.history[(l-n):(l)]
   for (i in 1:n)
   {
   HTML(paste(">",last.commands[i],sep=""))
   HTML((eval(parse(text=last.commands[i]))))
   HTML("")
   }
  }

HC <- function(Hlevel,text)
#HTML COMMENT - writes a series of comments to an HTML file
#The HTML heading level is specified by Hlevel, the text is a string
#html code can be inserted and will (probably!) be understood
  {
  if (Hlevel == 1)
  HTML(paste("<H1>",text,"</H1>"))
  if (Hlevel == 2)
  HTML(paste("<H2>",text,"</H2>"))
  if (Hlevel == 3)
  HTML(paste("<H3>",text,"</H3>"))
  if (Hlevel == 4)
  HTML(paste("<H4>",text,"</H4>"))
  if (Hlevel == 5)
  HTML(paste("<p>",text,"</p>"))
  }


HG <- function(file.path)
#HTML GRAPH - inserts a png graph file into the html file
#requires master.dir from wfnviaR settings file
 {
  #debug file.path <- "1_1comp_ka_lag.g77/conc_vs_time.jpg"
  HTMLInsertGraph(GraphFileName=file.path, WidthHTML=600)
 }


HGG2 <- function(plotobj,maintext)
#HTML GGPLOT2 GRAPH - creates a png file of a ggplot2 object, then inserts the png into the html file
#Optimised for metadata style script - see P:\MLN8237PKupdate\Datacheck6\datacheck6.html_script.R
 {
 png.file.name <- paste(maintext,".png",sep="")
 to.png(plotobj,maintext)
 HG(paste("file://",png.file.name,sep=""))
 }
 

HGG2long <- function(plotobj,maintext)
#HTML GGPLOT2 GRAPH - creates a png file of a ggplot2 object, then inserts the png into the html file
#Optimised for metadata style script - see P:\MLN8237PKupdate\Datacheck6\datacheck6.html_script.R
 {
 png.file.name <- paste(maintext,".png",sep="")
 to.png.long(plotobj,maintext)
 HG(paste("file://",png.file.name,sep=""))
 }


HGG2wide <- function(plotobj,maintext)
#HTML GGPLOT2 GRAPH - creates a png file of a ggplot2 object, then inserts the png into the html file
#Optimised for metadata style script - see P:\MLN8237PKupdate\Datacheck6\datacheck6.html_script.R
 {
 png.file.name <- paste(maintext,".png",sep="")
 to.png.wide(plotobj,maintext)
 HG(paste("file://",png.file.name,sep=""))
 }
 

HT <- function(file.path)
#HTML TABLE - retrieves a *.csv file as a dataframe, then writes the dataframe as an html table
 {
  #debug file.path <- "1_1comp_ka_lag.g77/1_1comp_ka_lag.fit.param.csv"
  temp.data.frame <- read.csv(file.path, stringsAsFactors=F)
  print(temp.data.frame)                                      #output in console
  HTML(temp.data.frame)                                #output in html
  rm(temp.data.frame)
 }



HL <- function(file.path)
#HTML LINK - inserts a hyperlink to a file into the html file
#requires master.dir from wfnviaR settings file
 {
 #Debug file.path <- "concdata.csv"
 #link.file <- paste(master.dir,file.path, sep="/")
 #link.file <- paste(master.dir,file.path, sep="/")
 #paste("<a href=",file.path,">",link.file,"</a>", sep=" ")
 link.string <- paste("<a href=",file.path,">",file.path,"</a>", sep=" ")
 HTML(link.string)
 #<a href="../rawdata1.xls">../rawdata1.xls</a>
 
 #HL("concdata.csv")
 }
 
 
#-------------------------------------------------------------------------------------------- 
#Function for ordering factors - from gdata package but renames to avoid conflict with reorder from stats
# This function changes the order of the levels of a factor. It can do so via three different mechanisms, depending on whether, X and FUN, new.order or sort are provided.
# If X and Fun are provided: The data in X is grouped by the levels of x and FUN is applied. The groups are then sorted by this value, and the resulting order is used for the new factor level names.
# If new.order is provided: For a numeric vector, the new factor level names are constructed by reordering the factor levels according to the numeric values. For vectors, new.order gives the list of new factor level names. In either case levels omitted from new.order will become missing (NA) values.
# If sort is provided (as it is by default): The new factor level names are generated by applying the supplied function to the existing factor level names. With sort=mixedsort the factor levels are sorted so that combined numeric and character strings are sorted in according to character rules on the character sections (including ignoring case), and the numeric rules for the numeric sections. See mixedsort for details.

#needs gdata library for mixed sort
 
 reorder2 <- function (x, X, FUN, ..., order = is.ordered(x), new.order, sort = mixedsort) 
{
    constructor <- if (order) 
        ordered
    else factor
    if (!missing(new.order)) {
        if (is.numeric(new.order)) 
            new.order <- levels(x)[new.order]
        else new.order <- new.order
    }
    else if (!missing(FUN)) 
        new.order <- names(sort(tapply(X, x, FUN, ...)))
    else new.order <- sort(levels(x))
    constructor(x, levels = new.order)
}


#--------------------------------------------------------------------------------------------  
  processSIMdata <- function(model.path.file.ext)
{ #begin processSIMdata

#Debug model.path.file.ext <- "NM14/base_CRCLCL_min_VPC.ctl"

#Work out the nonmem model dir and model file name
  model.path.file <- gsub(".ctl", "", model.path.file.ext)
  model.path <- dirname(model.path.file)
  model.file.ext <- basename(model.path.file.ext)
  model.file <- gsub(".ctl","",model.file.ext)

  model.dir <- paste(master.dir,"/",model.path, sep="")

#Work out the nonmem output dir
   output.dir <- paste(master.dir,"/",model.path.file,dir.extension, sep="")
   setwd(output.dir)
  
#Work out the SIM fit filename
  SIM.file.ext <- paste(model.file,".fit",sep="")
#Work out the SIM fit filename
  SIM.file.ext.out <- paste(model.file,".fit.csv",sep="")


#Need to remove lines with "TABLE NO. 1 in them" and the header lines for each new subject
   #Strip the unwanted lines
   indata <- readLines(SIM.file.ext)
   tablelines <- grep("TABLE NO.  1",indata)  #May be installation specific
   headerlines <- grep(" ID",indata)          #May be installation specific 

   #Extract the information in the header line for column names
   header <- indata[headerlines[1]]
   header <- scan(textConnection(header), what="char")
   colnum <- length(header)

   #Strip out the unwanted lines
   badlines <- c(tablelines,headerlines)
   indata <- indata[-badlines]

   #replace white space with commas
   for (i in 1:length(indata))
    {
    indata[i] <- gsub('[[:space:]]+',',',indata[i])
    }

   #write to a file
   writeLines(indata, "SIMtemp.txt")

   #read again as csv file
   SIMdata <- read.csv("SIMtemp.txt", header=F)
   SIMdata <- SIMdata[,-1]     #delete first blank column
   names(SIMdata) <- header

   #The NONMEM output does not make a new ID number for each simulation
   #Therefore make a list of SIM numbers
   nsims <- length(tablelines)
   numtimepoints <- length(SIMdata$ID)
   numtimespersubject <- numtimepoints/nsims
   SIMdata$SIM <- rep(1:nsims, each=numtimespersubject)

   #Write the SIMdata to a file
   write.csv(SIMdata,SIM.file.ext.out, row.names=F)

   #tidy up
   #consider deleting the original fit file?
   file.remove("SIMtemp.txt")
   setwd(master.dir)
   
   #SIMdata

} #end processSIMdata


processSIMdata2 <- function(model.path.file.ext)
 #may have speed advantage over original processSIMdata2
 {
  #Work out the nonmem model dir and model file name
  model.path.file <- gsub(".ctl", "", model.path.file.ext)
  model.path <- dirname(model.path.file)
  model.file.ext <- basename(model.path.file.ext)
  model.file <- gsub(".ctl","",model.file.ext)

  model.dir <- paste(master.dir,"/",model.path, sep="")

 #Work out the nonmem output dir
  output.dir <- paste(master.dir,"/",model.path.file,dir.extension, sep="")
  setwd(output.dir)
  
 #Work out the SIM fit filename
  SIM.file.ext <- paste(model.file,".fit",sep="")
 #Work out the SIM fit filename
  SIM.file.ext.out <- paste(model.file,".fit.csv",sep="")
 
  SIM.data=read.csv(SIM.file.ext,sep="",header=T,skip=1,stringsAsFactors=F)
  #remove TABLE and data item header from subsequent sub-problems
  SIM.data <- SIM.data[SIM.data[,1]!="TABLE",] 
  SIM.data <- as.data.frame(apply(SIM.data,2,as.numeric)) #extra headers become NA, including ID
  SIM.data <- SIM.data[is.na(SIM.data$ID)==F,]  #remove rows with NA for ID
  
  #Write the SIMdata to a file
  write.csv(SIM.data,SIM.file.ext.out, row.names=F)
  
  setwd(master.dir)

 }
  


#-----------------------------------------------------------------------------------------------------------
#Define a function that performs the calculates TAFD from DATE & TIME for one subject only
#Will be applied later to all subjects using "lapplyBy" of the doBy package
calculate.TAFD <- function(nmdata)

{ #begin calculate.TAFD

  #Convert date and time to POSIXct format in seconds
    datetimestring <- paste(nmdata$DATE, nmdata$TIME)
    datetimestring <- strptime(datetimestring, format = "%Y%m%d %H:%M", tz="GMT")  #see Note 1, see Note 2
    #strptime converts a string of characters representing time and date to a standard format called POSIXlt.
    nmdata$DATETIME <- as.POSIXct(datetimestring)
    #POSIXct is a standard date & time format based on seconds since the start of 1970
    
  #Make sure the data are sorted by time
    nmdata <- orderBy( ~DATETIME, data=nmdata)  

  #Find the index of the first dose
    doseindex <- which(nmdata$AMT>0)
    #doseindex <- which(is.na(nmdata$AMT)==F)   ##ALTERNATIVE DEPENDING ON HOW AMT IS SPECIFIED FOR DV RECORDS
    firstdoseindex <- doseindex[1]
  #Calculate time elapsed since the first dose
    nmdata$TAFD <- nmdata$DATETIME - nmdata$DATETIME[firstdoseindex]
    
  #Convert result from seconds to appropriate units
    conversion.factor = 60*60     #in this case from seconds to hours, see Note 3
    nmdata$TAFD <- nmdata$TAFD/conversion.factor
    nmdata$TAFD  <- as.numeric(nmdata$TAFD) 

   #Return the modified dataframe as output from the function
       nmdata

} #end calculate.TAFD



#Function that performs the calculates TAD from TAFD for one subject only
#Not suitable for ADDL doses - an alternative version exists for this!
#Will be applied later to all subjects using "lapplyBy" of the doBy package

#Usage
  #dataall$DNUM <- -999   #need to set up these columns for function to work!
  #dataall$TAD <- -999
  
  #dataall <- lapplyBy(~ID, data=dataall, calculate.TAD)
  #dataall <- bind.list(dataall)
  


calculate.TAD <- function(nmdata)
   { #begin calculate.TAD
   
     #debug
     #nmdata <- subset(SimData, ID==1)
     #nmdata <- subset(SimData, select=c(ID,TAFD,AMT,DV))
    
   
     #Calculate dose times attributable to AMT without ADDL item
      #doseindex1 <- which(is.na(nmdata$AMT)==F)  ##ALTERNATIVE DEPENDING ON HOW AMT IS SPECIFIED FOR DV RECORDS
      doseindex1 <- which(nmdata$AMT>0)
      doseindex1
      dosetimes1 <- nmdata$TAFD[doseindex1]
      dosetimes1
     
      #Combine all the dose times for AMT's with and without ADDL items
      dosetimes <- c(dosetimes1,Inf)             #Append a last dose time of infinity
      dosetimes <- sort(unique(dosetimes))       #Remove duplicate dose times and sort

      #Calculate the total number of doses for the subject
       numdoses <- length(dosetimes)
       
      #Calculate the total number of time points for the subject
       numtimes <- length(nmdata$TAFD)
       

     #For each dose time, cycle through the sample times, calculating DNUM and TAD
     if (numdoses > 1) 
     {
      for (i in 1:(numdoses-1))
       {
         for (k in 1:numtimes)
          {
           if(nmdata$TAFD[k] >= dosetimes[i]  & nmdata$TAFD[k] < dosetimes[i+1] ) #if time is between dose1 and dose2
            {
             nmdata$DNUM[k] <- i
             nmdata$TAD[k] <- nmdata$TAFD[k] - dosetimes[i]
            }
          }
       }
      }
      
      if (numdoses ==1) 
      {
      nmdata$DNUM <- 1
      nmdata$TAD <- nmdata$TAFD
      }

      #Return the modified dataframe as output from the function
       nmdata
     
   } #end calculate.TAD
 


#Function to process bootstrap output file
#Adjusted because BS data may be different size from resampling subjects of different size
processBSdata2 <- function(BS.file.ext,numberFlag=F)
{ #begin processBSdata2
  
#Debug  BS.file.ext <- "nmbsall.fit"
 
#Work out the BS fit filename
   BS.file.ext.out <- paste(BS.file.ext,".csv",sep="")


#Need to remove lines with "TABLE NO. 1" in them and the header lines for each new subject
   #Strip the unwanted TABLE lines
   indata <- readLines(BS.file.ext)
   tablelines <- grep("TABLE NO.  1",indata)
   indata <- indata[-tablelines]
   
   #Find the header lines
   headerlines <- grep("ID",indata)

   #Extract the information in the header line for column names
   header <- indata[headerlines[1]]
   header <- scan(textConnection(header), what="char")
   colnum <- length(header)

   #Strip out the unwanted headerlines
   indata <- indata[-headerlines]

   #replace white space with commas
   for (i in 1:length(indata))
    {
    indata[i] <- gsub('[[:space:]]+',',',indata[i])
    }

   #write to a file
   writeLines(indata, "BStemp.txt")

   #read again as csv file
   BSdata <- read.csv("BStemp.txt", header=F)
   BSdata <- BSdata[,-1]     #delete first blank column
   names(BSdata) <- header

   #The NONMEM output does not make a new ID number for each simulation
   #Therefore make a list of BS numbers
   if (numberFlag==T)
    {
       nsims <- length(tablelines)
       headerlines2 <- headerlines-seq(0,(nsims-1),1)-1   #Row numbers for last of previous bs set once headers are removed
       headerlines2 <- headerlines2[-1]   #Remove first header row number
       headerlines2 <- c(headerlines2,tail(headerlines2,1)) #Add a final value
       
       BSdata$BS <- NULL
       k <- 1
       for (i in 1:nrow(BSdata))
          {
          BSdata$BS[i] <- k
          if (i==headerlines2[k]) k <- k+1 else k <- k
          }
    }  
  
  
   #Write the BSdata to a file
   write.csv(BSdata,BS.file.ext.out)

   #tidy up
   #consider deleting the original fit file to save space?
   file.remove("BStemp.txt")
      
   return(BSdata)

} #end processBSdata2


#------------------------------------------------------------------------------------
#Function for filling in gaps in covariate data - for one ID - use doBy to apply to all ID
#Only applies for covariates that don't change with time
   #test0 <- c(0.05,0.05,0.05,0.05,0.05,0.05,0.05)
   #test1 <- c(NA,0.05,0.05,NA,0.05,NA,0.05)
   #test2 <- c(NA,"2_0.0833_0.05_1","2_0.0833_0.05_1",NA,"2_0.0833_0.05_1",NA,"2_0.0833_0.05_1")
   #test3 <- c(NA,NA,NA,NA)
   #test4 <- c(0.05,0.01,0.05,0.05,NA,0.05,0.05)
   
   fill.missing <- function(x)   #GOLD
    {
    all.fill <- unique(x[is.na(x)==F])  #All non-missing values
    fill <- all.fill[1]  #value to replace missing values
       if (length(all.fill)>1) stop("Can't fill with non-unique values") else x[is.na(x)==T] <- fill
    x
    }
    
    #fill.missing(test0)
    #fill.missing(test1)
    #fill.missing(test2)
    #fill.missing(test3)
    #fill.missing(test4)

#----------------------------------------------------------------------------------------------
###Imputation functions###

locf <- function (x)
#Last observation carried forward
#Finds an NA and carries forward the previous value  
  {
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position == 0] <- NA
    x[last.good.position]
}                                      


nocb <- function (x)
#Next observation carried backward
#Reverses the locf function  
  {
   rev(locf(rev(x)))
  }  

 
impute <- function(x)
#Function that runs locf first, then nocb
{
    x <- locf(x)
    x <- nocb(x)
    x
}    

  

#-------------------------------------------------------------------------------------------
###Functions for counting missing data###

#see also lengthNA

#Function for calculating percent missing - use apply to do column by column  
 calculate.percent.missing <- function(x)
    {
    length(x[is.na(x)==T])/length(x)*100
    } 

#Function returns T if any NA's
 anyNA <- function(x) any(is.na(x))

#Function returns T if all NA's
 allNA <- function(x) all(is.na(x))

# #Testing
# Concentration <- c(1,2,3,4)
# percent.missing(Concentration)
# anyNA(Concentration)
# allNA(Concentration)

# Concentration <- c(1,2,3,NA,4)
# percent.missing(Concentration)
# anyNA(Concentration)
# allNA(Concentration)

# Concentration <- c(NA,NA,NA,NA,NA)
# percent.missing(Concentration)
# anyNA(Concentration)
# allNA(Concentration)

# #Run this at STUDY level and ID level


  classify.missing <- function(x)
      {
       x[x ==0] <- 0   #complete
       x[x < 100 & x > 0]  <- 1  #locf
       x[x ==100]  <- 2     #impute
       x[x > 100 | x < 0]  <- x[x > 100 | x < 0]  #excludes ID & STDY, as they are > 100
       x
      }

      #test
      #x <- c(-1,0,50,100,150,200)
      #classify.missing(x)

      
 count.missing <- function(x)
    {
     N <- length(x)
     percent0 <- length(x[x==0])/N*100  #complete
     count0 <- length(x[x==0])          #complete
     percent1 <- length(x[x==1])/N*100  #locf
     count1 <- length(x[x==1])          #locf
     percent2 <- length(x[x==2])/N*100  #impute
     count2 <- length(x[x==2])          #impute
     
     result <- c(percent0,percent1,percent2,count0,count1,count2,N)
     #result <- cbind(c("complete","locf","impute","n"),result)
     result
    }


#Set up covariate names as a formula, and subset a covariate dataframe
# covnames <- as.formula("~AGE+HT+WT+BMI+BSA")
# covdata <- subset(datanew, select=c("AGE","HT","WT","BMI","BSA"))

#Example - Missing by Study
#   missingbystudy <- ddply(covdata, .(STDY), colwise(calculate.percent.missing))

#Example - Missing by Subject
#   missingbysubject <- ddply(covdata, .(STDY,ID), colwise(calculate.percent.missing))
#   missingbysubjectcode <- colwise(classify.missing)(missingbysubject)
#   missingsummary <- ddply(missingbysubjectcode, .(STDY), colwise(count.missing, covnames))
#   missingsummary <- cbind("Missing status"=c("complete_percent","locf_percent","impute_percent","complete_count","locf_count","impute_count","n"),missingsummary)
 

#-----------------------------------------------------------------------------
#Functions for listing and finding objects

# #Search for objects in workspace with wildcards
# ls(pattern=glob2rx("final*"))

# #Show all objects
# ls.str()
# print(ls.str(), max.level = 0)# don't show details


# #Functions
# #how do the functions look like which I am using?
# lsf.str() 

# #write the function to a text file
# writeLines(lsf.str(), "function_list.txt")

# #list function by wildcard
# lsf.str(pattern = "Draw")


# #Dataframes
# #dataframes are lists in this context
# ls.str(mode = "list") 

# print(ls.str(mode = "list"), max.level=0) 

# print(ls.str(mode = "list",pattern=glob2rx("*data*")), max.level=0)

# writeLines(print(ls.str(mode = "list",pattern=glob2rx("*data*")), max.level=0), "dataframe_list.txt")


# #show packages
# search() 

#------------------------------------------------------------------------------------------------------ 
#Alternatively - save all functions in an environment to text
#from the interweb!

save.functions.from.env <- function(file = "d:\\temp.r")
{
    # This function will go through all your defined functions and write them into "d:\\temp.r"
    # let's get all the functions from the envoirnement:
    funs <- Filter(is.function, sapply(ls( ".GlobalEnv"), get))
 
    # Let's 
    for(i in seq_along(funs))
    {
        cat(    # number the function we are about to add
            paste("\n" , "#------ Function number ", i , "-----------------------------------" ,"\n"),
            append = T, file = file
            )
 
        cat(    # print the function into the file
            paste(names(funs)[i] , "<-", paste(capture.output(funs[[i]]), collapse = "\n"), collapse = "\n"),
            append = T, file = file
            )
 
        cat(
            paste("\n" , "#-----------------------------------------" ,"\n"),
            append = T, file = file
            )
    }
 
    cat( # writing at the end of the file how many new functions where added to it
        paste("# A total of ", length(funs), " Functions where written into", file),
        append = T, file = file
        )
    print(paste("A total of ", length(funs), " Functions where written into", file))
}
 
 #save.functions.from.env(file="text.txt") # this is how you run it


#--------------------------------------------------------------------------------------------------
#rtf table functions

#Function for without - opposite of %in%
 "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y



#Used for reshaping summary tables
  insert_column <- function(df,newcoldata,newcolname,aftercolindex)
    #Function to insert a new column into a dataframe at a given column index
      {
        df2 <- data.frame(df,newcoldata)  #allows recycling
        names(df2)[ncol(df2)] <- newcolname
        df2 <- df2[append(1:ncol(df),ncol(df2),aftercolindex)]
        df2
      }


 move_column <- function(df,movecolindex,aftercolindex)
    #Function to move a new column into a new position specified by a column index
      {
        #debug
        #df <- covDescriptiveTable
        #movecolindex <- 8
        #aftercolindex <- 1
        df2 <- df[append(1:ncol(df)%w/o%movecolindex,movecolindex,aftercolindex)]
        df2
      }

  
 gsub_all <- function(oldtext,newtext,object)
   #Function to use gsub for all elements of a vector of matched old and new text  
     {
      newobject <- object
      for (i in 1: length(oldtext))
       {
        newobject <- gsub(oldtext[i],newtext[i],newobject)
       }
     newobject
     }




#Define a function to for "reverse" locf for neatening id columns
revlocf <- function(x)
#Function to "reverse" the locf process
#In a column of category values, values repeated after a new category value are replaced with NA
#For pretty formatting of tables
 {
    #Find the runs 
    rundata <- rle(x) 
    #Find the index of the first in category
    catfirstindex <- cumsum(c(1,rundata$lengths))
    catfirstindex <- head(catfirstindex, -1)
    #Find all index values
    allindex <- 1:length(x)
    #Find repeated category values
    catrepeatindex <- allindex %w/o% catfirstindex
    #Set repeated values to NA
    x[catrepeatindex] <- NA
    x
 }


writem3.rtf <- function(df, filename, font=10, idcols=1, title=NULL, paper="letter", layout="landscape")
#Function to write a dataframe as a rtf document with formatting
#library(rtf)
{

  #debug
  #df <- covDescriptiveTable
  #filename <- "testsummary"
  #idcols <- c(1,2,3)
  #font <- 10
  #title <- "Summary of Continuous Covariates"
  #translatedf <- NULL
  #paper <- "letter"
  #layout <- "portrait"

 #Set paper size
 if (paper=="letter" & layout=="portrait")
  {
    paperwidth <- 8.5
    paperheight <- 11
  }

 if (paper=="letter" & layout=="landscape")
  {
    paperwidth <- 11
    paperheight <- 8.5
  }


 if (paper=="a4" & layout=="portrait")
  {
    paperwidth <- 8.27
    paperheight <- 11.69
  }

 if (paper=="a4" & layout=="landscape")
  {
    paperwidth <- 11.69
    paperheight <- 8.27
  }


 #Initiate rtf file
 rtf <- NULL
 filename <- paste(filename,".rtf",sep="")
 rtf <- RTF(filename,width=paperwidth,height=paperheight,font.size=font,omi=c(1,1,1,1))

 #Title data
 tabletitle <- title
 addHeader(rtf, tabletitle, subtitle=NULL, TOC.level=1)
 addParagraph(rtf)

 #Neaten explanatory variable columns
 expvarcols <- idcols
 df <- colwise(as.character)(df)  #remove any factors
 df[expvarcols] <- colwise(revlocf)(df[expvarcols])  #doesn't work with repeated!! units

 
 #Format numbers to pretty 3 sig figs element by element
   for (i in 1:nrow(df))  
     {
       for (j in 1:ncol(df))  
        {
          df[i,j] <- formatT(df[i,j])
        }
     }
 

 addTable(rtf,df,row.names=F,NA.string="",col.justify="C", header.col.justify="C")   #,c(1,rep(0.5,4)) 

 metadata <- paste(metadata2," ",metadata3,"\n",filename, sep="")
 metadata <- paste("{\\fs16 Source: ",metadata," }", sep="")   #fs is fontsize*2
 addText(rtf, metadata)
 
 done(rtf)

}

# writem3.rtf(covDescriptiveTable, filename.out, idcols=c(1,2,3), title="Summary of Continuous Covariates")

#---------------------------------------------------------------------------------------------------------
#rtf basics

 #rtf 
 #note - rtf has \\ escape character, but in R must be written as \\\\ - escape the escape!!
 #new paragraph {\\\\par}  
 #new line {\\\\line}
 #superscript  mg/m{\\\\super 2}

#\trautofit1 in addition to \cellx0

# Other useful formatting commands include:

    # \fs? for font size (where ? is twice the font size such that \fs24 results in a 12 point font)
    # \i to turn italics on and \i0 to turn italics off
    # \b to turn bold on and \b0 to turn bold off
    # \scaps to turn small caps on and \scaps to turn small caps off
    # \strike to turn strike through on and \strike0 to turn strike through off
    # \caps to turn all capitals on and \caps0 to turn all capitals off
    # \outl to turn outline on and \outl0 to turn outline off


#http://latex2rtf.sourceforge.net/rtfspec_7.html#rtfspec_tabledef

#To make autofit to contents, save as txt file, reload then gsub to change as below:

#\trautofitN 	AutoFit:
#0 — No AutoFit (default)
#1 — AutoFit is on for the row. Overridden by \clwWidthN and \trwWidthN in any table row. 

#-----------------------------------------------------------------------------------------------------------
#Define a function that performs the calculates TAFE from DATE & TIME for one subject only
#Will be applied later to all subjects using "lapplyBy" of the doBy package
calculate.TAFE <- function(nmdata)

{ #begin calculate.TAFE

  #Make sure the data are sorted by time
    nmdata <- orderBy(~DATETIME, data=nmdata)  

  #Find the index of the first event
    eventindex <- which(nmdata$ID>0)
    #eventindex <- which(is.na(nmdata$AMT)==F)   ##ALTERNATIVE DEPENDING ON HOW AMT IS SPECIFIED FOR DV RECORDS
    firsteventindex <- eventindex[1]
  #Calculate time elapsed since the first event
    nmdata$TAFE <- nmdata$DATETIME - nmdata$DATETIME[firsteventindex]
    
  #Convert result from seconds to appropriate units
    conversion.factor = 60*60     #in this case from seconds to hours, see Note 3
    nmdata$TAFE <- nmdata$TAFE/conversion.factor
    nmdata$TAFE  <- as.numeric(nmdata$TAFE) 

   #Return the modified dataframe as output from the function
       nmdata

} #end calculate.TAFE


#-----------------------------------------------------------------------------------------------------------
#Define a function that performs the calculates TAFD-EVENT from DATE & TIME for one subject only
#Will be applied later to all subjects using "lapplyBy" of the doBy package
calculate.TAFDE <- function(nmdata)

{ #begin calculate.TAFDE

  #Make sure the data are sorted by time
    nmdata <- orderBy(~DATETIME, data=nmdata)  

  #Find the index of the first event
    doseindex <- which(nmdata$DOSE>0)
    #doseindex <- which(is.na(nmdata$AMT)==F)   ##ALTERNATIVE DEPENDING ON HOW AMT IS SPECIFIED FOR DV RECORDS
    firstdoseindex <- doseindex[1]
  #Calculate time elapsed since the first event
    nmdata$TAFDE <- nmdata$DATETIME - nmdata$DATETIME[firstdoseindex]
    
  #Convert result from seconds to appropriate units
    conversion.factor = 60*60     #in this case from seconds to hours, see Note 3
    nmdata$TAFDE <- nmdata$TAFDE/conversion.factor
    nmdata$TAFDE  <- as.numeric(nmdata$TAFDE) 

   #Return the modified dataframe as output from the function
       nmdata

} #end calculate.TAFDE



