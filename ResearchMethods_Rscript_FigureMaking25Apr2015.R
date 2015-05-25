
###RESEARCH METHODS
##April 2015

###The code below walks you through making one figure
##the code is made to be modified for other figures you need to make
#first part is a play
#second part gets you to a polished output
# it looks long but really isn't -- I've made it longer to illustrate points



##Data
##data set that is on LMS
###
##NOTE:  this is where YOU change the code to location on your local computer
###
MyData <- read.csv("E:/ResearchMethods/2015_Joe/DataAssignment/ResMethods_Data_15Apr2015.csv")
str(MyData)

summary(MyData$Height)
summary(MyData$DBH)

##Libraries
library(ggplot2)
#if you don't have this:
# -> tools -> install packages --> type in ggplot2 and let it go. 
#then run library code again



################
################
##GRAPHS
################
################
#This code is intended to help w making graphs


##you can make graphs however you want but...
## R is really powerful
## your hons thesis is a rare opportunity to hunker down and learn this stuff
## many R options exist
## this code is for ggplot2 which is popular
## I like it but other options are just fine
##ggplot is really well supported -- rseek.org search ggplot + any specific aspect 
#ALSO, download book from here:
#https://dl.dropboxusercontent.com/u/9147311/ggplot2_Elegant%20Graphics%20for%20Data%20Analysis.pdf


#################
####GETTING GOING
##ggplot 'grammar of graphics'
## it has a very specific syntax that is modular

#we'll start with quick plot = qplot
qplot(factor(Status),DBH,data=MyData)
#okay scatter of points for each value of Status
#NOTE: I used factor(Status) to get the function to recognize 0/1 as a factor, not a number
#not terribly helpful
#TRY WITH HEALTH and see what you get


qplot(factor(Status),DBH,data=MyData,colour=Trt)
#better --this gives us some capacity to distinguish groups
qplot(factor(Status),DBH,data=MyData,colour=Trt,size=4)
#what does size do?
#what happened w the legend? It added a box for the size. We can control this.
qplot(factor(Status),DBH,data=MyData,colour=Trt,shape=Trt,size=4)
#what did the shape argument do?

#How about an old-fashioned scatterplot
qplot(DBH,Health,data=MyData,colour=Trt,size=1)
qplot(DBH,Height,data=MyData,colour=Trt,size=1)
qplot(DBH,Height,data=MyData,colour=Trt,size=Status)
qplot(DBH,Height,data=MyData,colour=Species,size=Status)
qplot(DBH,BAplot.m2.ha,data=MyData,colour=Trt,size=2)
qplot(DBH,StemsPlot,data=MyData,colour=Trt,size=2)

#now in this instance it is less relevant we could add a smoothed line of fit
qplot(DBH,Height,data=MyData,geom=c("point","smooth"))
#the grey shaded area is a 95% confidence envelope
qplot(DBH,Height,data=MyData,colour=Species,geom=c("point","smooth"))

qplot(DBH,Height,data=MyData,colour=Species,geom=c("point","abline"))

#why four lines?
#how would you fit this by status?
#by status within species? (I'd subset and re-run with data=your subset dataframe)

####The code above introduces the argument of 'geom'
##ggplot has geom arguments of point, line, jitter, histogram, density, bar + others
##experiment with some of these

#For example, jitter points
qplot(DBH,Health,data=MyData,colour=Species,geom=c("point"),size=2)
qplot(DBH,Health,data=MyData,colour=Species,geom=c("point","jitter"),size=2)
#helps a lot when it's along a category like this

#maybe it's easier to see if we reverse the axes
qplot(Health,DBH,data=MyData,colour=Species,geom=c("point"),size=2)
qplot(Health,DBH,data=MyData,colour=Species,geom=c("point","jitter"),size=2)
#pretty good impression of where the data lie (smaller values) 
#and differences (smaller DBH in less healthy trees)
#also this uses the graph space effectively

#Let's explore a bit
notEM <- subset(MyData,Species!="EM")
qplot(Health,DBH,data=notEM,colour=Species,geom=c("point","jitter"),size=2)
#ahhh  now we can see the other 3 species much better!
#poor BG (Banksia grandis)
BG <- subset(MyData,Species=="BG")
qplot(Health,DBH,data=BG,colour=Species,geom=c("point","jitter"),size=2)
#note how qplot rescaled Y-Axis. We'll come back to managing that

#BOXPLOT
qplot(factor(Health),DBH,data=MyData,geom=c("boxplot"))
qplot(factor(Health),DBH,data=MyData,colour=Species,geom=c("boxplot"))
#nice, gives you a sense of where the data lie by species
#still just exploratory -- not nice enough to call final

qplot(factor(Status),DBH,data=MyData,colour=Species,geom=c("boxplot"))
qplot(DBH,data=MyData,colour=Species,geom=c("histogram"),fill=Species)
qplot(Species,DBH,data=MyData,colour=factor(Status),geom=c("boxplot"))



##HISTOGRAM
qplot(DBH,data=MyData,geom="histogram")
qplot(DBH,data=MyData,geom="histogram",colour=Species,fill=Species)
qplot(DBH,data=MyData,geom="histogram",colour=factor(Status),fill=factor(Status))
qplot(DBH,data=MyData,geom="histogram",colour=Trt,fill=Trt)


###
##Okay, we've now looked at a number of features of qplot 
#and you've learned some basics on controlling colour and plot type and point size
#and control grouping for display (Species, Trt, etc)
# you also can see how to convert Status or Health to a factor

#NOW we have to progress to finer levels of control using the same syntax
#but spelled out a little longer to allow for more control
#AND we need to learn how to control the finer bits and pieces (axis labels, font, etc)

################################################
################################################
#To accomplish this we'll work through an EXAMPLE


qplot(factor(Health),DBH,data=notEM,colour=Species,geom=c("boxplot"))
#remember a version of this plot from above?  
#It is similar to what you need to do but just different enought
#that it should force you see how/where to change code to suit your needs

##We will:
#a) calculate means and confidence intervals
#b) plot it roughly
#c) refine the plot to publication quality


########################## CALCULATING MEAN AND CIs
#I found this code online 
#all you have to do is run it
##########SUMMARYSE
############################################3
#############################################
#############################################
### CODE COPIED FROM cookbook-r.com
##

##SUMMARYSE
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )  
                 },
                 measurevar
  )
  # Rename the "mean" column   
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)  
}

######## THIS IS THE END OF THE COPIED CODE
###########################################
#all you have to do is copy + paste into your script and run it
###############################################################

#okay now we run summarySE to create object holding the values for the graphing
str(notEM)
TreeSum <- summarySE(notEM,measurevar="DBH",groupvars=c("Species","Trt","Health"))
TreeSum  #pretty cool: means + 95% confidence intervals calculated
#note the NAs where the sample was 1.  We'll have to deal w this later


##PLOTTING
#now instead of qplot we use the function ggplot
p <- ggplot(TreeSum,aes(x=factor(Health),y=DBH,colour=Trt,shape=Species))
p
#this first object is queueing up the stuff but we don't have a graph yet
#we haven't specified the geometry yet (points, lines, etc)
#aes is aesthetic -- we use it to tell ggplot the layout
#now we proceed to progressively refine the plot, each time creating a new object
#so we can see what's happening at each step along the way

#ggplot is nice in that it uses '+' to connect things
p1 <- p + geom_point(size=4)
p1
#ok we've got points
#let's get them spaced and not on same line
pd <- position_dodge(0.6)
p1 <- p + geom_point(size=4,position=pd) #for this I'm overwriting the former p1
p1
#okay, got the points spaced out
#note we have this double legend where the shape and colour arguments are groupings
#will need to fix this

p2 <- p1 + geom_errorbar(aes(ymin=DBH-ci,ymax=DBH+ci),width=.15,position=pd)
p2
#sweet.  starting to look more 'official'!
#what's up with the massive 95% CIs?  
#can you have negative values for CIs of DBH?
#not so much. another thing we're going to have to fix

p3 <- p2 + facet_grid(.~Species)
p3
#So facet is what makes panels
#these are horizontal, vertical is Species~.

#okay we're getting close with the overall layout
#time to start tinkering with the finer details

p4 <- p3 + theme(panel.grid.minor=element_blank(),
                 panel.grid.major=element_blank())
p4
#I am a fan of a solid background --that's just me though
#time to get the panel labels sorted
#All you have to do is copy + paste
#you can adapt this for Drought and Healthy too if you facet by them 
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Species") { 
    value[value=="CC"] <- "Corymbia callophylla"
    value[value=="AF"]   <- "Allocasuarina fraseriana"
    value[value=="BG"]   <- "Banksia grandis"
    value[value=="EM"] <- "Eucalyptus marginata"
      }
  return(value)
}
p5 <- p4 + ylim(0,100)+facet_grid(.~Species,labeller=mf_labeller)
p5
#Sweet
#Note:  if you try to limit the y-axis after facetting, you undo the labels
#sometimes order matters

p6 <- p5 + xlab("Tree Health Category") + ylab("Mean Tree Diameter (cm)")
p6
p7 <- p6 +   theme(axis.text.x=element_text(size=16),
                  axis.title.x=element_text(size=18),
                  axis.title.y=element_text(size=16),
                  axis.text.y=element_text(size=18),
                  strip.text.x=element_text(size=16,face="italic"))
p7
#examine code above
#which controls the values, which controls the title, which is the facet title
#you could insert other things in here --bold, colour, fill, etc
#have fun

#final thing to do is fix the legend
p8 <- p7+ guides(shape=FALSE)  #turn off the species legend
p8
p9 <- p8 + scale_colour_hue("Drought\nTreatment",
                            breaks=c("D","H"),
                            labels=c("Drought","Healthy"))
p9
#make it bigger, put it in more sensible spot
p10 <- p9 + theme(legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  legend.position=c(0.95,0.85))
p10

#we could mess around with this forever but this definitely is good enough!
#Note: if I were submitting this to a journal, 
#I would put 'NA' text labels where there is missing data.
#Otherwise it is too hard to visually see the pairs
#But, for us it is just an example which you will modify anyhow



#Now you can try this with the status on the x-axis and include EM