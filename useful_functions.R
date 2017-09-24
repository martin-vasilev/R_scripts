
# Martin R. Vasilev, 2016


#############
#  p_value  #    
#############

# takes a p-value and gives a "print-friendly" output (mostly for Rmarkdown)
p_value<- function(p){
  
  if(p>0.01 | p==0.01 | p==0.001){
    if(p>0.01){
      p<- round(p, 2)
    }
    
    new_p<- toString(p)
    new_p<- substr(new_p, 2, nchar(new_p)) # gets rid of the 0 before the decimal point
    sign<- "= "
  }
  
  if(p<0.01 & p>0.001){
    new_p<- ".01"
    sign<- "< "
  }
  
  if(p<0.001){
    new_p<- ".001"
    sign<- "< "
  }
  
  string<- paste("p", sign, new_p, sep= "") # merge into statement
  return(string)
}


#################
#  print_ANOVA  #    
#################

print_ANOVA<- function(object, line, type= "F1"){
  object<- object$ANOVA # subset only ANOVA results
  type<- (unlist((as.numeric(unlist(strsplit(unlist(type), "[^0-9]+")))))) # get F1 or F2
  type<- type[!is.na(type)] # take only the number
  df<- paste("F", type, "(", object$DFn[line], ",",object$DFd[line], ")", sep="")
  F_value<- round(object$F[line],2)
  MSE<- round(object$MSE[line], 2)
  p<- p_value(object$p[line])
  
  output<- paste(df, "= ", F_value, ", ", "MSE", "= ", MSE,
                 ", ", p, sep="")
  return(output)
  
}



##############
#  Bonf_LMM  #    
##############

# gives a critical t-value for Bonferroni-corrected k number of tests
Bonf_LMM <- function(k= 1, alpha=0.05){
  corr_alpha<- alpha/k
  crit_t<- qnorm(1- corr_alpha/2) # /2 because it's a two-tailed test
  
  return(crit_t)
}


##################
#  load_package  #    
##################

# generic function for checking if a package is installed and then loading it:
load_package<- function(package){
  
  #if(!is.character(package)){
  #  package<- deparse(substitute(package))
  #}
  
  if(package %in% rownames(installed.packages())==FALSE){
    message(paste("Installing required package ", toupper(package), "..."))
    install.packages(package)
  }
  suppressMessages(eval(parse(text= paste("library(", package, ")", sep=''))))
}

################
#  ttest.sens  #    
################

# Gives a sensitivity analysis with different priors (as a plot)
# Developed to work for the ttestBF function from the BayesFactor package
ttest.sens<- function(x, y, title="Bayes factor sensitivity", paired=TRUE, range= seq(0.1, 2, 0.1),
                     xLabelBreak=0.2, xAdjust= 10){
  
  if("BayesFactor" %in% rownames(installed.packages())==FALSE){
    message("Installing required package 'BayesFactor'...")
    install.packages("BayesFactor")
  }
  suppressMessages(library(BayesFactor))
  
  
  if("ggplot2" %in% rownames(installed.packages())==FALSE){
    message("Installing required package 'ggplot2'...")
    install.packages("ggplot2")
  }
  suppressMessages(library(ggplot2))
  
  if("scales" %in% rownames(installed.packages())==FALSE){
    message("Installing required package 'scales'...")
    install.packages("scales")
  }
  suppressMessages(library(scales))
  
  BF<- NULL
  
  for(i in 1:length(range)){
    
    B<- ttestBF(x=x, y=y, paired = paired, rscale=range[i])
    BF[i]<- extractBF(B)$bf
  }
  
  db<- data.frame(range, BF)
  
  xmargs<- seq(min(range), max(range), xLabelBreak)
  xmarg_S<- NULL
  
  for(i in 1:length(xmargs)){
      xmarg_S[i]<- toString(xmargs[i])
    }
  #ymargs<- seq(min(BF), max(BF), 0.3)
  
  Plot<- ggplot(data= db, aes(x=range, y= BF))+
    theme_bw() + theme(panel.grid.major = element_line(colour = "#BDBDBD", size=0.7),
                       axis.line = element_line(colour = "black", size=2),
                       panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
    scale_x_continuous(breaks=xmargs, labels= xmarg_S)+
    scale_y_continuous(breaks = pretty_breaks(n = 7))+
    geom_line(size=2)+ xlab("Prior scale")+ ylab("Bayes Factor")+ ggtitle(title)+
    theme(title=element_text(size=20), axis.title.x = element_text(size=20, face="bold"), 
          axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=20), 
          panel.border = element_rect(linetype = "solid", colour = "black"), 
          legend.key = element_rect(colour = "#000000", size=1))
  
  # add shape showing default prior:
  tpos<- NULL
  if(db$BF[1]< db$BF[nrow(db)]){
    tpos<- 0.5
  } else{
    tpos<- 0.9
  }
  
  Plot<- Plot + geom_point(mapping=aes(x=0.7, y=db$BF[which(db$range==0.7)], shape=22), color="red", fill="red", size=8)+ 
         scale_shape_identity()+ geom_text(mapping=aes(x=tpos, y=db$BF[which(db$range==0.7)]+(xAdjust/100)*db$BF[which(db$range==0.7)], 
         label= "default prior"), size=6)
  
  return(Plot)
  
}

##################
#  rescale_freq  #    
##################

# rescales the count frequencies of Uk-Subtlex (default) or the BNC to give frequency count per million 
# input is the frequency count, and the name of the database (leave empty for SUBTLEX or use db="BNC"
# for the BNC)
rescale_freq<- function(count, db="SUBTLEX", exact=FALSE){
  if(db=="SUBTLEX"){
    TotalCount<- 201335638 # from p. 1178
  }
  if(db=="BNC"){
    TotalCount<- 100000000 
  }
  
  count_per_mil<- (count/TotalCount)*1000000
  if(exact==FALSE){
    count_per_mil<- round(count_per_mil)
  }
    
  return(count_per_mil)
}

#####################
# NOT WRITTEN BY ME :
#####################
# Author: http://stackoverflow.com/a/22701462/3903676
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

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

ColorBlind<- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
