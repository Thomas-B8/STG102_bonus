# author = "Besognet Thomas"
# date = " 30/06/23" 
# project = "Korpela et al. Proceedings B Replication"
# name = "Supplemental work : modify models"

# this code is constituted by a "patchwork" of previous code used in the replication part,
# with alterations to create the new models 

# step 3

# packages 
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")                                       
library(ggplot2)
install.packages("cowplot")
library(cowplot)

# Opening data 
data <- read.csv("../data/data_Korpela.csv", sep=",", header=T, dec=".")

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# I Parameter's estimation 

# We create two data frames, the first one for models 1 et 2 (vole) , the second for models 3,4 and 5 (Predators)
estimators_vole <- data.frame(matrix(0,6,8))
names(estimators_vole)<-c("zone","model","a","b","d","e","f","sigma")

estimators_predators <- data.frame(matrix(0,9,10))
names(estimators_predators)<-c("zone","model","a","alpha","b","c","d","e","f","sigma")


# parameters are estimated by zones, so we have to filter the data frame 
for (zone in 1:3){
  if (zone==1){
    data_zone <- filter(data,zone=="north")
    region <- "north"
  }
  if (zone==2){
    data_zone <- filter(data,zone=="east")
    region <- "east"
  }
  if (zone==3){
    data_zone <- filter(data,zone=="west")
    region <- "west"
  }
  
  # then, we create vectors for each variables and each temporal laps 
  Present <-filter(data_zone,year>1990)
  At    <-Present$Vole.Autumn
  St    <-Present$Vole.Spring
  P1t   <-Present$Small.mustelid 
  P2t   <-Present$Generalist.predator
  P3t   <-Present$Avian.predator
  Past_1 <-filter(data_zone,year>1989,year<2011) 
  At_1    <-Past_1$Vole.Autumn
  St_1    <-Past_1$Vole.Spring
  P1t_1   <-Past_1$Small.mustelid
  P2t_1   <-Past_1$Generalist.predator
  P3t_1   <-Past_1$Avian.predator
  Past_2 <-filter(data_zone,year<2010)   
  At_2    <-Past_2$Vole.Autumn
  
  # we can now estimate our five models 
  model1 <- lm(formula = St ~ At_1 + P1t + P2t + P3t_1 ,offset = At_1 )
  model2 <- lm(formula = At ~ St + P1t + P2t + P3t ,offset = St   )
  model3 <- lm(formula = P1t ~ P1t_1 + At_1 + St_1 + At_2 + P2t_1 + P3t_1 ,offset = P1t_1 )
  model4 <- lm(formula = P2t ~ P2t_1 + At_1 + St_1 + At_2 + P1t_1 + P3t_1 ,offset = P2t_1 )
  model5 <- lm(formula = P3t ~ P3t_1 + St + At_1 + St_1 + P1t + P2t ,offset = P3t_1 )
  
  # and complete our data frames 
  # 1) the zone's name
  estimators_vole[2*(zone-1)+1,1] <- region
  estimators_vole[2*(zone-1)+2,1] <- region
  
  estimators_predators[3*(zone-1)+1,1] <- region
  estimators_predators[3*(zone-1)+2,1] <- region
  estimators_predators[3*(zone-1)+3,1] <- region
  
  
  # 2) the model's name
  estimators_vole[2*(zone-1)+1,2] <- 1
  estimators_vole[2*(zone-1)+2,2] <- 2
  
  estimators_predators[3*(zone-1)+1,2] <- 3
  estimators_predators[3*(zone-1)+2,2] <- 4
  estimators_predators[3*(zone-1)+3,2] <- 5 
  
  # 3.1) parameters for models 1 and 2 
  for (j in 3:7) {
    
    
    estimators_vole[2*(zone-1)+1,j] <- model1$coefficients[j-2]
    estimators_vole[2*(zone-1)+2,j] <- model2$coefficients[j-2]
    
  }
  
  # 3.2) parameters for models 3,4 and 5  
  for (j in 3:9) {
    
    estimators_predators[3*(zone-1)+1,j] <- model3$coefficients[j-2]
    estimators_predators[3*(zone-1)+2,j] <- model4$coefficients[j-2]
    estimators_predators[3*(zone-1)+3,j] <- model5$coefficients[j-2]
    
  }
  
  # 4) error's variance 
  estimators_vole[2*(zone-1)+1,8] <- summary.lm(model1)$sigma
  estimators_vole[2*(zone-1)+2,8] <- summary.lm(model2)$sigma
  
  estimators_predators[3*(zone-1)+1,10] <- summary.lm(model3)$sigma
  estimators_predators[3*(zone-1)+2,10] <- summary.lm(model4)$sigma
  estimators_predators[3*(zone-1)+3,10] <- summary.lm(model5)$sigma
  
  # we can end the zone's loop 
}

# we can register our data frame in csv 
write.csv (estimators_vole, "../data_supp/parameters_vole_step3.csv", row.names = T, quote = F)
write.csv (estimators_predators, "../data_supp/parameters_predators_step3.csv", row.names = T, quote = F)

#----------------------------------------------------------------------------------------------------------

# II Simulations 

# We create the data frame with data that we will produce 

# simulation's indicators 
simulations <-data.frame(matrix(0,24,10)) 
names(simulations) <- c("scenario","zone","direct density dependence","delayed density dependence","s-index","min_s","max_s","seasonality","min_n","max_n")

# vole density 
Vole <- data.frame(matrix(0,2000,24)) 
names(Vole) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in autumn with a 2 years gap 
Vole_autumn_2 <- data.frame(matrix(0,1000,24)) 
names(Vole_autumn_2) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in autumn with a 1 years gap 
Vole_autumn_1 <- data.frame(matrix(0,1000,24)) 
names(Vole_autumn_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in spring with a 1 years gap 
Vole_spring_1 <- data.frame(matrix(0,1000,24)) 
names(Vole_spring_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole density in spring without  years gap 
Vole_spring <- data.frame(matrix(0,1000,24)) 
names(Vole_spring) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole gowth rate in winter 
Yt_1 <- data.frame(matrix(0,1000,24)) 
names(Yt_1) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")

# vole gowth rate in summer 
Yt_2 <- data.frame(matrix(0,1000,24)) 
names(Yt_2) <- c("1n","1e","1o","2n","2e","2o","3n","3e","3o","4n","4e","4o","5n","5e","5o","6n","6e","6o","7n","7e","7o","8n","8e","8o")


# We open the parameters estimated previously and create variable to stock them
parameters <- read.csv("../data_supp/parameters_vole_step3.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)
parameters_pred <- read.csv("../data_supp/parameters_predators_step3.csv", sep=",", header=T, dec=".",stringsAsFactors=FALSE)


# Now we can do the simulations for each zone (3) and each scenario (8) , ie 24 simulations 
for (scenario in 1:8){ 
  
  for (zone in 1:3){
    
    # the simulation's number 
    sim <- 3*(scenario-1)+zone
    
    # for each zone, we have to select data and estimators for this zone 
    if (zone==1){
      
      Present <- filter(data,zone=="north",year==2011)
      Delay <- filter(data,zone=="north",year==2010)
      
      a1 <- parameters[1,4]
      b1 <- parameters[1,5]
      d1 <- parameters[1,6]
      e1 <- parameters[1,7]
      f1 <- parameters[1,8]
      sigma1 <- parameters[1,9]
      
      a2 <- parameters[2,4]
      b2 <- parameters[2,5]
      d2 <- parameters[2,6]
      e2 <- parameters[2,7]
      f2 <- parameters[2,8]
      sigma2 <- parameters[2,9]
      
      a3 <- parameters_pred[1,4]
      alpha3 <- parameters_pred[1,5]
      b3 <- parameters_pred[1,6]
      c3 <- parameters_pred[1,7]
      d3 <- parameters_pred[1,8]
      e3 <- parameters_pred[1,9]
      f3 <- parameters_pred[1,10]
      sigma3 <- parameters_pred[1,11]
      
      a4 <- parameters_pred[2,4]
      alpha4 <- parameters_pred[2,5]
      b4 <- parameters_pred[2,6]
      c4 <- parameters_pred[2,7]
      d4 <- parameters_pred[2,8]
      e4 <- parameters_pred[2,9]
      f4 <- parameters_pred[2,10]
      sigma4 <- parameters_pred[2,11]
      
      a5 <- parameters_pred[3,4]
      alpha5 <- parameters_pred[3,5]
      b5 <- parameters_pred[3,6]
      c5 <- parameters_pred[3,7]
      d5 <- parameters_pred[3,8]
      e5 <- parameters_pred[3,9]
      f5 <- parameters_pred[3,10]
      sigma5 <- parameters_pred[3,11]
      
    }
    
    if (zone==2){
      
      Present <- filter(data,zone=="east",year==2011)
      Delay <- filter(data,zone=="east",year==2010)
      
      a1 <- parameters[3,4]
      b1 <- parameters[3,5]
      d1 <- parameters[3,6]
      e1 <- parameters[3,7]
      f1 <- parameters[3,8]
      sigma1 <- parameters[3,9]
      
      a2 <- parameters[4,4]
      b2 <- parameters[4,5]
      d2 <- parameters[4,6]
      e2 <- parameters[4,7]
      f2 <- parameters[4,8]
      sigma2 <- parameters[4,9]
      
      a3 <- parameters_pred[4,4]
      alpha3 <- parameters_pred[4,5]
      b3 <- parameters_pred[4,6]
      c3 <- parameters_pred[4,7]
      d3 <- parameters_pred[4,8]
      e3 <- parameters_pred[4,9]
      f3 <- parameters_pred[4,10]
      sigma3 <- parameters_pred[4,11]
      
      a4 <- parameters_pred[5,4]
      alpha4 <- parameters_pred[5,5]
      b4 <- parameters_pred[5,6]
      c4 <- parameters_pred[5,7]
      d4 <- parameters_pred[5,8]
      e4 <- parameters_pred[5,9]
      f4 <- parameters_pred[5,10]
      sigma4 <- parameters_pred[5,11]
      
      a5 <- parameters_pred[6,4]
      alpha5 <- parameters_pred[6,5]
      b5 <- parameters_pred[6,6]
      c5 <- parameters_pred[6,7]
      d5 <- parameters_pred[6,8]
      e5 <- parameters_pred[6,9]
      f5 <- parameters_pred[6,10]
      sigma5 <- parameters_pred[6,11]
      
    }
    
    if (zone==3){
      Present <- filter(data,zone=="west",year==2011)
      Delay <- filter(data,zone=="west",year==2010)
      
      a1 <- parameters[5,4]
      b1 <- parameters[5,5]
      d1 <- parameters[5,6]
      e1 <- parameters[5,7]
      f1 <- parameters[5,8]
      sigma1 <- parameters[5,9]
      
      a2 <- parameters[6,4]
      b2 <- parameters[6,5]
      d2 <- parameters[6,6]
      e2 <- parameters[6,7]
      f2 <- parameters[6,8]
      sigma2 <- parameters[6,9]
      
      a3 <- parameters_pred[7,4]
      alpha3 <- parameters_pred[7,5]
      b3 <- parameters_pred[7,6]
      c3 <- parameters_pred[7,7]
      d3 <- parameters_pred[7,8]
      e3 <- parameters_pred[7,9]
      f3 <- parameters_pred[7,10]
      sigma3 <- parameters_pred[7,11]
      
      a4 <- parameters_pred[8,4]
      alpha4 <- parameters_pred[8,5]
      b4 <- parameters_pred[8,6]
      c4 <- parameters_pred[8,7]
      d4 <- parameters_pred[8,8]
      e4 <- parameters_pred[8,9]
      f4 <- parameters_pred[8,10]
      sigma4 <- parameters_pred[8,11]
      
      a5 <- parameters_pred[9,4]
      alpha5 <- parameters_pred[9,5]
      b5 <- parameters_pred[9,6]
      c5 <- parameters_pred[9,7]
      d5 <- parameters_pred[9,8]
      e5 <- parameters_pred[9,9]
      f5 <- parameters_pred[9,10]
      sigma5 <- parameters_pred[9,11]
      
    }
    
    # we initialize Vole density
    At <- mean(na.omit(Present$Vole.Autumn))
    At_1 <- mean(na.omit(Delay$Vole.Autumn))
    St <- mean(na.omit(Present$Vole.Spring))
    St_1 <- mean(na.omit(Delay$Vole.Spring)) 
    
    # we initialize predators density depending of the scenario 
    if (scenario ==2 ||scenario ==5 ||scenario ==7 ||scenario ==8 ){
      
      P1t  <-min(na.omit(data$Small.mustelid))
      P1t_1 <-min(na.omit(data$Small.mustelid))
      
    }
    
    else {
      
      P1t <- mean(na.omit(Present$Small.mustelid))
      P1t_1 <- mean(na.omit(Present$Small.mustelid))
      
    }
    
    if (scenario ==3 ||scenario ==4 ||scenario ==7 ||scenario ==8 ){
      
      P2t <-min(na.omit(data$Generalist.predator))
      
    }
    
    else {
      
      P2t <- mean(na.omit(Present$Generalist.predator))
      
    }    
    
    if (scenario ==3 ||scenario ==5 ||scenario ==6 ||scenario ==8 ){
      
      P3t <- min(na.omit(data$Avian.predator))
      
    }
    
    else {
      
      P3t <-mean(na.omit(Present$Avian.predator))
      
    }  
    
    # We create some vectors and initialize some variables 
    seasonality_vector <- 1:1000
    season <- 0
    
    # we can begin our 1000 years loop 
    for (i in 1:1000){
      # Chronologically calculated 
      # Predators are calculated only if there are taken in the model 
      # Winter 
      if (scenario ==1 ||scenario ==3 ||scenario ==4 ||scenario ==6 ){
        P1t_1 <- P1t
        P1t <- a3 + (alpha3+1)*P1t +b3*At +c3*St + d3*At_1 + e3*P2t + f3*P3t + rnorm(1,0,sigma3) 
      }
      if (scenario ==1 ||scenario ==2 ||scenario ==5 ||scenario ==6 ){
        P2t <- a4 + (alpha4+1)*P2t +b4*At +c4*St +d4*At_1 + e4*P1t_1 + f4*P3t + rnorm(1,0,sigma4)
      }
      # Spring
      St_2 <- St_1
      St_1 <- St
      St <- a1 + (b1+1)*At +d1*P1t +e1*P2t + f1*P3t+ rnorm(1,0,sigma1) 
      # Summer 
      if (scenario ==1 ||scenario ==2 ||scenario ==4 ||scenario ==7 ){
        P3t <- a5 + (alpha5+1)*P3t +b5*St +c5*At +d5*St_1+ e5*P1t + f5*P2t + rnorm(1,0,sigma5) 
      }
      # Autumn 
      At_2 <- At_1
      At_1 <- At 
      At <- a2 + (b2+1)*St +d2*P1t +e2*P2t +f2*P3t+ rnorm(1,0,sigma2)  
      # Vole density Storage 
      Vole_autumn_2[i,sim] <- At_2
      Vole_autumn_1[i,sim] <- At_1
      Vole_spring_1[i,sim] <- St_1
      Vole_spring[i,sim] <- St
      Vole[2*i-1,sim] <- St
      Vole[2*i,sim] <- At
      # Growth rate 
      Yt_1[i,sim] <- St-At_1
      Yt_2[i,sim] <- At-St
      # seasonality 
      season <- season + (At-St)-(St-At_1)
      seasonality_vector[i] <- (At-St)-(St-At_1)
      
      # end of the 1000 years loop 
    }
    
    # calcul : from seasonal density dependence to annual density dependancd 
    Density_dependence_1 <- lm(formula = Yt_1[,sim] ~ Vole_autumn_1[,sim] +  Vole_autumn_2[,sim] )
    Density_dependence_2 <- lm(formula = Yt_2[,sim] ~ Vole_spring[,sim] +  Vole_spring_1[,sim] )
    
    B1 <- Density_dependence_1$coefficient[2]
    C1 <- Density_dependence_1$coefficient[3]
    B2 <- Density_dependence_2$coefficient[2]
    C2 <- Density_dependence_2$coefficient[3]
    
    DR <- B1 + B2 + B2*B1 - 0.20 
    DL <- C1 + C2 + C2*B1 + B2*C1 
    
    # we complete our data table 
    simulations[sim,1] <- scenario
    simulations[sim,2] <- zone
    simulations[sim,3] <- DR
    simulations[sim,4] <- DL
    simulations[sim,5] <- sqrt(var(Vole[,sim]))
    simulations[sim,6] <- sqrt((999)*var(Vole[,sim])/qchisq(p=0.975,df=999))
    simulations[sim,7] <- sqrt((999)*var(Vole[,sim])/qchisq(p=0.025,df=999))
    simulations[sim,8] <- season/1000
    simulations[sim,9] <- season/1000-1.96*(var(seasonality_vector)/sqrt(1000))  
    simulations[sim,10] <- season/1000+1.96*(var(seasonality_vector)/sqrt(1000)) 
    
    # end of the zone's loop
  }
  
  # end of the scenario's loop 
}

# we register our results 
write.csv (Vole, "../data_supp/vole_density_simulations_step3.csv", row.names = T, quote = F) 
write.csv (simulations, "../data_supp/simulations_indicators_step3.csv",row.names=T,quote=F)

#--------------------------------------------------------------------------------------------------------

# III Figures : direct and delayed density dependence + s-index 

simulations <- read.csv("../data_supp/simulations_indicators_step3.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# data cleaning 
simulations$scenario <- as.integer(simulations$scenario)
simulations$direct.density.dependence_p <- simulations$direct.density.dependence+1

simulations_north <- filter(simulations,zone==1)
simulations_east <- filter(simulations,zone==2)
simulations_west <- filter(simulations,zone==3)

# A) density dependence 

# A.1 north 

a1 <- ggplot(simulations_north) +
  geom_point(size=6,aes(x = direct.density.dependence_p,y = delayed.density.dependence,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependence north") +
  xlab("direct density dependence+1") + ylab("delayed density dependence")+ theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.background = element_rect(fill="grey70",linewidth=1, linetype="solid", colour ="black"),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.title = element_text(colour="black", size=20, face="bold"))

a1



# A.2 west  

a2 <- ggplot(simulations_west) +
  geom_point(size=6,aes(x =direct.density.dependence_p,y =delayed.density.dependence,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependence west") +
  xlab("direct density dependence+1") + ylab("delayed density dependence")+ theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

a2

# A.3 east 

a3 <- ggplot(simulations_east) +
  geom_point(size=6,aes(x =direct.density.dependence_p,y =delayed.density.dependence,color = factor(scenario)))+
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  geom_function(fun=function(x) -x^2 / 4, xlim=c(-2, 2))+
  geom_segment(aes(x = -2, y = -1, xend = 0, yend = 1))+
  geom_segment(aes(x = 0, y = 1, xend = 2, yend = -1))+
  geom_segment(aes(x = -2, y = -1, xend = 2, yend = -1))+
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))+
  ggtitle("density dependence east") +
  xlab("direct density dependence+1") + ylab("delayed density dependence")+ theme_classic()+
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

a3

legend_a <- get_legend(a1+ theme(legend.position = 'right'))

# A.4 Global figure and register 

A <- plot_grid(a1+ theme(legend.position = 'none'),a2,a3, legend_a ,labels=c("1", "2","3",""), ncol = 2, nrow = 2)

A

pdf("../Figures_supp/graph_triangle_step3.pdf",  
    width = 16, height = 12, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

# B) s-index

# B.1 north 

b1 <- ggplot(simulations_north, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index north ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b1

# B.2 west 

b2 <- ggplot(simulations_west, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index west ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b2

# B.3 east  

b3 <- ggplot(simulations_east, aes(x = factor(scenario), y=s.index, color=factor(scenario))) +
  geom_errorbar(aes(ymin = min_s, ymax = max_s), colour="black", width=0.4, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=8, shape=20) +
  xlab("scenario") + ylab("s-index")+
  ggtitle("s-index east ") +
  scale_color_manual(name="scenario",values=c('black','pink','red',"yellow","orange","lightseagreen","darkblue","cornsilk4"))+
  theme_classic() +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"),
        legend.position='none')

b3


# B.4 Global figure and register 

B <- plot_grid(b1,b2,b3, labels=c("1", "2","3"), ncol = 3, nrow = 1)
B

pdf("../Figures_supp/graph_s_index_step3.pdf",  
    width = 12, height = 6, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(B)

# Closing the graphical device
dev.off() 

#-------------------------------------------------------------------------------------------------------------

# IV simulations figure (vole density)

# data importation 
Vole <- read.csv("../data_supp/vole_density_simulations_step3.csv",sep=",", header=T, dec=".", stringsAsFactors=FALSE)

# S1 - North 

s1n_data <- Vole[1:100,1:2]

s1n_graph <- ggplot(data=s1n_data,aes(x=s1n_data[,1],y=s1n_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1n_graph

a1 <- acf(s1n_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - North 

s2n_data <- Vole[1:100,c(1,5)]

s2n_graph <- ggplot(data=s2n_data,aes(x=s2n_data[,1],y=s2n_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2n_graph

a2 <- acf(s2n_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - North 

s3n_data <- Vole[1:100,c(1,8)]

s3n_graph <- ggplot(data=s3n_data,aes(x=s3n_data[,1],y=s3n_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3n_graph

a3 <- acf(s3n_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - North 

s4n_data <- Vole[1:100,c(1,11)]

s4n_graph <- ggplot(data=s4n_data,aes(x=s4n_data[,1],y=s4n_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4n_graph

a4 <- acf(s4n_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - North 

s5n_data <- Vole[1:100,c(1,14)]

s5n_graph <- ggplot(data=s5n_data,aes(x=s5n_data[,1],y=s5n_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5n_graph

a5 <- acf(s5n_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - North 

s6n_data <- Vole[1:100,c(1,17)]

s6n_graph <- ggplot(data=s6n_data,aes(x=s6n_data[,1],y=s6n_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6n_graph

a6 <- acf(s6n_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - North 

s7n_data <- Vole[1:100,c(1,20)]

s7n_data[,1] <- as.numeric(s7n_data[,1])
s7n_data[,2] <- as.numeric(s7n_data[,2])

s7n_graph <- ggplot(data=s7n_data,aes(x=s7n_data[,1],y=s7n_data[,2],group=1))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7n_graph

a7 <- acf(s7n_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - North 

s8n_data <- Vole[1:100,c(1,23)]

s8n_graph <- ggplot(data=s8n_data,aes(x=s8n_data[,1],y=s8n_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 north") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8n_graph

a8 <- acf(s8n_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure north and register 

A <- plot_grid(s1n_graph,a1_acf,s2n_graph,a2_acf,s3n_graph,a3_acf,s4n_graph,a4_acf,s5n_graph,a5_acf,s6n_graph,a6_acf,s7n_graph,a7_acf,s8n_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# Opening the graphic device 
pdf("../Figures_supp/sim_north_step3.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(A)

# Closing the graphical device
dev.off() 

# S1 - East

s1e_data <- Vole[1:100,c(1,3)]

s1e_graph <- ggplot(data=s1e_data,aes(x=s1e_data[,1],y=s1e_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1e_graph

a1 <- acf(s1e_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - East 

s2e_data <- Vole[1:100,c(1,6)]

s2e_graph <- ggplot(data=s2e_data,aes(x=s2e_data[,1],y=s2e_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2e_graph

a2 <- acf(s2e_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - East

s3e_data <- Vole[1:100,c(1,9)]

s3e_graph <- ggplot(data=s3e_data,aes(x=s3e_data[,1],y=s3e_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3e_graph

a3 <- acf(s3e_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - East

s4e_data <- Vole[1:100,c(1,12)]

s4e_graph <- ggplot(data=s4e_data,aes(x=s4e_data[,1],y=s4e_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4e_graph

a4 <- acf(s4e_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - East

s5e_data <- Vole[1:100,c(1,15)]

s5e_data[,1] <- as.numeric(s5e_data[,1])
s5e_data[,2] <- as.numeric(s5e_data[,2])

s5e_graph <- ggplot(data=s5e_data,aes(x=s5e_data[,1],y=s5e_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5e_graph

a5 <- acf(s5e_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - East 

s6e_data <- Vole[1:100,c(1,18)]

s6e_graph <- ggplot(data=s6e_data,aes(x=s6e_data[,1],y=s6e_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6e_graph

a6 <- acf(s6e_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - East

s7e_data <- Vole[1:100,c(1,21)]

s7e_graph <- ggplot(data=s7e_data,aes(x=s7e_data[,1],y=s7e_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7e_graph

a7 <- acf(s7e_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - East

s8e_data <- Vole[1:100,c(1,24)]

s8e_graph <- ggplot(data=s8e_data,aes(x=s8e_data[,1],y=s8e_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 east") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8e_graph

a8 <- acf(s8e_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure east and register 

B <- plot_grid(s1e_graph,a1_acf,s2e_graph,a2_acf,s3e_graph,a3_acf,s4e_graph,a4_acf,s5e_graph,a5_acf,s6e_graph,a6_acf,s7e_graph,a7_acf,s8e_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)

# Opening the graphic device 
pdf("../Figures_supp/sim_east_step3.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(B)

# Closing the graphical device
dev.off() 


# S1 - West

s1o_data <- Vole[1:100,1:4]

s1o_graph <- ggplot(data=s1o_data,aes(x=s1o_data[,1],y=s1o_data[,2]))+geom_line(color="black",size=1) +
  ggtitle("Vole s1 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s1o_graph

a1 <- acf(s1o_data[,2])
a1_data <- with(a1, data.frame(lag, acf))
a1_acf <- ggplot(data=a1_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a1_acf

# S2 - West 

s2o_data <- Vole[1:100,c(1,7)]

s2o_graph <- ggplot(data=s2o_data,aes(x=s2o_data[,1],y=s2o_data[,2]))+geom_line(color="darkmagenta",size=1) +
  ggtitle("Vole s2 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s2o_graph

a2 <- acf(s2o_data[,2])
a2_data <- with(a2, data.frame(lag, acf))
a2_acf <- ggplot(data=a2_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a2_acf

# S3 - West

s3o_data <- Vole[1:100,c(1,10)]

s3o_graph <- ggplot(data=s3o_data,aes(x=s3o_data[,1],y=s3o_data[,2]))+geom_line(color="red",size=1) +
  ggtitle("Vole s3 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s3o_graph

a3 <- acf(s3o_data[,2])
a3_data <- with(a3, data.frame(lag, acf))
a3_acf <- ggplot(data=a3_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a3_acf

# S4 - West

s4o_data <- Vole[1:100,c(1,13)]

s4o_graph <- ggplot(data=s4o_data,aes(x=s4o_data[,1],y=s4o_data[,2]))+geom_line(color="goldenrod1",size=1) +
  ggtitle("Vole s4 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s4o_graph

a4 <- acf(s4o_data[,2])
a4_data <- with(a4, data.frame(lag, acf))
a4_acf <- ggplot(data=a4_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a4_acf

# S5 - West

s5o_data <- Vole[1:100,c(1,16)]

s5o_graph <- ggplot(data=s5o_data,aes(x=s5o_data[,1],y=s5o_data[,2]))+geom_line(color="darkorange3",size=1) +
  ggtitle("Vole s5 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s5o_graph

a5 <- acf(s5o_data[,2])
a5_data <- with(a5, data.frame(lag, acf))
a5_acf <- ggplot(data=a5_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a5_acf

# S6 - West

s6o_data <- Vole[1:100,c(1,19)]

s6o_graph <- ggplot(data=s6o_data,aes(x=s6o_data[,1],y=s6o_data[,2]))+geom_line(color="lightseagreen",size=1) +
  ggtitle("Vole s6 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s6n_graph

a6 <- acf(s6o_data[,2])
a6_data <- with(a6, data.frame(lag, acf))
a6_acf <- ggplot(data=a6_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a6_acf

# S7 - West

s7o_data <- Vole[1:100,c(1,22)]

s7o_graph <- ggplot(data=s7o_data,aes(x=s7o_data[,1],y=s7o_data[,2]))+geom_line(color="darkblue",size=1) +
  ggtitle("Vole s7 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s7o_graph

a7 <- acf(s7o_data[,2])
a7_data <- with(a7, data.frame(lag, acf))
a7_acf <- ggplot(data=a7_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a7_acf

# S8 - West

s8o_data <- Vole[1:100,c(1,25)]

s8o_graph <- ggplot(data=s8o_data,aes(x=s8o_data[,1],y=s8o_data[,2]))+geom_line(color="grey30",size=1) +
  ggtitle("Vole s8 west") +
  xlab("time") + ylab("Vole index") + theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))

s8o_graph

a8 <- acf(s8o_data[,2])
a8_data <- with(a8, data.frame(lag, acf))
a8_acf <- ggplot(data=a8_data, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+ theme_classic()+ 
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5),
        axis.title.x = element_text( size=18, face="bold"),
        axis.title.y = element_text( size=18, face="bold"),
        axis.text.x = element_text(face="bold", size=12,color="black") ,
        axis.text.y = element_text(face="bold", size=12,color="black"))
a8_acf

# Global figure west 

C <- plot_grid(s1o_graph,a1_acf,s2o_graph,a2_acf,s3o_graph,a3_acf,s4o_graph,a4_acf,s5o_graph,a5_acf,s6o_graph,a6_acf,s7o_graph,a7_acf,s8o_graph,a8_acf, labels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), ncol = 2, nrow = 8)
# Opening the graphic device 
pdf("../Figures_supp/sim_west_step3.pdf",  
    width = 8, height = 18, 
    bg = "white",         
    colormodel = "cmyk")

# Creating a plot
plot(C)

# Closing the graphical device
dev.off() 

