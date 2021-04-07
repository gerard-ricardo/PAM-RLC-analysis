#1) Label your file names correctly

# 2) Import RLC data
setwd("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/180119 rlc/r mil csvs")
library(tidyr)
library(readr)
library(dplyr)
library(plyr)
# data1 <- list.files(full.names = TRUE) %>% lapply(read_delim, delim = ';') %>% bind_rows #Import multiple csvs into one dataframe
read_csv_filename1 <- function(filename){
  ret <- read_csv(filename)
  ret$disc <- filename #EDIT
  ret
}   #also read_delim(delim = ';')
filenames = list.files(full.names = TRUE)
data1 <- ldply(filenames, read_csv_filename1)
options(scipen = 999)  # turn off scientific notation
str(data1) #check data type is correct
data1$disc <- as.factor(as.character(data1$disc))
data1$no.f <- as.factor(as.character(data1$No.))
data1$disc = data1 %>% separate(disc, c("a", "b", 'c', 'd')) %>% .$b

#3) Import and join Env. treatment data
env.fact <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header= TRUE,dec=",", na.strings=c("",".","NA"))
str(env.fact)
env.fact$dli <- as.numeric(as.character(env.fact$dli))
env.fact$disc <- as.factor(as.character(env.fact$disc))
env.fact$spp <- as.factor(as.character(env.fact$spp))
env.fact$spec <- as.factor(as.character(env.fact$spec))
data2 = left_join(data1, env.fact, by = 'disc')  #joined only for the data
data1 = data2
#Trim the fat and long form
data.s = dplyr::select (data1,c('disc', 'PAR','dli', 'spec', 'Y(II)1','Y(II)2', 'Y(II)3'))  #remove column
str(data.s)
data.s$disc <- as.factor(as.character(data.s$disc))
data1.long <- data.s %>% pivot_longer(-c(disc, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")
data1.long <- arrange(data1.long, spec, dli, rep)
data1.long = data1.long[complete.cases(data1.long), ]
data1.long$id <- paste(data1.long$disc, data1.long$rep, sep = "")  #individual ID
str(data1.long)
data1.long$rep <- as.factor(as.character(data1.long$rep))
data1.long$id <- as.factor(as.character(data1.long$id))

# #################rETR#################
#4)
data1.long$rETR = data1.long$PAR * data1.long$meas
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
p0 = ggplot()+geom_point(data1.long, mapping = aes(x = PAR, y = rETR), size = 1 )+facet_wrap(~id)
p0

#Data cleaning
data1.l = data1.long[which(data1.long$rETR<100),]   #remove rTER anomalies
data1.l = data1.l[which(data1.l$PAR<1200),]   #remove highest treatment
p0 = ggplot()+geom_point(data1.l, mapping = aes(x = PAR, y = rETR), size = 1 ) + facet_wrap(~disc+rep)
p0  #clean multi-plot

#########Modeling################
#####Fit a single RLC curve####
#Add small amount to x and y to allow for fit of model
data1.s$`m001 Y(II)`$PAR <- ifelse(data1.s$`m001Y(II)1`$PAR <= 0, 0.1, data1.s$`m001Y(II)1`$PAR)  #
data1.s$`m001 Y(II)1`$rETR <- ifelse(data1.s$`m001Y(II)1`$rETR <= 0, 0.01, data1.s$`m001Y(II)1`$rETR)  #

data1.s = split(data1.l, data1.l$id)
data1.s$`m001Y(II)1`
p0 = ggplot()+geom_point(data1.s$`m001Y(II)1`, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek1() 
p0

source("https://raw.githubusercontent.com/gerard-ricardo/data/master/ssplattmy")  #for starting values
library(minpack.lm)
start = unname(getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Pmax), data1.s$`m001Y(II)1`))
md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=start[3],alpha=start[1], beta=start[2]), data = data1.s$`m001Y(II)1`)  
df.x <- data.frame(PAR = seq(0.1, 926, length = 100)) #setting up  new  data frame (df) defining log.x values to run 
vec.x =df.x[,1]
plot(data1.s$`m001Y(II)1`$PAR, data1.s$`m001Y(II)1`$rETR, col = 'red')
lines(vec.x, predict(md1, df.x)) #looks good for m001Y(II)1

#####Fit multiple RLC curves####
data1.l$PAR <- ifelse(data1.l$PAR <= 0, 0.1, data1.l$PAR)  #
data1.l$rETR <- ifelse(data1.l$rETR <= 0, 0.01, data1.l$rETR)  #
#find multiple starts
starts = data1.l %>% group_by(id) %>% do(broom::tidy(stats::getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Ys), data = . ))) %>% 
  pivot_wider(names_from = names, values_from = x, names_prefix = "") %>% dplyr::select (.,-c('NA'))
colnames(starts) <- c("id", "alpha.s", 'beta.s', 'Pmax.s') 
library(IDPmisc)
starts = NaRV.omit(starts) #removes inf

library(dplyr)
library(nlme)
test2 = data1.l  %>% right_join(.,starts, by = 'id') %>% 
  group_by(id) %>%
  do(model = try(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
                       start = list(Pmax = mean(.$Pmax.s),
                                    alpha = mean(.$alpha.s),
                                    beta = mean(.$beta.s)),
                       data = .),silent = TRUE))   #this gets models for all models

test2$model[[1]]  #check for model 1
#run loop to predict for all models
usq=list()#
for(i in 1:nrow(test2)) {
  out <- try(predict(test2$model[[i]], df.x))
  usq=c(usq,list(out))
}
usq
df3 = data.frame(t(do.call(rbind.data.frame, usq)), row.names = paste0("", 1:100))  #put all prediciton in data.frame
str(df3)
names = test2$id
names <- as.factor(as.character(names))  #add col names
colnames(df3) <- names
df3[] <- lapply(df3, function(x) as.numeric(as.character(x)))   #convert all to numeric which adds NAs for error
df3$PAR = df.x$PAR
df3.long = df3 %>% pivot_longer(-PAR,  names_to = "id" ,values_to = "rETR") %>% data.frame() #keep vec.x, add all other columns to factors , add all their values to meas)
df3.long = dplyr::arrange(df3.long, id) 
str(df3.long)
df3.long$rETR <- as.numeric(as.character(df3.long$rETR))  #add col names
p0 = ggplot()+geom_point(data1.l, mapping = aes(x = PAR, y = rETR), size = 1 )
p0 = p0 + geom_line(df3.long, mapping = aes(x = PAR, y = rETR))
p0 = p0 + facet_wrap(~id)
p0  #clean multi-plot

####extract  parameters from all RLC######
test = data1.l  %>% right_join(.,starts, by = 'id') %>% 
  group_by(id) %>%
  do(model = try(broom::tidy(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
                                   start = list(Pmax = mean(.$Pmax.s),
                                                alpha = mean(.$alpha.s),
                                                beta = mean(.$beta.s)),
                                   data = .),silent = TRUE)) )  #this get parameters for all models

test$model[[1]]  #check for model 1

unest.test = test %>% unnest(model)
df.param  = dplyr::select(unest.test, c(id, term, estimate))
dat_wide <- df.param %>% pivot_wider(names_from = term, values_from = estimate)  %>% dplyr::select(.,-c("NA")) #year goes to columns, their areas go as the values, area is the prefix
dat_wide$ETRm = dat_wide$Pmax*(dat_wide$alpha/(dat_wide$alpha+dat_wide$beta))*((dat_wide$beta/(dat_wide$alpha+dat_wide$beta)))^(dat_wide$beta/dat_wide$alpha)
dat_wide$Ek = dat_wide$ETRm/dat_wide$alpha
dat_wide$Em =(dat_wide$Pmax/dat_wide$alpha)*log((dat_wide$alpha+dat_wide$beta)/dat_wide$beta)
final.df = left_join(dat_wide, data1.long, by = 'id')
