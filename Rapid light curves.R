#1) Label your file names correctly

# 2) Import RLC data
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/180119 rlc/r csvs")
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
head(data1)
options(scipen = 999)  # turn off scientific notation
str(data1) #check data type is correct
data1$disc <- as.factor(as.character(data1$disc))
data1$no.f <- as.factor(as.character(data1$No.))
data1$disc1 = data1 %>% separate(disc, c("a", "b", 'c', 'd')) %>% .$b

#3) Import and join Env. treatment data
env.fact <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header= TRUE,dec=",", na.strings=c("",".","NA"))
str(env.fact)
env.fact$dli <- as.numeric(as.character(env.fact$dli))
env.fact$disc1 = env.fact$disc
data2 = left_join(data1, env.fact, by = 'disc1')  #joined only for the data
data1 = data2
#Trim the fat and long form
data.s = dplyr::select (data1,c('disc1', 'PAR','dli', 'spec', 'Y(II)1','Y(II)2', 'Y(II)3'))  #remove column
data1$disc1 <- as.factor(as.character(data1$disc1))
data1.long <- data.s %>% pivot_longer(-c(disc1, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")
data1.long <- arrange(data1.long, spec, dli, rep)
data1.long = data1.long[complete.cases(data1.long), ]
data1.long$id <- paste(data1.long$disc1, data1.long$rep, sep = "")  #individual ID
str(data1.long)
data1.long$disc1 <- as.factor(as.character(data1.long$disc1))
data1.long$rep <- as.factor(as.character(data1.long$rep))
data1.long$id <- as.factor(as.character(data1.long$id))

# #Create two 0.01 DLI for each spectra
# data2.t8 <- subset(data2, dli== 0.01)  #subset 0.01
# data2.t8$spec = rep('w', nrow(data2.t8)) #overwrite y to w
# data3 = rbind(data2, data2.t8)
# data1 = data3



# #################rETR#################
#4)
data1.long$rETR = data1.long$PAR * data1.long$meas
library(ggplot2)
source("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/1 my functions/theme sleek.R")
theme_set(theme_sleek())
p0 = ggplot()+geom_point(data1.long, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek()+facet_wrap(~id)
p0

#Data cleaning
data1.l = data1.long[which(data1.long$rETR<100),]   #remove rTER anomalies
data1.l = data1.l[which(data1.l$PAR<1200),]   #remove highest treatment
p0 = ggplot()+geom_point(data1.l, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek() + facet_wrap(~disc1+rep)
p0

#Modelling
#Fit one
data1.s = split(data1.l, data1.l$id)
data1.s$`m001Y(II)1`
p0 = ggplot()+geom_point(data1.s$`m001Y(II)1`, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek() 
p0
#Add small amount to x and y to allow for fit of model
data1.s$`m001 Y(II)`$PAR <- ifelse(data1.s$`m001Y(II)1`$PAR <= 0, 0.1, data1.s$`m001Y(II)1`$PAR)  #
data1.s$`m001 Y(II)1`$rETR <- ifelse(data1.s$`m001Y(II)1`$rETR <= 0, 0.01, data1.s$`m001Y(II)1`$rETR)  #

########Starting values#####################################################################
source("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/1 my functions/SSPlatt.my.R")
library(minpack.lm)
md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=-30,alpha=0.35, beta=-0.5), data = data1.s$`m001Y(II)1`)  
#Ys = Ys, apha = inital slope, beta = second slope, 
# library(stats)
# library(Platt)
# df = data1.s$`m001Y(II)1` %>%  data.frame()
# getInitial(rETR ~ SSPlatt0(I, alpha, beta, Pmax), data = df)
# start = print(getInitial(rETR ~ SSPlatt0(PAR, alpha, beta, Pmax), data1.s$`m001 Y(II)1`) )
# str(start.df)
# start.df = start %>% unlist()%>% t(.) %>% data.frame() 
# start.df$Pmax = start.df$Pmax*-1
# start = as.list(getInitial(rETR ~ SSPlatt(PAR, alpha, beta, Pmax), data1.s$`m001 Y(II)1` ))
# library(minpack.lm)
# md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=38.77,alpha=0.35, beta=-0.295), data = data1.s$`m001Y(II)1`)  
df.x <- data.frame(PAR = seq(0.1, 926, length = 100)) #setting up  new  data frame (df) defining log.x values to run 
vec.x =df.x[,1]
plot(data1.s$`m001Y(II)1`$PAR, data1.s$`m001Y(II)1`$rETR, col = 'red')
lines(vec.x, predict(md1, df.x)) #looks good


##All model fits
data1.l$PAR <- ifelse(data1.l$PAR <= 0, 0.1, data1.l$PAR)  #
data1.l$rETR <- ifelse(data1.l$rETR <= 0, 0.01, data1.l$rETR)  #

#Intial values
source("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/1 my functions/SSPlatt.my.R")
starts = data1.l %>% group_by(id) %>% do(broom::tidy(stats::getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Ys), data = . ))) %>% 
    pivot_wider(names_from = names, values_from = x, names_prefix = "") %>% dplyr::select (.,-c('NA'))
colnames(starts) <- c("id", "alpha.s", 'beta.s', 'Pmax.s') 

library(IDPmisc)
starts = NaRV.omit(starts)

starts$id

data1.l$id

##########6)The models#############################
library(dplyr)
library(nlme)
# groupedData(rETR ~ PAR | id, data1.l)

#Does work. But note that right join instead of left may remove some before error
test = data1.l  %>% right_join(.,starts, by = 'id') %>% 
  group_by(id) %>%
do(model = try(broom::tidy(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
                     start = list(Pmax = mean(.$Pmax.s),
                          alpha = mean(.$alpha.s),
                          beta = mean(.$beta.s)),
                     data = .),silent = TRUE)) )


test$model




unest.test = test %>%
  unnest(model)
df.param  = dplyr::select(unest.test, c(id, term, estimate))
dat_wide <- df.param %>% pivot_wider(names_from = term, values_from = estimate)  %>% dplyr::select(.,-c("NA")) #year goes to columns, their areas go as the values, area is the prefix
dat_wide$ETRm = dat_wide$Pmax*(dat_wide$alpha/(dat_wide$alpha+dat_wide$beta))*((dat_wide$beta/(dat_wide$alpha+dat_wide$beta)))^(dat_wide$beta/dat_wide$alpha)
dat_wide$Ek = dat_wide$ETRm/dat_wide$alpha
dat_wide$Em =(dat_wide$Pmax/dat_wide$alpha)*log((dat_wide$alpha+dat_wide$beta)/dat_wide$beta)
final.df = left_join(dat_wide, data1.long, by = 'id')