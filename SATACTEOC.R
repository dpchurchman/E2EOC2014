#sessionInfo()

# .dat file downloaded here:
#https://tea.texas.gov/Student_Testing_and_Accountability/Testing/
#State_of_Texas_Assessments_of_Academic_Readiness_(STAAR)/
#STAAR_Aggregate_Data_For_2013-2014/
# Downloaded 11/1/2018
E2_EOC_14 <-read.delim('data/EOC/cfy14ee2.dat',header=FALSE,
                       sep=',',stringsAsFactors = FALSE)
dim(E2_EOC_14)
# Downloaded on 1/21/2019 from https://tea.texas.gov/
#acctres/sat_act_index.html
SAT_ACT_16 <- read.csv('data/SATACT/sat_act_campus_data_class_2016.csv')

#str(SAT_ACT_16)
SAT_ACT_16 <- SAT_ACT_16[,c('Campus','Group','Grads_Mskd',
                            'Part_Rate','Above_Crit_Rate')]
SATACTnames <- as.character(unique(SAT_ACT_16$Group))


# Error in header of TEA files for 2014; line break in 
#header names where names go onto second line, making ncol hard
# to determine.
# Created generic helper function to fix the headers on dataframes
fixEOCheader <- function(df, split){
  # Finds the index where the names get split
  split <- min(which(is.na(df[1,])))-1
  # Combines the names from the 1st and 2nd rows at the split
  dfnames <- cbind(df[1,1:split],df[2,1:(ncol(df)-split)])
  # Sets the names of the dfs
  df <- setNames(df, dfnames)
  # Gets rid of redundant names row
  df <- df[3:nrow(df),]
  return(df)
}
# Apply function to fix header
E2_EOC_14 <- fixEOCheader(E2_EOC_14)



#Most EOC var names come in trio of 
#yes:y, no:n and missing:v; this function pastes that suffix
ynv = function(x){c(paste0(x,'y'),paste0(x,'n'),paste0(x,'v'))}
#The list of EOC vars that need suffix
EOCgroupsynv <- list('bil','voc','gif','spe','ti1','atr')
#List of all EOC vars that have SATACT corresponding variable
EOCnames <- c('all','ethb','ethi','etha','ethh',
              'ethp','ethw','eth2','ethv', 
              ynv('eco'),'sexf','sexm','sexv', 
              #sex does not use y/n but f/m
              unlist(lapply(EOCgroupsynv,ynv)),
              'lepc','MISSING','lepv', 
              #LEP strangely missing "Not LEP" as cat in EOC
              rep("MISSING",3), #No immigrant cats in EOC
              ynv('mig'))

# Create corresponding names table for SAT group names vs EOC
GroupNames <- data.frame(SATACTnames,EOCnames)
# Change from factors to character
GroupNames[] <- lapply(GroupNames, as.character)

#Add in column names for Dropout data
GroupNames$DropNames <- c('ALL','AA','NA','AS',
                          'HS','PI','WH','MU','MISSING',
                          'ECN','MISSING','MISSING',
                          'FEM','MAL','MISSING',
                          'BE','MISSING','MISSING',
                          'CTE','MISSING','MISSING',
                          'GFT','MISSING','MISSING',
                          'SPE','MISSING','MISSING',
                          'TTL','MISSING','MISSING',
                          'ATR','MISSING','MISSING',
                          'LEP','MISSING', 'MISSING',
                          'MISSING','MISSING','MISSING',
                          'MIG','MISSING','MISSING')

#There are 4 vars in SAT/ACT not in EOC, which 
#is dropped here (Not LEP and immigrant)
#Dropout data does not contain information about 
#"missing data" vs. "not of group", so will remove
GroupNames <- GroupNames[GroupNames$EOCnames != 'MISSING', ]
GroupNames <- GroupNames[GroupNames$DropNames != 'MISSING', ]




#Function to extract relevant columns by group, match SAT/ACT name
vars <- c('_d','_satis_rec_nm','_adv_rec_nm')
grouper <- function(EOCgroup='all',SATACTgroup='All Students'){
  colNames <- c('CAMPUS',paste0('e2_',EOCgroup,vars))
  df <- E2_EOC_14[,colNames]
  names(df) <- c('Campus','TestedE2','PassedE2','AdvancedE2')
  df[] <- lapply(df, as.numeric)
  df$Group <- SATACTgroup
  df <- df[,c('Group','Campus','PassedE2','AdvancedE2','TestedE2')]
  
  return(df)
}

# Loop each of the different group names into 
#the grouper function to extract relevant columns
dfList <- vector('list',dim(GroupNames)[1])
for(i in 1:length(dfList)){
  tmp <- grouper(GroupNames[i,2],GroupNames[i,1]) 
  dfList[[i]]<- tmp
}

# Stack all the dataframes to mirror SAT/ACT long format
library(data.table)
E2long <- rbindlist(dfList,use.names = TRUE)

# Merge EOC and SAT Data; left merge will drop 
#unmatching SAT rows and keep schools with EOC but no SAT/ACT
SATACTEOC <- merge(E2long, SAT_ACT_16, all.x=TRUE)
SATACTEOC$Campus <- as.factor(SATACTEOC$Campus)


# Change rates to actual rates
SATACTEOC$Part_Rate <- SATACTEOC$Part_Rate/100
SATACTEOC$Above_Crit_Rate <- SATACTEOC$Above_Crit_Rate/100

#Change grads masked to numeric
SATACTEOC$Grads_Mskd <- gsub("<",'',SATACTEOC$Grads_Mskd)
SATACTEOC$Grads_Mskd <- as.numeric(SATACTEOC$Grads_Mskd)


##########################################################
#Dropouts
##########################################################
library(readxl)
#Downloaded on 1/26/2019 from:
#https://tea.texas.gov/acctres/completion/2016/level.html
dropout16 <- read_xlsx('data/Dropout/Campus_Data_Download_4yr_2016.xlsx',
                       sheet='COMP_2016_4yr')
#Subset just those calculated for state accountability
dim(dropout16)
dropout16 <- dropout16[dropout16$CALC_FOR_STATE_ACCT =='Yes',]
dim(dropout16)

#CMP2 is graduation; continuation; GED combined

vars <- c('D','R_CMP2','R_GRAD','R_CONT','R_GED','R_DROP')
grouper2 <- function(Dropgroup='ALL',SATACTgroup='All Students'){
  columnNames <- c('CAMPUS',paste0('CAMP_',Dropgroup,vars))
  df <- dropout16[,columnNames]
  names(df) <- c('Campus','N_Drop','GradContGED_Rate','Grad_Rate',
                 'Cont_Rate', 'GED_Rate','Drop_Rate')
  #df[] <- lapply(df, as.numeric)
  df$Group <- SATACTgroup
  df <- df[,c('Group','N_Drop','Campus','GradContGED_Rate','Grad_Rate',
              'Cont_Rate','GED_Rate','Drop_Rate')]
  
  return(df)
}
# Loop each of the different group names into the grouper 
#function to extract relevant columns
dfList2 <- vector('list',dim(GroupNames)[1])
for(i in 1:length(dfList)){
  tmp <- grouper2(GroupNames[i,3],GroupNames[i,1]) 
  dfList2[[i]]<- tmp
}

droplong <- rbindlist(dfList2,use.names = TRUE)


data <- merge(SATACTEOC, droplong, all.x=TRUE)
#Will get NAs for "." which is missing data for campus
data$Grad_Rate <- as.numeric(data$Grad_Rate)/100
data$GradContGED_Rate <- as.numeric(data$GradContGED_Rate)/100
data$GED_Rate <- as.numeric(data$GED_Rate)/100
data$Drop_Rate <- as.numeric(data$Drop_Rate)/100
data$Cont_Rate <- as.numeric(data$Cont_Rate)/100

#Strip masking from N_Drop
data$N_Drop[data$N_Drop == '.'] <- '0'
data$N_Drop <- gsub("<",'',data$N_Drop)
data$N_Drop <- as.numeric(data$N_Drop)



denominator <- data[data$Group =='All Students',]
groups <- data[data$Group != 'All Students']
denominator <- denominator[,c(2,5,6,9)]
names(denominator) <- c('Campus','E2_d','SAT_d','Drop_d')

groups_d <- merge(denominator,groups, 
              by = intersect(names(denominator), names(groups)),
              all.y=TRUE)

groups_d$E2_pct_pop <- groups_d$TestedE2/groups_d$E2_d


###############################################
####Feature Creation###########################
###############################################
# Get passing and advanced rate E2
groups_d$E2Pass_Rate <- groups_d$PassedE2/groups_d$TestedE2
groups_d$E2Adv_Rate <- groups_d$AdvancedE2/groups_d$TestedE2
#Change to actual rates
groups_d$GradContGED_Rate <- groups_d$GradContGED_Rate/100 
groups_d$GED_Rate <- groups_d$GED_Rate/100
groups_d$Grad_Rate <- groups_d$Grad_Rate/100
groups_d$Cont_Rate <- groups_d$Cont_Rate/100
groups_d$Drop_Rate <- groups_d$Drop_Rate/100
str(data)
#rates <- data[,c(1,2,7,8,10,11,12,13,14,15,16)]
counts <- data[,c(1,2,5,6,9)]
names(counts) <- c('Group','Campus','E2_EOC','SAT_ACT',
                   'Dropout_Report')
library(tidyr)
countslong<-gather(counts,Criteria,Number,E2_EOC:Dropout_Report)
# Group American Indian, Asian, Multiracial and Pacific islander into
#Other race/ethnicity
# otherethncounts <- countslong[countslong$Group %in% 
#                                 c('American Indian',
#                                 'Asian','Multiracial',
#                                 'Pacific Islander'),]
# 
# OtherEthn <- aggregate(Number~Campus+Criteria,data=otherethncounts, 
#           FUN = sum)
# OtherEthn$Group <- 'Other Ethnicity/Race'
# OtherEthn <- OtherEthn[,c('Group','Campus','Criteria','Number')]
# countslong <- rbind(countslong,OtherEthn)
# countslong <- countslong[!(countslong$Group  %in% 
#                            c('American Indian',
#                              'Asian','Multiracial',
#                              'Pacific Islander')),]





###############################################
####Visualization##############################
###############################################
library(ggplot2)


# Compare E2 2014 number of students to SAT/ACT class of 2016 and 
# denominator of dropout report
g1 <- ggplot(denominator, aes(x=E2_d,y=SAT_d))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_point(alpha=.4, color=ifelse((denominator$E2_d > 3*denominator$SAT_d |
                                      3*denominator$E2_d < denominator$SAT_d) &
                                      (denominator$E2_d >250 | denominator$SAT_d >250),
                                    'red','black'))+
  geom_line(aes(y=E2_d),color='blue')+
  labs(x='2014 English II EOC',y='Class of 2016 SAT/ACT')


g2 <- ggplot(denominator, aes(x=E2_d,y=Drop_d))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_point(alpha=.4, color=ifelse((denominator$Drop_d > 3*denominator$E2_d |
                                       3*denominator$E2_d < denominator$SAT_d) &
                                      (denominator$E2_d >250 | denominator$SAT_d >250) ,
                                    'red','black'))+
  geom_line(aes(y=E2_d),color='blue')+
  labs(x='2014 English II EOC',y='Class of 2016 Graduating Class')


g3 <- ggplot(denominator, aes(x=Drop_d,y=SAT_d))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_point(alpha=.4, color=ifelse((denominator$Drop_d > 3*denominator$SAT_d |
                                      3*denominator$Drop_d < denominator$SAT_d) &
               (denominator$Drop_d >250 | denominator$SAT_d >250) ,
                                    'red','black'))+
  geom_line(aes(x=E2_d,y=E2_d),color='blue')+
  labs(x='Class of 2016 Graduating Class',y='Class of 2016 SAT/ACT')

library(gridExtra)
g4 <- grid.arrange(g1, g3, g2, ncol=2,nrow=2)
g4
ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/Size.png',g4)


g1outliers <- denominator$Campus[which((denominator$E2_d > 3*denominator$SAT_d |
  3*denominator$E2_d < denominator$SAT_d) &
  (denominator$E2_d >250 | denominator$SAT_d >250))]

g2outliers <- denominator$Campus[which((denominator$Drop_d > 3*denominator$E2_d |
                                          3*denominator$E2_d < denominator$SAT_d) &
                                         (denominator$E2_d >250 | denominator$SAT_d >250))]
g2outliers %in% g1outliers

outliers <- g1outliers
length(outliers)

groups_d <- groups_d[ !groups_d$Campus %in% outliers,]

#length(unique(groups_d$Campus))
#head(groups_d)
pct_pop_ethn <- groups_d[groups_d$Group %in%
                           c('White','Asian','African American',
                              'Hispanic','Multiracial',
                              'Pacific Islander','American Indian'),
                         c('Campus','Group',
                       'E2_pct_pop','TestedE2')]


g1 <- ggplot(pct_pop_ethn,aes(x=Group,y=E2_pct_pop))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_boxplot(alpha=.4)+
  labs(x=NULL,y=NULL)

pct_pop_imm <- groups_d[groups_d$Group %in%
                          c('Bil/ESL','LEP/ELL','Migrant',
                            'Male','Female'),
                        c('Campus','Group',
                          'E2_pct_pop','TestedE2')]

g2 <- ggplot(pct_pop_imm,aes(x=Group,y=E2_pct_pop))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_boxplot(alpha=.4)+
  labs(x=NULL,y=NULL)

pct_pop_other <- groups_d[groups_d$Group %in%
                           c("At Risk",'CTE','Title1',
                             'Economically Disadvantaged',
                             'Special Ed','Gifted'),
                         c('Campus','Group',
                           'E2_pct_pop','TestedE2')]

g3 <- ggplot(pct_pop_other,aes(x=Group,y=E2_pct_pop))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_boxplot(alpha=.4)+
  labs(x=NULL,y=NULL)

g4 <- grid.arrange(g1,g2,g3,nrow=3,ncol=1)

ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/EthnPlot.png',g4)

pct_pop_riskeco <- groups_d[groups_d$Group %in%
                        c("At Risk",
                          'Economically Disadvantaged'),
                      c('Campus','Group','TestedE2')]
library(tidyr)
risk_wide <- pct_pop_riskeco%>%spread(Group, TestedE2)

g <- ggplot(risk_wide,aes(x=`At Risk`,
                          y=`Economically Disadvantaged`))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_line(aes(x=`At Risk`,y=`At Risk`),color='blue')+
  geom_point(alpha=.4)+
  labs(x='At Risk',y='Economically Disadvantaged')
g
ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/AtRiskPlot.png',g)
#cor(risk_wide$`At Risk`,risk_wide$`Economically Disadvantaged`)

library(GGally)

E2_d_groups <- groups_d[,c('Campus','Group','TestedE2')]


other_ethns <- c('Pacific Islander','Multiracial','Asian','American Indian')

create_other_ethn <- function(column,df=data){
  cols <- c('Campus','Group',column)
  df_groups <- df[,names(df) %in% cols,with=FALSE]
  df_other <- df_groups[df_groups$Group %in% other_ethns,]
  df_other <- aggregate(as.formula(paste0(column,"~Campus")), data=df_groups,FUN=sum)
  df_other$Group = 'Other Race Ethn'
  df_other <- df_other[,c(1,3,2)]
  df_groups <- rbind(df_groups[!df_groups$Group %in% other_ethns,],df_other)
  return(df_groups)
}

E2_d_groups <- create_other_ethn('TestedE2', groups_d)

library(tidyr)
E2_d_wide <- E2_d_groups%>%spread(Group, TestedE2)
E2_d_wide_values <- E2_d_wide[,-1]
#ggpairs(E2_d_wide_values) 

#Create "Other ethnicity" dataframe
vars <- names(data)[-c(1:2)]

data_other <- create_other_ethn(names(data)[3],data)
for(v in vars[2:length(vars)]){
  df = create_other_ethn(v,data)
  data_other <- merge(data_other, df, all =TRUE)
}

#data_other <- data
#Male is complement of female; Bil.ESL has many fewer values than LEP.ELL
data_trim <- data_other[!data_other$Group %in% c('Male','Bil.ESL'),]

data_all <- data_trim[data_trim$Group =='All Students',]
data_all <- data_all[,-c(1)]

#Source: https://stackoverflow.com/questions/5468280/scale-a-series-between-two-points
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
data_all$School_Size  <- range01(data_all$TestedE2)

data_subs <- data_trim[data_trim$Group !='All Students',]

data_subs_tested <- data_subs[,c(2,1,5)]
number_subs_wide <- data_subs_tested %>% spread(Group,TestedE2)

data_all <- merge(data_all,number_subs_wide,all.x=TRUE)

group_names <- c('Female',
                 'White','Hispanic','African American','Other Race Ethn',
                 'Migrant','LEP/ELL',
                 'Gifted','Special Ed','CTE',
                 'Economically Disadvantaged','At Risk','Title1')

data_all <- as.data.frame(data_all)

data_all[,group_names] <- 
  data_all[,group_names]/data_all$TestedE2


data_all$Rate_Passed_E2 <- data_all$PassedE2 / data_all$TestedE2 
#data_all_E2 <- data_all[,-c(2:13)]

matrix_data <- data_all[,c('School_Size',group_names,'Rate_Passed_E2','Grad_Rate',
                              'Part_Rate','Above_Crit_Rate')]
corr_matrx <- ggcorr(matrix_data,nbreaks=11,palette = "RdGy",hjust=.2,#label=TRUE,
       method = c("complete","spearman"))
corr_matrx1
ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/PopsCorr1.png', corr_matrx)


ggcorr(matrix_data,nbreaks=7,palette = "RdGy",hjust=.5,layout.exp = 1,#label=TRUE,
       method = c("complete","spearman"),legend.position=c(0.1,.667),name='Spearman Correlation')

g <- ggplot(matrix_data,aes(x=`Rate_Passed_E2`,
                          y=`Above_Crit_Rate`))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_line(aes(x=`At Risk`,y=`At Risk`),color='blue')+
  geom_point(alpha=.4)+
  labs(x='Passed English II EOC',y='Above Critical Rate on SAT/ACT')
g
ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/E2vsACT.png',g)
head(data_all)
g <- ggplot(data_all,aes(x=`Rate_Passed_E2`,
                            y=`Grad_Rate`))+
  theme_bw()+
  theme(text=element_text(family='serif'))+
  geom_line(aes(x=`At Risk`,y=`At Risk`),color='blue')+
  geom_point(alpha=.4,size=data_all$School_Size*5+2)+
  stat_smooth(method='lm',color='red')+
  ylim(0,1)+
  labs(x='Passed English II EOC',y='Graduation Rate')
g
ggsave('C:/Users/dpchu/OneDrive/SMU/Capstone/E2vsGrad.png',g)

names(data_all)

