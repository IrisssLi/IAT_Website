# 加载必要的库
packages <- c("data.table", "dplyr", "ggplot2", "ggpubr", "mousetrap", "tidyr")
newpackages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
lapply(packages, require, character.only = TRUE)

args <- commandArgs(trailingOnly = TRUE)
iat4_dir <- args[1]
iat7_dir <- args[2]
output_dir <- args[3]

# 读取数据
iat4 <- read_bulk(iat4_dir, fun=read_mt, extension=".mt")
iat7 <- read_bulk(iat7_dir, fun=read_mt, extension=".mt")

# preprocesing
iat4 <- mt_import_wide(iat4)
mt_4 <- mt_remap_symmetric(iat4)
mt_4 <- mt_align_start_end(mt_4)
mt_4 <- mt_time_normalize(mt_4)
mt_4 <- mt_measures(mt_4)
mt_4 <- mt_angles(mt_4,use='tn_trajectories')
mt_4 <- mt_derivatives(mt_4,use='tn_trajectories')
iat1_4=cbind(mt_4$data,mt_4$measures,mt_4$tn_trajectories)
iat1_4$group='seq'
iat1_4$run='congruent'

iat7 <- mt_import_wide(iat7)
mt_7 <- mt_remap_symmetric(iat7)
mt_7 <- mt_align_start_end(mt_7)
mt_7 <- mt_time_normalize(mt_7)
mt_7 <- mt_measures(mt_7)
mt_7 <- mt_angles(mt_7,use='tn_trajectories')
mt_7 <- mt_derivatives(mt_7,use='tn_trajectories')
iat1_7=cbind(mt_7$data,mt_7$measures,mt_7$tn_trajectories)
iat1_7$group='seq'
iat1_7$run='incongruent'

# merge and save all
alldata=rbind(iat1_4,iat1_7)
#write.csv(alldata,'C:/Users/liliz/Desktop/IATwebsite/data/merged_data.csv')

# check for every subject: revise manually
alldata=alldata[,!duplicated(colnames(alldata))]

# filter: error trials +600ms, trials RT<300ms, subj with >10% trials RT>10s

data=alldata[alldata$RT<10000,]
data[data$error==1,]$RT=data[data$error==1,]$RT+600
data$event='zero'
for (i in c(1:length(data$order))){
  if ((data[i,'condition']=='real')|(data[i,'condition']=='unreal')){
    data[i,'event']=data[i,'condition']
  }
  if (grepl('ev1',data[i,'condition'],  fixed = TRUE)){
    data[i,'event']='CR'
  }
  if (grepl('ev2',data[i,'condition'],  fixed = TRUE)){
    data[i,'event']='CI'
  }
}

data=data[data$error==0,]

# IATscore
mean=data %>% group_by(subjID,run) %>% summarise(RT = mean(RT),)
std=data %>% group_by(subjID) %>% summarise(RT = sd(RT),)
IATscore=data.frame((mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT)
colnames(IATscore)=c('IATscore')
IATscore$subjID=unique(data$subjID)


# MT indices
MTdata=data[data$error==0,]
mean=MTdata %>% group_by(subjID,run) %>% dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))
std=MTdata %>% group_by(subjID) %>% summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
MTscore=std
MTscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
MTscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
MTscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
MTscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC



# bar plot total IAT effect
MTscore$RT=IATscore$IATscore
colnames(MTscore)[colnames(MTscore)=='MD_above']='MD'
MTscore_long=tidyr::pivot_longer(MTscore,cols = c("RT","MAD","AD","MD","AUC"))

agg <- MTscore_long %>% 
  group_by(name) %>% 
  dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se = sd / sqrt(n)) 


f = ggplot(data = agg, aes(x = name, y=mean)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8,fill='#d6e3f8')  + 
  geom_point(data = MTscore_long, aes(x = name, y=value),size=3,position = position_jitter(seed = 1, width = 0),color='#004ba875') +
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mean-se, ymax=mean+se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='metrics') + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='IAT effect') +
  coord_cartesian(ylim = c(0,0.5))+
  geom_hline(yintercept=0, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
#ggsave(file=paste0('C:/Users/liliz/Desktop/IATwebsite/data/IATeffect_merge.svg'), plot=f, width=6, height=4)



# organize for bar plot for CR &CI
MTdata=data[data$error==0,]
mean=MTdata[(MTdata$event=='CR')|(MTdata$event=='real'),] %>% group_by(subjID,run) %>% dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))

std=MTdata[(MTdata$event=='CR')|(MTdata$event=='real'),] %>% group_by(subjID) %>% dplyr::summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
CRscore=std
CRscore$RT=(mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT
CRscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
CRscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
CRscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
CRscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC
CRscore$event='real'

mean=MTdata[(MTdata$event=='CI')|(MTdata$event=='unreal'),] %>% group_by(subjID,run) %>%dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))

std=MTdata[(MTdata$event=='CI')|(MTdata$event=='unreal'),] %>% group_by(subjID) %>% dplyr::summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
CIscore=std
CIscore$RT=(mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT
CIscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
CIscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
CIscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
CIscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC
CIscore$event='unreal'

allscore=rbind(CRscore,CIscore)
colnames(allscore)[colnames(allscore)=='MD_above']='MD'
allscore_long=tidyr::pivot_longer(allscore,cols = c("RT","MAD","AD","MD","AUC"))


# bar plot for CI & CR

ev_colors=c('#ffbf00','#9ccc00')
ev_colors=c('#41337A','#41337A50')
ev_colors=c('#004ba8','#004ba850')

agg <- allscore_long %>% 
  group_by(event,name) %>% 
  dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se = sd / sqrt(n)) 


f = ggplot(data = agg, aes(x = name, y=mean,fill=event)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=ev_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mean-se, ymax=mean+se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='metrics') + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='IAT effect') +
  coord_cartesian(ylim = c(0,0.4))+
  geom_hline(yintercept=0, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position='none',
        legend.text = element_text(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
#ggsave(file=paste0('C:/Users/liliz/Desktop/IATwebsite/data/IATeffect.svg'), plot=f, width=6, height=4)

# 保存结果
write.csv(alldata, file.path(output_dir, 'merged_data.csv'))
ggsave(file.path(output_dir, 'IATeffect_merge.svg'), plot=f1, width=6, height=4)
ggsave(file.path(output_dir, 'IATeffect.svg'), plot=f2, width=6, height=4)

