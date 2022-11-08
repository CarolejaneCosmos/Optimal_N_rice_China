library(mapproj);library(plyr);library(maps);library(maptools)
library(ggplot2);library(ggpubr);library(reshape2);library(cowplot)
library(gdata);library(piecewiseSEM);library(nlme);library(dplyr)
library(ape);library(lavaan);library(caper);library(boot)
library(MASS);library(MuMIn);library(ggsci);library(ggsignif);library(agricolae);library(showtext);library(officer);library(rvg);library(here);library(glue)
setwd() #choose your working directory

windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             RMN=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial")) #Change Font types
font_add('Arial','/Library/Fonts/Arial.ttf') #add Arial font to the library
#showtext_auto()

#Fig. 2
#Yield Nfer Nr map
Yieldproduct<-read.xls("Data_Fig2.xlsx",perl = "c:/perl64/bin/perl.exe",fileEncoding="utf8")
Yieldproduct$Nstrategy<-factor(Yieldproduct$Nstrategy,levels = c("FN","ON","EON"),ordered = TRUE)
Yieldproduct$Sub.1<-factor(Yieldproduct$Sub.1,levels = c("East China","Central China","Northeast","Northwest","South China","Southwest","Early rice","Late rice"),ordered = TRUE)
Yieldproduct<-Yieldproduct[1:24,]

Fig2_Nrate<-ggplot(Yieldproduct,aes(Nstrategy,EONR))+
  scale_fill_npg()+
  scale_color_npg()+
  geom_bar(stat="identity",aes(fill=Nstrategy),width=.5,alpha=.8)+
  geom_errorbar(aes(x=Nstrategy,ymin=EONR_l,ymax=EONR_h,col=Nstrategy),size=1,width=.3,position=position_dodge(1))+
  facet_wrap(vars(Sub.1),nrow=2,drop = TRUE,scales = "free")+
  xlab(expression("N application strategies"))+
  ylab(expression("N rate (Kg N"~~ha^-1~")"))+
  geom_vline(xintercept = 1.5,linetype=2,col="grey")+
  geom_vline(xintercept = 2.5,linetype=2,col="grey")+
  scale_y_continuous(limits = c(0,450),expand=c(0,0))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(legend.position ="none",legend.title=element_blank(),legend.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())

Fig2_dml <- rvg::dml(ggobj = Fig2_Nrate)#Convert ggobj to class dml 
# initialize PowerPoint slide
officer::read_pptx() %>%
officer::add_slide() %>%
  # specify object and location of object
officer::ph_with(Fig1_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
base::print(
  target = here::here(
    "Fig2_Nrate.pptx"
  )
)
#ggsave("Nrate_map.pdf",width=7,heigh=4.5)
#Yield

Fig2_Y<-ggplot(Yieldproduct,aes(Nstrategy,Yield))+
  scale_fill_npg()+
  scale_color_npg()+
  geom_bar(stat="identity",aes(fill=Nstrategy),width=.5)+
  geom_errorbar(aes(x=Nstrategy,ymin=YieldL,ymax=YieldH,col=Nstrategy),size=1,width=.3,position=position_dodge(1))+
  facet_wrap(vars(Sub.1),nrow=2,drop = TRUE,scales = "free")+
  xlab(expression("N application strategies"))+
  ylab(expression("Rice yield (Mg"~~ha^-1~")"))+
  geom_vline(xintercept = 1.5,linetype=2,col="grey")+
  geom_vline(xintercept = 2.5,linetype=2,col="grey")+
  scale_y_continuous(limits = c(0,10),expand=c(0,0))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(legend.position ="none",legend.title=element_blank(),legend.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())

Fig2_dml <- rvg::dml(ggobj = Fig2_Y)#Convert ggobj to class dml 
# initialize PowerPoint slide
officer::read_pptx() %>%
  officer::add_slide() %>%
  # specify object and location of object
  officer::ph_with(Fig1_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig2_Yield.pptx"))

#Nr losses

Fig2_Nr<-ggplot(Yieldproduct,aes(Nstrategy,Nr_losses))+
  scale_fill_npg()+
  scale_color_npg()+
  geom_bar(stat="identity",aes(fill=Nstrategy),width=.5)+
  geom_errorbar(aes(x=Nstrategy,ymin=Nr_losses_lower,ymax=Nr_losses_higher,col=Nstrategy),size=1,width=.3,position=position_dodge(1))+
  facet_wrap(vars(Sub.1),nrow=2,drop = TRUE,scales = "free")+
  xlab(expression("N application strategies"))+
  ylab(expression("Nr losses (kg"~~ha^-1~")"))+
  geom_vline(xintercept = 1.5,linetype=2,col="grey")+
  geom_vline(xintercept = 2.5,linetype=2,col="grey")+
  scale_y_continuous(limits = c(0,110),expand=c(0,0))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(legend.position ="none",legend.title=element_blank(),legend.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())

Fig2_dml <- rvg::dml(ggobj = Fig2_Nr)#Convert ggobj to class dml 
# initialize PowerPoint slide
officer::read_pptx() %>%
  officer::add_slide() %>%
  # specify object and location of object
  officer::ph_with(Fig1_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig2_Nr.pptx"))

#Fig. 3
#Rice production
FerYNr<-read.xls("Data_Fig3.xlsx",perl="c:/perl64/bin/perl.exe",fileEncoding="utf8")
FerYNr_1<-FerYNr[-c(1:9),]
Chinafer<-subset(FerYNr,Continent=="China"&Index=="Fertilizer consumption")
RiceYield<-subset(FerYNr,Continent=="China"&Index=="Yield")
ChinaNr<-subset(FerYNr,Continent=="China"&Index=="Reactive N losses")
RiceYield$Continent<-factor(RiceYield$Continent,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
RiceYield$Nstrategy<-factor(RiceYield$Nstrategy,levels = c("FN","ON","EON"),ordered = TRUE)
Chinafer$Nstrategy<-factor(Chinafer$Nstrategy,levels = c("FN","ON","EON"),ordered = TRUE)
ChinaNr$Nstrategy<-factor(ChinaNr$Nstrategy,levels = c("FN","ON","EON"),ordered = TRUE)
RiceYieldopt<-subset(RiceYield,Nstrategy=="ON"|Nstrategy=="EON")
RiceYieldFN<-subset(RiceYield,Nstrategy=="FN")

#Rice production
RiceYield_1<-subset(FerYNr,Index=="Yield")
RiceYield_1<-RiceYield_1[-c(1:3),]
RiceYield_1$Continent<-factor(RiceYield_1$Continent,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
RiceYield_1$Nstrategy<-factor(RiceYield_1$Nstrategy,levels = c("EON","ON","FN"),ordered = TRUE)
Fig3_Rice_production<-ggplot(RiceYield_1,aes(Value,Nstrategy))+
  geom_bar(aes(fill = Continent),stat = "identity",width = .3,alpha=.9,position = "stack")+
  geom_vline(xintercept = 218, col="darkred", linetype=1, size=1)+
  ylab(expression("Rice production (Mt )"))+
  scale_x_continuous(limits = c(0,235),expand=c(0,0))+
  geom_errorbar(data=RiceYieldopt,aes(y=Nstrategy,xmin=Value,xmax=Higherbound),alpha=.9,col="black",size=.5,width=.1,position=position_dodge(2))+
  geom_errorbar(data=RiceYieldFN,aes(y=Nstrategy,xmin=Higherbound ,xmax=Value),col="black",alpha=.9,size=.5,width=.1,position=position_dodge(2))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_jco()+
  scale_color_jco()+
  theme(legend.position ="none",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank()) 

Fig3_dml <- rvg::dml(ggobj = Fig3_Rice_production)#Convert ggobj to class dml 
# initialize PowerPoint slide
officer::read_pptx() %>%
  officer::add_slide() %>%
  # specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig3_Y.pptx"))
#Fertilizer N
Chinafer_1<-subset(FerYNr,Index=="Fertilizer consumption")
Chinafer_1<-Chinafer_1[-c(1:3),]
Chinafer_1$Continent<-factor(Chinafer_1$Continent,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
Chinafer_1$Nstrategy<-factor(Chinafer_1$Nstrategy,levels = c("EON","ON","FN"),ordered = TRUE)
Fig3_Fertilizer<-ggplot(Chinafer_1,aes(Value,Nstrategy))+
  scale_y_discrete()+
  annotate("rect",xmin=3.90,xmax=5.06,ymin=0.4,ymax=3.6,fill="light gray",col="transparent",alpha=.5)+
  geom_bar(aes(fill = Continent),stat = "identity",width = .3,alpha=.9,position = "stack")+
  geom_vline(xintercept = 4.48, col="darkred", linetype=1, size=1)+
  ylab(expression("N consumption (Mt )"))+
  scale_x_continuous(limits = c(0,8.6),expand=c(0,0))+
  geom_errorbar(data=Chinafer,aes(y=Nstrategy,xmin=Lowerbound ,xmax=Higherbound),alpha=.9,col="black",size=.5,width=.1,position=position_dodge(2))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_jco()+
  scale_color_jco()+
  theme(legend.position ="top",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank()) 
Fig3_dml <- rvg::dml(ggobj = Fig3_Fertilizer)#Convert ggobj to class dml 
# initialize PowerPoint slide
officer::read_pptx() %>%
  officer::add_slide() %>%
  # specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig3_F.pptx"))
#Nrloss
ChinaNr_1<-subset(FerYNr,Index=="Reactive N losses")
ChinaNr_1<-ChinaNr_1[-c(1:3),]
ChinaNr_1$Continent<-factor(RiceYield_1$Continent,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
ChinaNr_1$Nstrategy<-factor(RiceYield_1$Nstrategy,levels = c("EON","ON","FN"),ordered = TRUE)
Fig3_Nloss<-ggplot(ChinaNr_1,aes(Value,Nstrategy))+
  scale_y_discrete()+
  annotate("rect",xmin=0.73,xmax=1.35,ymin=0.4,ymax=3.6,fill="light gray",col="transparent",alpha=.5)+
  geom_bar(aes(fill = Continent),stat = "identity",width = .3,alpha=.9,position = "stack")+
  geom_vline(xintercept = 1.01, col="darkred", linetype=1, size=1)+
  ylab(expression("Total Nr losses(Mt)"))+
  scale_x_continuous(limits = c(0,2.3),expand=c(0,0))+
  geom_errorbar(data=ChinaNr,aes(y=Nstrategy,xmin=Lowerbound ,xmax=Higherbound),alpha=.9,col="black",size=.5,width=.1,position=position_dodge(2))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_jco()+
  scale_color_jco()+
  theme(legend.position ="none",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank()) 

Fig3_dml <- rvg::dml(ggobj = Fig3_Nloss)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig3_Nr.pptx"))

#Fig 3 b
library(dplyr)
library(stringr)
Nrlevel<-read.xls("Nr_threshold.xlsx",perl="c:/perl64/bin/perl.exe",fileEncoding="utf8",sheet = 5)
FN_level<-Nrlevel[c(1:24),c(1,5,7,8,18:20)]
FN_level_NH3<-subset(FN_level,Index == "NH3")
FN_level_water<-subset(FN_level,Index == "Leaching$runoff")
FN_level_N2O<-subset(FN_level, Index == "N2O")
FN_level_NH3$Region<-factor(FN_level_NH3$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
FN_level_N2O$Region<-factor(FN_level_N2O$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
FN_level_water$Region<-factor(FN_level_water$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)

ON_level<-Nrlevel[c(1:24),c(1,5,7,8,21:23)]
ON_level_NH3<-subset(ON_level,Index == "NH3")
ON_level_water<-subset(ON_level,Index == "Leaching$runoff")
ON_level_N2O<-subset(ON_level, Index == "N2O")
ON_level_NH3$Region<-factor(ON_level_NH3$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
ON_level_N2O$Region<-factor(ON_level_N2O$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
ON_level_water$Region<-factor(ON_level_water$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)

EON_level<-Nrlevel[c(1:24),c(1,5,7,8,24:26)]
EON_level_NH3<-subset(EON_level,Index == "NH3")
EON_level_water<-subset(EON_level,Index == "Leaching$runoff")
EON_level_N2O<-subset(EON_level, Index == "N2O")
EON_level_NH3$Region<-factor(EON_level_NH3$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
EON_level_N2O$Region<-factor(EON_level_N2O$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)
EON_level_water$Region<-factor(EON_level_water$Region,levels = c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ordered = TRUE)

#NH3
Fig3_NH3<-ggplot(FN_level_NH3)+
  scale_x_discrete(labels = function(x) str_wrap(x,5))+  annotate("rect",xmin=0,xmax=8.5,ymin=0.53,ymax=1.30,fill="lightpink",col="transparent",alpha=.5)+
  geom_hline(aes(yintercept = y),data.frame(y = c(0:4)),color = "lightgrey")+
  geom_segment(aes(x = Region,y=0,xend =Region,yend = 4),linetype = "dashed",color = "lightgrey")+
  geom_col(data = FN_level_NH3,aes(x = Region,y = Current_FN_percent,fill = "FN"),position = "dodge2",width = .7)+
  geom_errorbar(data = FN_level_NH3,aes(x = Region,ymax = Current_FN_high_percent,ymin = Current_FN_low_percent,col="FN"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = ON_level_NH3,aes(x = Region,y = Current_ON_percent,fill = "ON"),position = "dodge2",width = .7)+
  geom_errorbar(data = ON_level_NH3,aes(x = Region,ymax = Current_ON_high_percent,ymin = Current_ON_low_percent,col="ON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = EON_level_NH3,aes(x = Region,y = Current_EON_percent,fill = "EON"),position = "dodge2",width = .7)+
  geom_errorbar(data = EON_level_NH3,aes(x = Region,ymax = Current_EON_high_percent,ymin = Current_EON_low_percent,col="EON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_hline(aes (yintercept = 1),color = "darkred", linetype=1, size=1)+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_npg(limits=c("FN", "ON", "EON"))+
  scale_color_npg(limits=c("FN", "ON", "EON"))+
  scale_y_continuous(limits = c(-1.5, 4.5),expand = c(0, 0),breaks = c(0, 1, 2, 3)) + 
  theme(legend.position ="top",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank(),panel.border = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +
  annotate(x = 0.1, y = 1.2, label = "100%", geom = "text", color = "black") +
  annotate( x = 0.1, y = 2.2, label = "200%",  geom = "text", color = "black") +
  annotate(x = 0.1, y = 3.2, label = "300%", geom = "text", color = "black") +
  annotate( x = 0.1,y = 4.2, label = "400%",  geom = "text",  color = "black") +
  coord_polar()

Fig3_dml <- rvg::dml(ggobj = Fig3_NH3)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(
    target = here::here(
      "Fig3_NH3.pptx"))

#Water
Fig3_water<-ggplot(FN_level_water)+
  scale_x_discrete(labels = function(x) str_wrap(x,4))+
  annotate("rect",xmin=0,xmax=8.7,ymin=0.77,ymax=1.33,fill="lightpink",col="transparent",alpha=.5)+
  geom_segment(aes(x = Region,y=0,xend =Region,yend = 2),linetype = "dashed",color = "lightgrey")+
  geom_hline(aes(yintercept = y),data.frame(y = c(0:2)),color = "lightgrey")+
  geom_col(data = FN_level_water,aes(x = Region,y = Current_FN_percent,fill = "FN"),position = "dodge2",width = .7)+
  geom_errorbar(data = FN_level_water,aes(x = Region,ymax = Current_FN_high_percent,ymin = Current_FN_low_percent,col="FN"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = ON_level_water,aes(x = Region,y = Current_ON_percent,fill = "ON"),position = "dodge2",width = .7)+
  geom_errorbar(data = ON_level_water,aes(x = Region,ymax = Current_ON_high_percent,ymin = Current_ON_low_percent,col="ON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = EON_level_water,aes(x = Region,y = Current_EON_percent,fill = "EON"),position = "dodge2",width = .7)+
  geom_errorbar(data = EON_level_water,aes(x = Region,ymax = Current_EON_high_percent,ymin = Current_EON_low_percent,col="EON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_hline(aes (yintercept = 1),color = "darkred", linetype=1, size=1)+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_npg(limits=c("FN", "ON", "EON"))+
  scale_color_npg(limits=c("FN", "ON", "EON"))+
  scale_y_continuous(limits = c(-1.5, 2.5),expand = c(0, 0),breaks = c(0, 1, 2)) + 
  theme(legend.position ="none",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank(),panel.border = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),axis.text.x = element_text(size = 12)) +
  annotate(x = 8.7, y = 1.2, label = "100%", geom = "text", color = "gray12") +
  annotate( x = 8.7, y = 2.2, label = "200%",  geom = "text", color = "gray12") +
  coord_polar()

Fig3_dml <- rvg::dml(ggobj = Fig3_water)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 9, height = 4.95, left = 0.5, top = 1)) %>%
  base::print(target = here::here("Fig3_Water.pptx"))

#N2O
Fig3_N2O<-ggplot(FN_level_N2O)+
  scale_x_discrete(labels = function(x) str_wrap(x,4))+
  annotate("rect",xmin=0,xmax=8.7,ymin=0.50,ymax=2.34,fill="lightpink",col="transparent",alpha=.5)+
  geom_segment(aes(x = Region,y=0,xend =Region,yend = 3),linetype = "dashed",color = "lightgrey")+
  geom_hline(aes(yintercept = y),data.frame(y = c(0:3)),color = "lightgrey")+
  geom_col(data = FN_level_N2O,aes(x = Region,y = Current_FN_percent,fill = "FN"),position = "dodge2",width = .7)+
  geom_errorbar(data = FN_level_N2O,aes(x = Region,ymax = Current_FN_high_percent,ymin = Current_FN_low_percent,col="FN"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = ON_level_N2O,aes(x = Region,y = Current_ON_percent,fill = "ON"),position = "dodge2",width = .7)+
  geom_errorbar(data = ON_level_N2O,aes(x = Region,ymax = Current_ON_high_percent,ymin = Current_ON_low_percent,col="ON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_col(data = EON_level_N2O,aes(x = Region,y = Current_EON_percent,fill = "EON"),position = "dodge2",width = .7)+
  geom_errorbar(data = EON_level_N2O,aes(x = Region,ymax = Current_EON_high_percent,ymin = Current_EON_low_percent,col="EON"),show.legend = FALSE,size=1,width=.1,position="dodge2")+
  geom_hline(aes (yintercept = 1),color = "darkred", linetype=1, size=1)+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank(),axis.title.x =element_blank() )+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_npg(limits=c("FN", "ON", "EON"))+
  scale_color_npg(limits=c("FN", "ON", "EON"))+
  scale_y_continuous(limits = c(-1.5, 3.5),expand = c(0, 0),breaks = c(0, 1, 2, 3)) + 
  theme(legend.position ="none",legend.title=element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.background = element_blank(),panel.background = element_blank(),panel.border = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),axis.text.x = element_text(size = 12)) +
  annotate(x = 8.7, y = 1.2, label = "100%", geom = "text", color = "gray12") +
  annotate( x = 8.7, y = 2.2, label = "200%",  geom = "text", color = "gray12") +
  annotate(x = 8.7, y = 3.2, label = "300%", geom = "text", color = "gray12") +
  coord_polar()

Fig3_dml <- rvg::dml(ggobj = Fig3_N2O)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig3_dml, ph_location(width = 4.87, height = 3.86, left = 0.5, top = 0.5)) %>%
  base::print(target = here::here("Fig3_N2O.pptx"))

#Fig. 4
Diffplot<-read.csv("Data_Fig4.csv")
Diffplot<-as.data.frame(Diffplot)
Diffplot$Nstrategy<-factor(Diffplot$Nstrategy,levels=c("ON","EON"),ordered = TRUE)
Diffplot$Difftype<-as.factor(Diffplot$Difftype)
Diffplot$percent<-factor(Diffplot$percent,levels = c("≤-5","(-5,0]","(0,5]",">5"),ordered = TRUE)
Diffplot$Sub.1<-as.factor(Diffplot$Sub.1)
Diffplot <- within(Diffplot, Difftype <- factor(Difftype, levels = c("Yield","Economic benefit","NEEB")))
with(Diffplot,levels(Difftype))
Diffplot <- within(Diffplot, Sub.1 <- factor(Sub.1, levels =  c("East China","Central China","Northeast","South China","Southwest","Early rice","Late rice")))
with(Diffplot,levels(Sub.1))

Fig_4<-ggplot(Diffplot,aes(Ndif,percent))+
  scale_y_discrete()+
  annotate("rect",xmin=0,xmax=0.99,ymin=2.5,ymax=4.55,fill="light gray",col="transparent",alpha=.5)+
  geom_hline(yintercept = 1.5,color="grey",alpha=.9,linetype=2)+
  geom_hline(yintercept = 3.5,color="grey",alpha=.9,linetype=2)+
  stat_summary(fun="mean",aes(fill=Nstrategy),geom="bar",size=.5,position="dodge",alpha = .8)+
  guides(col=FALSE)+
  geom_errorbarh(aes(y=percent,xmin=Lowerbound,xmax=Upperbound,col=Nstrategy),size=0.4,height=.3,position=position_dodge(1))+
  geom_errorbarh(aes(y=percent,xmin=Upperbound,xmax=Lowerbound,col=Nstrategy),size=0.4,height=.3,position=position_dodge(1))+
  facet_grid(Difftype~Sub.1,drop = TRUE)+
  scale_x_continuous(limits = c(0,0.99),expand=c(0,0),labels=scales::percent)+
  xlab(expression("Frequency (%)"))+
  ylab(expression("Difference relative to Farmer practice N (%)"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  scale_fill_npg()+
  scale_color_npg()+
  theme(legend.position =c(0.05,0),legend.justification = c(0.05,0),legend.title=element_blank(),legend.background = element_blank())

Fig4_dml <- rvg::dml(ggobj = Fig_4)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig4_dml, ph_location(width = 7.2, height = 4.4, left = 0.5, top = 0.5)) %>%
  base::print(target = here::here("Fig4.pptx"))

#Fig 5
Diffplot<-read.csv("Data_Fig5.csv")
Diffplot<-as.data.frame(Diffplot)
Diffplot$Ndif<-as.numeric(Diffplot$Ndif)
Diffplot$Ndifh<-as.numeric(Diffplot$Ndifh)
Diffplot$Ndifl<-as.numeric(Diffplot$Ndifl)
Diffplot$Sub.1<-factor(Diffplot$Sub.1,levels = c("East China","Central China","Northeast","Northwest","South China","Southwest","Early rice","Late rice"),ordered = TRUE)
Diffplot$Nstrategy<-factor(Diffplot$Nstrategy,levels = c("ON","EON"),ordered = TRUE)
Diffplot$Difftype<-factor(Diffplot$Difftype,levels = c("NEEB","Economic benefit","Yield","NUE","N balance","Yield-scaled N losses","Total N losses","N rate"),ordered = TRUE)

Fig_5<-ggplot(Diffplot,aes(Ndif,Difftype))+
  scale_y_discrete()+
  annotate("rect",xmin=-1.4,xmax=1.4,ymin=0.5,ymax=1.53,fill="light gray",col="transparent",alpha=.5)+
  annotate("rect",xmin=-1.4,xmax=1.4,ymin=2.55,ymax=3.53,fill="light gray",col="transparent",alpha=.5)+
  annotate("rect",xmin=-1.4,xmax=1.4,ymin=4.55,ymax=5.53,fill="light gray",col="transparent",alpha=.5)+
  annotate("rect",xmin=-1.4,xmax=1.4,ymin=6.55,ymax=7.53,fill="light gray",col="transparent",alpha=.5)+
  stat_summary(fun  ="mean",aes(fill=Nstrategy),geom="bar",size=2,position="dodge")+
  geom_errorbarh(aes(xmin=pmax(Ndifl,-1.4),xmax=pmin(Ndifh,1.4),y=Difftype,color=Nstrategy),height=.5,size=0.3,alpha=.9,position=position_dodge(1))+
  geom_vline(xintercept=0,size=.5,color="red",linetype=2)+
  facet_wrap(vars(Sub.1),nrow=2,drop = TRUE)+
  scale_x_continuous(limits = c(-1.4,1.4),expand=c(0,0),labels=scales::percent)+
  ylab(expression("Variable"))+
  xlab(expression("Difference relative to Farmer practice N (%)"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.title.y=element_blank())+
  scale_fill_npg()+
  scale_color_npg()+
  theme(legend.position =c(1,1),legend.justification = c(1,1),legend.title=element_blank(),legend.background = element_blank())

Fig5_dml <- rvg::dml(ggobj = Fig_5)#Convert ggobj to class dml 
officer::read_pptx() %>% # initialize PowerPoint slide
  officer::add_slide() %>%# specify object and location of object
  officer::ph_with(Fig5_dml, ph_location(width = 6, height = 4.8, left = 0.5, top = 0.5)) %>%
  base::print(target = here::here("Fig5.pptx"))

##Constructing Models for rice production and Nr (Supplementary Figs. 8-12)
Rice_data<-read.csv("CRNP_DB_2022.csv")
Rice_Data_Single<-subset(Rice_data,Crop.species == "Single-harvest rice ")
Rice_Data_Subregion<-split(Rice_Data_Single, list(Rice_Data_Single$Sub.1))
M2yield_East<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1|City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$`East China`) #Random mix effect model
rsquared(M2yield_East)
#summary(M2yield_East)
M2yield_Central<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$`Central China`)
rsquared(M2yield_Central)
#summary(M2yield_Central)
M2yield_North<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$`North China`)
rsquared(M2yield_North)
#summary(M2yield_North)
M2yield_Northeast<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$Northeast)
rsquared(M2yield_Northeast)
#summary(M2yield_Northeast)
M2yield_Northwest<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$Northwest)
rsquared(M2yield_Northwest)
#summary(M2yield_Northwest)
M2yield_South<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$`South China`)
rsquared(M2yield_South)
#summary(M2yield_South)
M2yield_Southwest<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1| City,method="REML",na.action=na.omit,data=Rice_Data_Subregion$Southwest)
rsquared(M2yield_Southwest)
#summary(M2yield_Southwest)
Data_Early<-subset(Rice_data,Sub.1=="Central China"|Sub.1=="South China")
Data_Early<-subset(Data_Early,Crop.species=="Early rice")
M2yield_Early<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1|City,method="REML",na.action=na.omit,data=Data_Early)
rsquared(M2yield_Early)
#summary(M2yield_Early)
Data_Late<-subset(Rice_data,Sub.1=="Central China"|Sub.1=="South China")
Data_Late<-subset(Data_Late,Crop.species=="Late rice")
M2yield_Late<-lme(RiceYield~NFertilizer+I(NFertilizer^2),random = ~1|City,method="REML",na.action=na.omit,data=Data_Late)
rsquared(M2yield_Late)
#summary(M2yield_Late)

#Fix effects of each regional models 
Fix_yield<-rbind(fixef(M2yield_East),fixef(M2yield_Central),fixef(M2yield_Northeast),fixef(M2yield_Northwest),fixef(M2yield_South),fixef(M2yield_Southwest), fixef(M2yield_Early),fixef(M2yield_Late))


#population prediction intervals to calculate CIs from p.257 Bolker 2008
x <- 0:1000; cis <- 0.95; lowb <- 0.5 - (cis / 2); upb <- 0.5 + (cis / 2)
vmat <- mvrnorm(1000, mu = fixef(M2yield_East), Sigma = vcov(M2yield_East))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Central), Sigma = vcov(M2yield_Central))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
CentralCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Northeast), Sigma = vcov(M2yield_Northeast))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
NortheastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Northwest), Sigma = vcov(M2yield_Northwest))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
NorthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_South), Sigma = vcov(M2yield_South))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
SouthCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Southwest), Sigma = vcov(M2yield_Southwest))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
SouthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Early), Sigma = vcov(M2yield_Early))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EarlyCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M2yield_Late), Sigma = vcov(M2yield_Late))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
f <- vmat[, 3] #coef for Nfer^2
yQ2_CIs = function(a, c, f,x){c+a*x+f*(x^2)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],f = f[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
LateCI<-data.frame(x,CIlb,CIub)



#Supplementary Fig 8
Fig_S8a<-ggplot(Rice_Data_Subregion$`East China`)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=EastCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [1,1]+ Fix_yield [1,2]*x+ Fix_yield [1,3]*x^2),aes(color="East China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8b<-ggplot(Rice_Data_Subregion$`Central China`)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=CentralCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [2,1]+ Fix_yield [2,2]*x+ Fix_yield [2,3]*x^2),aes(color="Central China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8c<-ggplot(Rice_Data_Subregion$Northeast)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=NortheastCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [3,1]+ Fix_yield [3,2]*x+ Fix_yield [3,3]*x^2),aes(color="Northeast China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8d<-ggplot(Rice_Data_Subregion$Northwest)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=NorthwestCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [4,1]+ Fix_yield [4,2]*x+ Fix_yield [4,3]*x^2),aes(color="Northwest China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8e<-ggplot(Rice_Data_Subregion$`South China`)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=SouthCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [5,1]+ Fix_yield [5,2]*x+ Fix_yield [5,3]*x^2),aes(color="South China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8f<-ggplot(Rice_Data_Subregion$Southwest)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=SouthwestCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [6,1]+ Fix_yield [6,2]*x+ Fix_yield [6,3]*x^2),aes(color="Southwest China"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8g<-ggplot(Data_Early)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=EarlyCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [7,1]+ Fix_yield [7,2]*x+ Fix_yield [7,3]*x^2),aes(color="Early rice"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
Fig_S8h<-ggplot(Data_Late)+
  geom_point(aes(NFertilizer,RiceYield),col="grey",alpha=1/2,size=1.5,position="jitter")+
  geom_ribbon(data=LateCI,aes(x=x,ymin=CIlb,ymax=pmin(CIub,12)),fill="grey",alpha = .5,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_yield [8,1]+ Fix_yield [8,2]*x+ Fix_yield [8,3]*x^2),aes(color="Late rice"),size=1,alpha=.8,show.legend = FALSE)+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Yield (Mg "~ha^-1~")"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "top",legend.title=element_blank())
plot_grid(Fig_S8a,Fig_S8b,Fig_S8c,Fig_S8d,Fig_S8e,Fig_S8f,Fig_S8g,Fig_S8h,labels = c("a","b","c","d","e","f","g","h"),label_size = 12, ncol = 4)
ggsave("Fig S8.png",width=12,heigh=8)



#NH3 models
NH3data<-subset(Rice_data,is.na(NH3.volatilization.kg.N.ha.1.)=="FALSE")
NH3data_Single<-subset(NH3data,Crop.species == "Single-harvest rice ")
NH3data_Subregion<-split(NH3data_Single, list(NH3data_Single$Sub.1))
M_East<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1| City,method="REML",na.action=na.omit,data=NH3data_Subregion$`East China`)
rsquared(M_East)
#summary(M_East)
M_Central<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1| City,method="REML",na.action=na.omit,data=NH3data_Subregion$`Central China`)
rsquared(M_Central)
#summary(M_Central)
M_Northeast<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1|  City,method="REML",na.action=na.omit,data=NH3data_Subregion$Northeast)
rsquared(M_Northeast)
#summary(M_Northeast)
M_Northwest<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1|  City,method="REML",na.action=na.omit,data=NH3data_Subregion$Northwest)
rsquared(M_Northwest)
#summary(M_Northwest)
M_Southwest<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1|  City,method="REML",na.action=na.omit,data=NH3data_Subregion$Southwest)
rsquared(M_Southwest)
#summary(M_Southwest)
Data_Double<-subset(NH3data,Sub.1=="Central China"|Sub.1=="South China")
Data_Early<-subset(Data_Double, Crop.species=="Early rice")
M_Early<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1|  City,method="REML",na.action=na.omit,data=Data_Early)
rsquared(M_Early)
#summary(M_Early)
Data_Late<-subset(Data_Double,Crop.species=="Late rice")
M_Late<-lme(NH3.volatilization.kg.N.ha.1.~NFertilizer,random = ~1|  City,method="REML",na.action=na.omit,data=Data_Late)
rsquared(M_Late)
#summary(M_Late)

#Fix effects of each regional models 
Fix_NH3<-rbind(fixef(M_East),fixef(M_Central),fixef(M_Northeast),fixef(M_Northwest),fixef(M_Southwest), fixef(M_Early),fixef(M_Late))

#population prediction intervals to calculate CIs from p.257 Bolker 2008
x <- 0:1000; cis <- 0.95; lowb <- 0.5 - (cis / 2); upb <- 0.5 + (cis / 2)
vmat <- mvrnorm(1000, mu = fixef(M_East), Sigma = vcov(M_East))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Central), Sigma = vcov(M_Central))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
CentralCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northeast), Sigma = vcov(M_Northeast))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
NortheastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northwest), Sigma = vcov(M_Northwest))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
NorthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Southwest), Sigma = vcov(M_Southwest))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
SouthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Early), Sigma = vcov(M_Early))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EarlyCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Late), Sigma = vcov(M_Late))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){c+a*x}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
LateCI<-data.frame(x,CIlb,CIub)

#Supplementary Fig 9
Fig_S9a<-ggplot(NH3data_Subregion$`East China`)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (Fix_NH3[1,1]+ Fix_NH3[1,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  geom_ribbon(data=EastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = c(0,0.6),legend.justification = c(0,0.6),legend.title=element_blank(),legend.background = element_blank())
Fig_S9b<-ggplot(NH3data_Subregion$`Central China`)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (Fix_NH3[2,1]+ Fix_NH3[2,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  geom_ribbon(data=CentralCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())
Fig_S9c<-ggplot(NH3data_Subregion$Northeast)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  geom_ribbon(data=NortheastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_NH3[3,1]+ Fix_NH3[3,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())
Fig_S9d<-ggplot(NH3data_Subregion$Northwest)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  geom_ribbon(data=NorthwestCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_NH3[4,1]+ Fix_NH3[4,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S9f<-ggplot(NH3data_Subregion$Southwest)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  geom_ribbon(data=SouthwestCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_NH3[5,1]+ Fix_NH3[5,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())
Fig_S9g<-ggplot(Data_Early)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  geom_ribbon(data=EarlyCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .3,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_NH3[6,1]+ Fix_NH3[6,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())
Fig_S9h<-ggplot(Data_Late)+
  geom_point(aes(NFertilizer,NH3.volatilization.kg.N.ha.1.),col="grey",alpha=1/2,size=1.5)+
  geom_ribbon(data=LateCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,150)),fill="grey",alpha = .3,show.legend = FALSE) +
  stat_function(fun=function(x) (Fix_NH3[7,1]+ Fix_NH3[7,2]*x),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.11*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(NH[3]~"emission (kg"~~N~ha^-1~")"))+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())
plot_grid(Fig_S9a,Fig_S9b,Fig_S9c,Fig_S9d,Fig_S9f,Fig_S9g,Fig_S9h,labels = c("a","b","c","d","e","f","g"),label_size = 12, ncol = 4)
ggsave("Fig S9.png",width=12,heigh=8)

#N2O Random mix-effect models
N2Odata<-subset(Rice_data,is.na(N2O.emission..kg.N.ha.)=="FALSE")
N2Odata_Single<-subset(N2Odata,Crop.species == "Single-harvest rice ")
N2Odata_Subregion<-split(N2Odata_Single, list(N2Odata_Single$Sub.1))
M_East<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=N2Odata_Subregion$`East China`)#Random mix effect model
rsquared(M_East)
#summary(M_East)
M_Central<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=N2Odata_Subregion$`Central China`)
rsquared(M_Central)
#summary(M_Central)
M_Northeast<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=N2Odata_Subregion$Northeast)
rsquared(M_Northeast)
#summary(M_Northeast)
M_South<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=N2Odata_Subregion$`South China`)
rsquared(M_South)
#summary(M_South)
M_Southwest<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=N2Odata_Subregion$Southwest)
rsquared(M_Southwest)
#summary(M_Southwest)
Data_Double<-subset(N2Odata,Sub.1=="Central China"|Sub.1=="South China")
Data_Early<-subset(Data_Double, Crop.species=="Early rice")
M_Early<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Early)
rsquared(M_Early)
#summary(M_Early)
Data_Late<-subset(Data_Double,Crop.species=="Late rice")
M_Late<-lme(I(log(N2O.emission..kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Late)
rsquared(M_Late)
#summary(M_Late)

#Fix effects of each regional models 
Fix_N2O<-rbind(fixef(M_East),fixef(M_Central),fixef(M_Northeast),fixef(M_South),fixef(M_Southwest), fixef(M_Early),fixef(M_Late))

#population prediction intervals to calculate CIs from p.257 Bolker 2008
x <- 0:1000; cis <- 0.95; lowb <- 0.5 - (cis / 2); upb <- 0.5 + (cis / 2)
vmat <- mvrnorm(1000, mu = fixef(M_East), Sigma = vcov(M_East))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Central), Sigma = vcov(M_Central))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
CentralCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northeast), Sigma = vcov(M_Northeast))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
NortheastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_South), Sigma = vcov(M_South))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
SouthCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Southwest), Sigma = vcov(M_Southwest))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
SouthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Early), Sigma = vcov(M_Early))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
EarlyCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Late), Sigma = vcov(M_Late))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
LateCI<-data.frame(x,CIlb,CIub)


#Supplementary Fig 10
Fig_S10a<-ggplot(N2Odata_Subregion$`East China`)+
  geom_ribbon(data=EastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[1,1])*exp(Fix_N2O[1,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = c(0,0.7),legend.justification = c(0,0.7),legend.title=element_blank(),legend.background = element_blank())

Fig_S10b<-ggplot(N2Odata_Subregion$`Central China`)+
  geom_ribbon(data=CentralCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,8)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[2,1])*exp(Fix_N2O[2,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S10c<-ggplot(N2Odata_Subregion$Northeast)+
  geom_ribbon(data=NortheastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,8)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[3,1])*exp(Fix_N2O[3,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S10d<-ggplot(N2Odata_Subregion$`South China`)+
  geom_ribbon(data=SouthCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,8)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[4,1])*exp(Fix_N2O[4,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S10e<-ggplot(N2Odata_Subregion$Southwest)+
  geom_ribbon(data=SouthwestCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,8)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[5,1])*exp(Fix_N2O[5,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S10f<-ggplot(Data_Early)+
  geom_ribbon(data=EarlyCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[6,1])*exp(Fix_N2O[6,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S10g<-ggplot(Data_Late)+
  geom_ribbon(data=LateCI,aes(x=x,ymin=pmax(CIlb,0),ymax=CIub),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,N2O.emission..kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_N2O[7,1])*exp(Fix_N2O[7,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.004*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,8),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression(N[2]~O~"-N"~"emission (kg"~~N~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

plot_grid(Fig_S10a,Fig_S10b,Fig_S10c,Fig_S10d,Fig_S10e,Fig_S10f,Fig_S10g,labels = c("a","b","c","d","e","f","g"),label_size = 12, ncol = 4)
ggsave("Fig S10.png",width=12,heigh=8)

#Runoff
Runoffdata<-subset(Rice_data,is.na(runoff.kg.N.ha.)=="FALSE")
Runoffdata_Single<-subset(Runoffdata,Crop.species == "Single-harvest rice ")
Runoffdata_Subregion<-split(Runoffdata_Single, list(Runoffdata_Single$Sub.1))
M_East<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Runoffdata_Subregion$`East China`)
rsquared(M_East)
#summary(M_East)
M_Central<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Runoffdata_Subregion$`Central China`)
rsquared(M_Central)
#summary(M_Central)
M_Northeast<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Runoffdata_Subregion$Northeast)
rsquared(M_Northeast)
#summary(M_Northeast)
M_South<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Runoffdata_Subregion$`South China`)
rsquared(M_South)
#summary(M_South)
M_Southwest<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Runoffdata_Subregion$Southwest)
rsquared(M_Southwest)
#summary(M_Southwest)
Data_Double<-subset(Runoffdata,Sub.1=="Central China"|Sub.1=="South China")
Data_Early<-subset(Data_Double, Crop.species=="Early rice")
M_Early<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Early)
rsquared(M_Early)
#summary(M_Early)
Data_Late<-subset(Data_Double,Crop.species=="Late rice")
M_Late<-lme(I(log(runoff.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Late)
rsquared(M_Late)
#summary(M_Late)

#Fix effects of each regional models 
Fix_runoff<-rbind(fixef(M_East),fixef(M_Central),fixef(M_Northeast),fixef(M_South),fixef(M_Southwest), fixef(M_Early),fixef(M_Late))


#population prediction intervals to calculate CIs from p.257 Bolker 2008
x <- 0:1000; cis <- 0.95; lowb <- 0.5 - (cis / 2); upb <- 0.5 + (cis / 2)
vmat <- mvrnorm(1000, mu = fixef(M_East), Sigma = vcov(M_East))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Central), Sigma = vcov(M_Central))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
CentralCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northeast), Sigma = vcov(M_Northeast))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
NortheastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_South), Sigma = vcov(M_South))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
SouthCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Southwest), Sigma = vcov(M_Southwest))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
SouthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Early), Sigma = vcov(M_Early))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
EarlyCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Late), Sigma = vcov(M_Late))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
LateCI<-data.frame(x,CIlb,CIub)

#Supplementary Fig 11
Fig_S11a<-ggplot(Runoffdata_Subregion$`East China`)+
  geom_ribbon(data=EastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[1,1])*exp(Fix_runoff[1,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = c(0,0.7),legend.justification = c(0,0.7),legend.title=element_blank(),legend.background = element_blank())

Fig_S11b<-ggplot(Runoffdata_Subregion$`Central China`)+
  geom_ribbon(data=CentralCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[2,1])*exp(Fix_runoff[2,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S11c<-ggplot(Runoffdata_Subregion$Northeast)+
  geom_ribbon(data=NortheastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[3,1])*exp(Fix_runoff[3,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S11d<-ggplot(Runoffdata_Subregion$`South China`)+
  geom_ribbon(data=SouthCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[4,1])*exp(Fix_runoff[4,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S11f<-ggplot(Data_Early)+
  geom_ribbon(data=EarlyCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[6,1])*exp(Fix_runoff[6,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S11g<-ggplot(Data_Late)+
  geom_ribbon(data=LateCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,20)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,runoff.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_runoff[7,1])*exp(Fix_runoff[7,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,20),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Runoff N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

plot_grid(Fig_S11a,Fig_S11b,Fig_S11c,Fig_S11d,Fig_S11f,Fig_S11g,labels = c("a","b","c","d","e","f"),label_size = 12, ncol = 3)
ggsave("Fig S11.png",width=9,heigh=8)

#Leaching
Leachingdata<-subset(Rice_data,is.na(leaching.kg.N.ha.)=="FALSE")
Leachingdata_Single<-subset(Leachingdata,Crop.species == "Single-harvest rice ")
Leachingdata_Subregion<-split(Leachingdata_Single, list(Leachingdata_Single$Sub.1))
M_East<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Leachingdata_Subregion$`East China`)
rsquared(M_East)
#summary(M_East)
M_Central<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Leachingdata_Subregion$`Central China`)
rsquared(M_Central)
#summary(M_Central)
M_Northeast<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Leachingdata_Subregion$Northeast)
rsquared(M_Northeast)
#summary(M_Northeast)
M_Northwest<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Leachingdata_Subregion$Northwest)
rsquared(M_Northwest)
#summary(M_Northwest)
M_Southwest<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1| City,method="REML",na.action=na.omit,data=Leachingdata_Subregion$Southwest)
rsquared(M_Southwest)
#summary(M_Southwest)
Data_Double<-subset(Leachingdata,Sub.1=="Central China"|Sub.1=="South China")
Data_Early<-subset(Data_Double, Crop.species=="Early rice")
M_Early<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Early)
rsquared(M_Early)
#summary(M_Early)
Data_Late<-subset(Data_Double,Crop.species=="Late rice")
M_Late<-lme(I(log(leaching.kg.N.ha.))~NFertilizer,random=~1|City,method="REML",na.action=na.omit,data=Data_Late)
rsquared(M_Late)
#summary(M_Late)


#Fix effects of each regional models 
Fix_leaching<-rbind(fixef(M_East),fixef(M_Central),fixef(M_Northeast),fixef(M_Northwest),fixef(M_Southwest), fixef(M_Early),fixef(M_Late))


#population prediction intervals to calculate CIs from p.257 Bolker 2008
x <- 0:1000; cis <- 0.95; lowb <- 0.5 - (cis / 2); upb <- 0.5 + (cis / 2)
vmat <- mvrnorm(1000, mu = fixef(M_East), Sigma = vcov(M_East))
a <- vmat[, 2] #coef for N fertilizer 
c <- vmat[, 1] #global intercept
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {
  dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)
}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {
  civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)
}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
EastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Central), Sigma = vcov(M_Central))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ]
CIub <- civec_yQ2 [2, ]
CentralCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northeast), Sigma = vcov(M_Northeast))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
NortheastCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Northwest), Sigma = vcov(M_Northwest))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
NorthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Southwest), Sigma = vcov(M_Southwest))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
SouthwestCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Early), Sigma = vcov(M_Early))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
EarlyCI<-data.frame(x,CIlb,CIub)

vmat <- mvrnorm(1000, mu = fixef(M_Late), Sigma = vcov(M_Late))
a <- vmat[, 2] ;c <- vmat[, 1] 
yQ2_CIs = function(a, c,x){exp(c)*exp(a*x)}
dist = array(dim = c(1000, length(x)))
for (i in 1:1000) {dist[i, ] = yQ2_CIs (a = a[i],c = c[i],x = x)}
civec_yQ2 <- array(dim = c(2, length(x)))
for (j in 1:length(x)) {civec_yQ2 [, j] <- quantile(dist[, j], c(lowb, upb), na.rm = TRUE)}
CIlb <- civec_yQ2 [1, ];CIub <- civec_yQ2 [2, ]
LateCI<-data.frame(x,CIlb,CIub)


#Supplementary Fig 12
Fig_S12a<-ggplot(Leachingdata_Subregion$`East China`)+
  geom_ribbon(data=EastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[1,1])*exp(Fix_leaching[1,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = c(0,0.7),legend.justification = c(0,0.7),legend.title=element_blank(),legend.background = element_blank())

Fig_S12b<-ggplot(Leachingdata_Subregion$`Central China`)+
  geom_ribbon(data=CentralCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[2,1])*exp(Fix_leaching[2,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S12c<-ggplot(Leachingdata_Subregion$Northeast)+
  geom_ribbon(data=NortheastCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[3,1])*exp(Fix_leaching[3,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S12d<-ggplot(Leachingdata_Subregion$Northwest)+
  geom_ribbon(data=NorthwestCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[4,1])*exp(Fix_leaching[4,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S12e<-ggplot(Leachingdata_Subregion$Southwest)+
  geom_ribbon(data=SouthwestCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[5,1])*exp(Fix_leaching[5,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S12f<-ggplot(Data_Early)+
  geom_ribbon(data=EarlyCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[6,1])*exp(Fix_leaching[6,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

Fig_S12g<-ggplot(Data_Late)+
  geom_ribbon(data=LateCI,aes(x=x,ymin=pmax(CIlb,0),ymax=pmin(CIub,40)),fill="grey",alpha = .5,show.legend = FALSE) +
  geom_point(aes(NFertilizer,leaching.kg.N.ha.),col="grey",alpha=1/2,size=1.5)+
  stat_function(fun=function(x) (exp(Fix_leaching[7,1])*exp(Fix_leaching[7,2]*x)),color="#E64B35CC",size=1,alpha=.8,show.legend = FALSE)+ 
  stat_function(fun=function(x) 0.24*x, aes(color="IPCC"),size=1,linetype=2)+
  scale_color_manual(values=c("#4DBBD5FF"),breaks=c("IPCC"))+
  scale_x_continuous(expand = c(0,0),limits = c(0,400))+
  scale_y_continuous(limits=c(0,40),expand = c(0,0))+
  xlab(expression("N rate (kg N "~ha^-1~")"))+
  ylab(expression("Leaching N loss (kg N"~~ha^-1~")"))+
  theme_bw()+
  theme(text = element_text(family = "RMN"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(legend.position = "none",legend.title=element_blank(),legend.background = element_blank())

plot_grid(Fig_S12a,Fig_S12b,Fig_S12c,Fig_S12d,Fig_S12f,Fig_S12g,labels = c("a","b","c","d","e","f","g"),label_size = 12, ncol = 4)
ggsave("Fig S12.png",width=12,heigh=8)

#Economic benefit optimal N 
Riceprice<-430 #430$/Mg
Nprice<-0.79 #0.79$/kg
#Optimal N rate (ON)
ON = array(dim = c(1, 8))
EB = array(dim = c(1, 8))
for (i in 1:8){
fun<-function(x) -((Fix_yield [i,1]+ Fix_yield [i,2]*x+ Fix_yield [i,3]*x^2)*Riceprice[1]-x*Nprice[1])
ON[i]<-optim(par=200,lower=0,upper=300,fn=fun,method="Brent",control = list(maxit=9000))[[1]]
EB[i]<--optim(par=200,lower=0,upper=300,fn=fun,method="Brent",control = list(maxit=9000))[[2]]
}
#Calculate upper and lower bound of EONR base on Sawyer et al. 2006
ON_h = array(dim = c(1, 8))
ON_l = array(dim = c(1, 8))
for (i in 1:8){
  fun<-function(x) ((Fix_yield [i,1]+ Fix_yield [i,2]*x+ Fix_yield [i,3]*x^2)*Riceprice[1]-x*Nprice[1])
  fun_b<-function(x) fun(x)-(EB[i]-2.47)
  ON_h[i]<-uniroot(fun_b,interval=c(ON[i],300))[1]
  ON_l[i]<-uniroot(fun_b,interval=c(0,ON[i]))[1]
}
ON<-rbind(c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),ON, ON_h, ON_l)

#Net environmental economic benefit N
#Sources of reactive N costs can be found at Table S2 (unit $/kg N)
NH3price<-1.98
NO3price<-17.17
GHGprice<-26.145
healthNH3<-1.85
healthN2O<-0.3
healthNO3<-0.21

Fix_NH3<-rbind(Fix_NH3[1:4,],Fix_NH3[5,],Fix_NH3[5:7,])
Fix_N2O<-rbind(Fix_N2O[1:3,],Fix_N2O[3,],Fix_N2O[4:7,])
Fix_runoff<-rbind(Fix_runoff[1:3,],Fix_runoff[3,],Fix_runoff[4:7,])
Fix_leaching<-rbind(Fix_leaching[1:4,],Fix_leaching[5,],Fix_leaching[5:7,])
NEEB<-array(dim = c(1, 8))
EON<-array(dim = c(1, 8))
NEEB_EON<-array(dim = c(1, 8))
for (i in 1:8){
  NEEB<- function(x) -((Fix_yield [i,1]+ Fix_yield [i,2]*x+ Fix_yield [i,3]*x^2)*Riceprice[1]-x*Nprice-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x)+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*0.0075+(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*0.01+(exp(Fix_N2O[i,1])*exp(Fix_N2O[i,2]*x)))*298*44/28/1000*GHGprice[1]-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x)+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*0.0075+(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*0.01+(exp(Fix_N2O[i,1])*exp(Fix_N2O[i,2]*x)))*healthN2O[1]-(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*(1-0.01)*(NH3price[1]+healthNH3[1])-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x))+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*(1-0.0075)*(NO3price[1]+healthNO3[1]))  
  EON[i]<- optim(par = 200, lower = 0, upper = 300, fn = NEEB, method = "Brent",control = list(maxit = 9000))[[1]]
  NEEB_EON[i]<--optim(par = 200, lower = 0, upper = 300, fn = NEEB, method = "Brent",control = list(maxit = 9000))[[2]]
}
#Calculate upper and lower bound of EONR base on Sawyer et al. 2006
EON_h = array(dim = c(1, 8))
EON_l = array(dim = c(1, 8))
for (i in 1:8){
  fun<-function(x) ((Fix_yield [i,1]+ Fix_yield [i,2]*x+ Fix_yield [i,3]*x^2)*Riceprice[1]-x*Nprice-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x)+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*0.0075+(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*0.01+(exp(Fix_N2O[i,1])*exp(Fix_N2O[i,2]*x)))*298*44/28/1000*GHGprice[1]-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x)+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*0.0075+(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*0.01+(exp(Fix_N2O[i,1])*exp(Fix_N2O[i,2]*x)))*healthN2O[1]-(Fix_NH3[i,1]+ Fix_NH3[i,2]*x)*(1-0.01)*(NH3price[1]+healthNH3[1])-((exp(Fix_leaching[i,1])*exp(Fix_leaching[i,2]*x))+(exp(Fix_runoff[i,1])*exp(Fix_runoff[i,2]*x)))*(1-0.0075)*(NO3price[1]+healthNO3[1]))  
  fun_b<-function(x) fun(x)-(NEEB_EON[i]-2.47)
  EON_h[i]<-uniroot(fun_b,interval=c(EON[i],300))[1]
  EON_l[i]<-uniroot(fun_b,interval=c(0,EON[i]))[1]
}
EON<-rbind(c("East China","Central China","Northeast China","Northwest China","South China","Southwest China","Early rice","Late rice"),EON, EON_h,EON_l)
