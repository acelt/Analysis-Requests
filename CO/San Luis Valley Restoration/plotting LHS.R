library(tidyverse)
library(scales)

data_altr <- read.csv(file = "C:\\Users\\alaurencetraynor\\Documents\\CO\\SLVFO Restoration\\outputs\\slvfo_alamosatrinchera_analysis_2023-11-01.csv")

data_altr$Rating <- as.factor(data_altr$Rating)
data_altr$Rating <-factor(data_altr$Rating, levels = c("Not Meeting", "Meeting"))

data_altr_LHS1 <- data_altr[1:18,]
data_altr_LHS3 <- data_altr[18:32,]

dodge1 = position_dodge(width = 0.5)

ggplot(data_altr_LHS1, aes(y = Estimated.hectares*2.47105, x = Indicator, col = Rating))+
  geom_point(position = dodge1)+
  geom_errorbar(aes(ymin = Lower.confidence.bound.of.acres..80...Goodman.multinomial.*2.47105,
                    ymax = Upper.confidence.bound.of.acres..80...Goodman.multinomial.*2.47105),
                position = dodge1,
                width = 0.3)+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 1))+
  labs(y = "Acres", 
       x = "")+
  expand_limits(y=c(0,50000))+
  geom_abline(intercept = 162889.4, linetype = 2, col = "blue")+
  scale_y_continuous(labels =  scales::comma)+
  ggtitle("Land Health Standard 1 - Alamosa Trinchera")
  

ggplot(data_altr_LHS3, aes(y = Estimated.hectares*2.47105, x = Indicator, col = Rating))+
  geom_point(position = dodge1)+
  geom_errorbar(aes(ymin = Lower.confidence.bound.of.acres..80...Goodman.multinomial.*2.47105,
                    ymax = Upper.confidence.bound.of.acres..80...Goodman.multinomial.*2.47105),
                position = dodge1,
                width = 0.3)+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 1))+
  labs(y = "Acres", 
       x = "")+
  expand_limits(y=c(0,50000))+
  geom_abline(intercept = 162889.4, linetype = 2, col = "blue")+
  scale_y_continuous(labels =  scales::comma)+
  ggtitle("Land Health Standard 3 - Alamosa Trinchera")
