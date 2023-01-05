#Soc 334Final
#Normal Dağılım grafiği
#pnorrm(x, mean=, sd=) = CDF
#dnorm = height of PDF, density
#rnorm() = random Normal generator
#qnorm(%) = CDF score to (z) score
# geom_...(..., stat="") = stat_...(..., geom ="")

library(ggplot2)
#import edusavings.tsv

#Final Ödevi 1 Soru 1a
m <- ggplot(data.frame(x = c(47.5, 152.5)), aes(x)) +
  stat_function(fun = dnorm,geom = "line", size=1, args = list(mean = 100,sd = 15), xlim = c(47.5, 145)) +
  geom_area(fun = dnorm,
                stat = "function",
                fill = "green4",
                args = list(
                  mean = 100,
                  sd = 15), 
                xlim = c(133, 160)) +
  scale_x_continuous(breaks=c(seq(55,145,by=15), 133)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x= element_text(face="bold", colour="blue", size = 12)) +
  labs(title = "IQ Dağılımı", x="133 IQ'lu Mensa Üyelerinin Dağılımı") +
  geom_segment(mapping= aes(x = 133, y = 0, xend = 133, yend = 0.0024), linetype="solid", color = "purple", linewidth=1.5 )

m

#Final Ödevi 1 Soru 1b
mg <- ggplot(data.frame(x = c(55, 145)), aes(x)) +
  stat_function(fun = dnorm,geom = "line", size=1, args = list(mean = 100,sd = 5), xlim = c(55, 130)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "green4",
                args = list(
                  mean = 100,
                  sd = 5), 
                xlim = c(133, 145)) +
  scale_x_continuous(breaks=c(seq(55,145,by=5), 133)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x= element_text(face="bold", colour="blue", size = 12)) +
  labs(title = "IQ Dağılımı", x="9 Kişilik Grubun IQ Dağılımı")+
  geom_hline(yintercept=0, linetype="solid", size=0.5, color="red")

mg

#Final Ödevi 1 Soru 2

a <- ggplot(data.frame(x = c(0,5), y=c(0,0.2)), aes(x,y))+
  scale_x_continuous(breaks=c(0,2,3,5)) + 
  stat_function(fun = dunif, args = list(min = 0, max = 5), geom = "area", fill = "green")+
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax = 0.2), fill="red")+
  geom_segment(mapping = aes(x = 5, y = 0, xend = 5, yend = 0.2), linetype="dashed", color = "red", size=1.5) +
  geom_segment(mapping = aes(x = 0, y = 0.2, xend = 5, yend = 0.2), linetype="dashed", color = "red", size=1.5)+
  geom_segment(mapping = aes(x = 2, y = 0, xend = 2, yend = 0.2), linetype="solid", color = "blue", size=1.5) +
  geom_segment(mapping = aes(x = 3, y = 0, xend = 3, yend = 0.2), linetype="solid", color = "blue", size=1.5) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y= element_text(face="bold", colour="blue", size = 10),
        axis.title.x= element_text(face="bold", colour="blue", size = 12)) +
  labs(title = "Havaalanı bekleme süreleri dağılımı", y = "Olasılık Yoğunluk Fonksiyonu", x="Geçen Dakikalar")
  
  
a  


#Final Ödevi 1, Soru 3

p <- ggplot(data.frame(x = c(41.5, 249.5)), aes(x)) +
  stat_function(fun = dnorm,geom = "line", args = list(mean = 143,sd = 29), xlim = c(41.5, 140)) +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "green4",
                args = list(
                  mean = 143,
                  sd = 29), 
                xlim = c(140, 211)) +
  stat_function(fun = dnorm,
                geom = "line", 
                args = list(
                  mean = 143,
                  sd = 29),
                xlim = c(211, 249.5)) +
  scale_x_continuous(breaks=c(seq(56,230,by=29), 140, 211)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x= element_text(face="bold", colour="blue", size = 12)) +
  labs(title = "Kadın Pilotların Ağırlıkları Dağılımı", x="Kadınların Ağırlıkları Dağılımı") +
  geom_vline(xintercept=140, linetype="dashed", color = "red", size=1.5) +
  geom_segment(mapping= aes(x = 211, y = 0, xend = 211, yend = 0.0008), linetype="dashed", color = "red", size=1.5 )
      
p

#Final Ödevi 3

# Görsel ayrıma dayanarak 2 parçalı Doğrusal olmayan Regresyon


edus_highregime <- subset(edusavings, edusavings$Education.expenditure.Percent.of.GNI.2019 >= 4.8)
edus_lowregime <- subset(edusavings, edusavings$Education.expenditure.Percent.of.GNI.2019 <= 4.8)
eduPlot <- ggplot(edusavings,aes(x=Education.expenditure.Percent.of.GNI.2019,y=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019))+
  geom_point()+
  geom_smooth(data=edus_highregime, method = "lm")+
  geom_smooth(data=edus_lowregime, method = "lm")

eduPlot

#Loess yöntemiyle doğrusal olmayan regresyon, locally weighted smoothing

eduPlot <- ggplot(edusavings,aes(x=Education.expenditure.Percent.of.GNI.2019,y=Unemployment.with.advanced.education.Percent.of.total.labor.force.2019))+
  geom_point()+
  geom_smooth(method = "loess")

eduPlot

