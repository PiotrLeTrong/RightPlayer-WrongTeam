rm(list = ls())
load("/Users/Piotr/Dropbox/DraftData/data.RData")
setwd("/Users/Piotr/Dropbox/DraftData/")
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggrepel)

df$Tm[df$Tm == "STL"] <- "LAR"
df <- df[complete.cases(df$Player),]
teams <- sort(unique(df$Tm))
df$Division <- "AFC East"

df$Division[df$Tm == "KAN" | df$Tm == "DEN" | df$Tm == "OAK" | df$Tm == "SDG"] <- "AFC West"
df$Division[df$Tm == "JAG" | df$Tm == "TEN" | df$Tm == "IND" | df$Tm == "HOU"] <- "AFC South"
df$Division[df$Tm == "CLE" | df$Tm == "PIT" | df$Tm == "CIN" | df$Tm == "BAL"] <- "AFC North"


df$Division[df$Tm == "LAR" | df$Tm == "SFO" | df$Tm == "ARI" | df$Tm == "SEA"] <- "NFC West"
df$Division[df$Tm == "DAL" | df$Tm == "WAS" | df$Tm == "PHI" | df$Tm == "NYG"] <- "NFC East"
df$Division[df$Tm == "NOR" | df$Tm == "ATL" | df$Tm == "TAM" | df$Tm == "CAR"] <- "NFC South"
df$Division[df$Tm == "CHI" | df$Tm == "MIN" | df$Tm == "DET" | df$Tm == "GNB"] <- "NFC North"
df$Tm[df$Player == "Philip Rivers"] <- "SDG"
df$Tm[df$Player == "Eli Manning"] <- "NYG"
df$DrAV[df$Player == "Philip Rivers"] <- df$CarAV[df$Player == "Philip Rivers"]
df$DrAV[df$Player == "Eli Manning"] <- df$CarAV[df$Player == "Eli Manning"]

df.2003 <- df[df$year>=2003,]
na <- df.2003[df.2003$CarAV>0 & is.na(df.2003$DrAV),]

table <- df.2003 %>% group_by(Tm, Rnd) %>% dplyr::summarise(meanCarAV = mean(CarAV, na.rm=TRUE), 
                                                       meanDrAV = mean(DrAV, na.rm=TRUE), 
                                                       medCarAV = median(CarAV, na.rm=TRUE),
                                                       medDrAV = median(DrAV, na.rm=TRUE))

table$Division <- "AFC East"

table$Division[table$Tm == "KAN" | table$Tm == "DEN" | table$Tm == "OAK" | table$Tm == "SDG"] <- "AFC West"
table$Division[table$Tm == "JAX" | table$Tm == "TEN" | table$Tm == "IND" | table$Tm == "HOU"] <- "AFC South"
table$Division[table$Tm == "CLE" | table$Tm == "PIT" | table$Tm == "CIN" | table$Tm == "BAL"] <- "AFC North"

table$Conference <- "AFC"

table$Division[table$Tm == "LAR" | table$Tm == "SFO" | table$Tm == "ARI" | table$Tm == "SEA"] <- "NFC West"
table$Division[table$Tm == "DAL" | table$Tm == "WAS" | table$Tm == "PHI" | table$Tm == "NYG"] <- "NFC East"
table$Division[table$Tm == "NOR" | table$Tm == "ATL" | table$Tm == "TAM" | table$Tm == "CAR"] <- "NFC South"
table$Division[table$Tm == "CHI" | table$Tm == "MIN" | table$Tm == "DET" | table$Tm == "GNB"] <- "NFC North"
table$Conference[grepl("NFC", table$Division)] <- "NFC"

table$MeanRghtPlyrWrngTm <- table$meanCarAV-table$meanDrAV
table$MedianRghtPlyrWrngTm <- table$medCarAV-table$medDrAV

ggplot(table, aes(x = Tm, y = medCarAV)) +
         geom_bar(stat = "identity", aes(color = Rnd, fill = Rnd)) +
  xlab("Round") +
  ylab("Median Career AV") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0,
                                  size = 20)) +
  facet_grid(Division~., scales = "free") +
  coord_flip()


div <- unique(table$Division)
conf <- unique(table$Conference)


for(i in 1:length(conf)){
  pdf(paste0("./", conf[i],".pdf"), 
      width = 14, 
      height = 8.5)
  print(
  ggplot(table, aes(x = Rnd, y = MedianRghtPlyrWrngTm), color = "#888888") +
    geom_line(aes(group = Tm), color = "#888888", alpha = 0.20) +
    geom_line(data = subset(table, table$Conference==conf[i]), aes(group = Tm, color = Tm)) +
    ggtitle(paste0("Conference: ", conf[i])) +
    scale_x_continuous(breaks = 1:7)+
    xlab("Round") +
    ylab("Right Player, Wrong Team") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
          axis.text.y = element_text(hjust = 1, size = 16),
          axis.ticks.x = element_blank(),
          strip.text.x = element_text(size = 12),
          plot.title = element_text(hjust = 0,
                                    size = 20))
  )
  dev.off()
}

ggplot(df, aes(x = overall, y=CarAV)) +
  geom_point(alpha=0.5, stroke=0) +
  geom_point(data = subset(df, df$College.Univ=="California"), aes(x = overall, y=CarAV), color = "blue") +
  scale_x_continuous() +
  facet_grid(.~Rnd, scales = "free") +
  theme(
    legend.position = "none"
  )

Cal <-  subset(df, df$College.Univ=="California")

table.out <- table[,c(1:6, 9:10)]

ggplot(table.out, aes(x = medCarAV, y = MedianRghtPlyrWrngTm)) +
  geom_jitter(data = subset(table.out),
              aes(), size = 2, alpha = 0.2, stroke = 0, color = "#888888") +
  geom_jitter(data = subset(table.out, table.out$Rnd<=2),
              aes(color = Tm, shape = as.factor(Rnd)), size = 4) +
  geom_text_repel(data = subset(table.out, table.out$Rnd <= 2), aes(label = Tm)) +
  scale_x_continuous(breaks = seq(0,50,10)) +
  geom_abline(intercept = 0, slope = 0.5) +
  geom_abline(intercept = 0, slope = 0.25) +
  xlab("Median Carreer AV") +
  ylab("Right Player, Wrong Team") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0,
                                  size = 20))

ggplot(table.out, aes(x = medCarAV, y = MedianRghtPlyrWrngTm)) +
  geom_jitter(data = subset(table.out, table.out$Rnd == 4),
              aes(color = Tm, shape = as.factor(Rnd)), size = 4) +
  geom_text_repel(data = subset(table.out, table.out$Rnd == 4), aes(label = Tm)) +
  scale_x_continuous(breaks = seq(1,15,1)) +
  geom_abline(intercept = 0, slope = 0.5) +
  geom_abline(intercept = 0, slope = 0.25) +
  xlab("Median Carreer AV") +
  ylab("Right Player, Wrong Team") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0,
                                  size = 20))

ggplot(table.out, aes(x = medCarAV, y = MedianRghtPlyrWrngTm))+
  geom_jitter(data = subset(table.out),
              aes(), size = 2, alpha = 0.2, stroke = 0, color = "#888888") +
  geom_jitter(data = subset(table.out, table.out$Rnd == 6),
              aes(color = Tm, shape = as.factor(Rnd)), size = 6) +
  geom_text_repel(data = subset(table.out, table.out$Rnd == 6), aes(label = Tm)) +
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10)) +
  scale_y_continuous(breaks = seq(-5,5,1), limits = c(-5,5)) +
  geom_abline(intercept = 0, slope = 0.5) +
  geom_abline(intercept = 0, slope = 0.25) +
  xlab("Median Carreer AV") +
  ylab("Right Player, Wrong Team") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0,
                                  size = 20))

Cal.table <- Cal %>% group_by(Player, Tm, Rnd) %>% dplyr::summarise(meanCarAV = mean(CarAV, na.rm=TRUE), 
                                                                    meanDrAV = mean(DrAV, na.rm=TRUE))
Cal.table$MeanRghtPlyrWrngTm <- Cal.table$meanCarAV-Cal.table$meanDrAV

ggplot(data = subset(Cal.table, Cal.table$meanCarAV>0), aes(x = meanCarAV, y = MeanRghtPlyrWrngTm)) +
  geom_jitter(aes(color = Tm), size = 4) +
  geom_text_repel(data = subset(Cal.table, Cal.table$meanCarAV>25),
                  aes(label = Player)) +
  xlab("Median Carreer AV") +
  ylab("Right Player, Wrong Team") +
  geom_abline(intercept = 0, slope = 0.5) +
  geom_abline(intercept = 0, slope = 0.25) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.text.y = element_text(hjust = 1, size = 16),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0,
                                  size = 20))

rounds <- df.2003 %>% group_by(Rnd) %>% dplyr::summarise(meanCarAV = mean(CarAV, na.rm=TRUE), 
                                                             meanDrAV = mean(DrAV, na.rm=TRUE), 
                                                             medCarAV = median(CarAV, na.rm=TRUE),
                                                             medDrAV = median(DrAV, na.rm=TRUE))
low <- as.numeric(round(rounds[1, 2])) - 2.5
high <- as.numeric(round(rounds[1, 2])) + 2.5

goodplyrs.rnd1 <- df[df$CarAV == as.numeric(round(rounds[1, 2])),]
goodplyrs.rnd1 <- goodplyrs[complete.cases(goodplyrs$Player),]

for(i in 1:7){
  assign(paste0("goodplyrs.rnd",i), df[df$CarAV == as.numeric(round(rounds[i, 2])),])
}

ls(pattern ="goodplyrs.rnd")

goodplyrs.rnd1 <- goodplyrs.rnd1[complete.cases(goodplyrs.rnd1$Player),]
goodplyrs.rnd2 <- goodplyrs.rnd2[complete.cases(goodplyrs.rnd2$Player),]
goodplyrs.rnd3 <- goodplyrs.rnd3[complete.cases(goodplyrs.rnd3$Player),]
goodplyrs.rnd4 <- goodplyrs.rnd4[complete.cases(goodplyrs.rnd4$Player),]
goodplyrs.rnd5 <- goodplyrs.rnd5[complete.cases(goodplyrs.rnd5$Player),]
goodplyrs.rnd6 <- goodplyrs.rnd6[complete.cases(goodplyrs.rnd6$Player),]
goodplyrs.rnd7 <- goodplyrs.rnd7[complete.cases(goodplyrs.rnd7$Player),]

table.out$PercentileLostMedian <- table.out$MedianRghtPlyrWrngTm/table.out$medCarAV *100
table.out.cgb <- table.out[table.out$medCarAV>10,]
table.out.cgb <- as.data.frame(table.out.cgb)
table.out.cgb <- arrange(table.out.cgb, desc(PercentileLostMedian))
table.out.cgb$PercentileLostMedian <- paste0(sprintf("%.2f",table.out.cgb$PercentileLostMedian), "%")
table.out.cgb <- table.out.cgb[1:10,c(1,2,5,6,8,9)]
write.csv(table.out.cgb, "./table.csv")

df$Difference <- df$CarAV-df$DrAV