library(ggplot2)
library(plyr)
library(reshape)
library(scales)
aa<-read.table("different_gene_numbers.xls",header=T)
aa$order<-c(1:5)
trade.m <- melt(aa, id.vars = c("sample", "order"))
trade.a <- ddply(trade.m, c("order", "sample", "variable"), summarise, value = sum(value))
trade.a$sample <- as.character(trade.a$sample)
trade.a$sample <- factor(trade.a$sample, levels=unique(trade.a$sample))
ggplot(trade.a, aes(x=sample, fill=variable)) +
      geom_bar(data = subset(trade.a, variable == "up"), width=0.5,
      aes(y = value), stat = "identity")
pdf(file="up_down_gene.pdf")
last_plot() + geom_bar(data = subset(trade.a, variable == "down"), width=0.5,
    aes(y = -value), stat = 'identity') +
    xlab("") +
    ylab("down  -  up") +
	ylim(-2400, 2400) +
	theme(text = element_text(family="Times", size=20, face="bold", color="black"),
	axis.text = element_text(family="Times", size=20, face="bold", color="black"),
	axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
