library(ggplot2)
library(plyr)
library(reshape)
library(scales)
aa<-read.table("COME_out/lncrna_geneid_class")
bb<-read.table("~/personaldata/lncRNA_20161105/cuffNAA/cuffNAA-CPC_left.del.class")
common<-read.table("common_COME_CPC_class")
cc<-as.data.frame(table(aa$V2))
dd<-as.data.frame(table(bb$V2))
common_data<-as.data.frame(table(common$V2))
cc$Freq2<-as.factor(dd$Freq)
cc$Freq3<-as.factor(common_data$Freq)
colnames(cc)<-c("class","COME","CPC","common")
nn<-melt(cc,id.vars=c("class"))
nn$value<-as.numeric(nn$value)
for(i in 1:15){
	if(nn$variable[i]=="COME"){
	nn$percentage[i]<-round(nn$value[i]/sum(nn[nn$variable=="COME",]$value), digits = 2)
	}else if(nn$variable[i]=="CPC"){
	nn$percentage[i]<-round(nn$value[i]/sum(nn[nn$variable=="CPC",]$value), digits = 2)
	}else{
	nn$percentage[i]<-round(nn$value[i]/sum(nn[nn$variable=="common",]$value), digits = 2)}
}

pdf("percentage_class.pdf")
ggplot(nn,aes(x = variable, y = value,fill = class)) +
	 theme_bw() + 
	 theme(panel.border= element_blank(), text = element_text(size=15))+
	 theme(axis.line.x = element_line(color="black"),
	       axis.line.y = element_line(color="black"),
		   axis.text = element_text(color="black")) +
     geom_bar(position = "fill",stat = "identity",width = .5) +
	 xlab("Method") +
	 ylab("") +
     scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
	 geom_text(aes(label = paste(value,"(",percentage*100,"%",")")), position=position_fill(vjust = 0.5), size=5)
dev.off()
