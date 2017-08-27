library(dplyr)
library(ggplot2)
library(grDevices)
class<-read.table("classification_modified_up.xls",header=F,sep="\t")
aa<-as.data.frame(class %>% group_by(V1) %>% arrange(desc(V7),.by_group = TRUE))
colnames(aa)<-c("Biological_Process","GO.ID","GO_Term","Annotated","Significant","Expected","pvalue","catagory")
aa_vec <- as.vector(c(as.vector(aa[aa$Biological_Process=="others",3]),as.vector(aa[aa$Biological_Process!="others",3])))
aa$Biological_Process <- factor(aa$Biological_Process, levels = c(levels(aa$Biological_Process)[3],levels(aa$Biological_Process)[-3]))
pdf("modified_GO_up.pdf")
par(mar=c(1,22,5,1))
ggplot(data=aa, aes(x=GO_Term, y=-(log10(pvalue)),fill=Biological_Process))+
		geom_bar(stat="identity",width=0.5)+
		coord_flip()+
		theme_bw()+
		scale_x_discrete(limits=aa_vec)+
		theme(axis.line.x=element_line(size=0.1),
			  axis.ticks.x=element_line(size=0.1),
			  axis.ticks.y=element_line(size=0.1),
			  axis.line.y=element_line(size=0.1),
			  panel.border = element_blank(),
		      panel.grid.major = element_blank(),
		      panel.grid.minor = element_blank(),
		   	  axis.line = element_line(colour = "black"),
		      axis.text.y=element_text(size=7),
			  legend.text=element_text(size=8),
			  legend.key.size = unit(0.3, "cm"))+
		scale_y_continuous(expand = c(0,0))+
		scale_fill_discrete(guide = guide_legend(reverse=T))
dev.off()
