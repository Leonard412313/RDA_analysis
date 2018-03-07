#载入vegan包  
library(vegan)  
#读取“样本-物种”文件  
sp <- read.table("C:\\Users\\JL_Zh\\Desktop\\RDA_analysis\\sample-species.txt",
                 sep = "\t",header = T,row.names = 1)  

sp  
#读取“样本-环境因子”文件  
se <- read.table("C:\\Users\\JL_Zh\\Desktop\\RDA_analysis\\sample-enviroment.txt",
                 sep = "\t",header = T,row.names = 1)  

se  
#选择用RDA还是CCA分析？先用“样本-物种”文件做DCA分析！  
decorana(sp)   
#根据看分析结果中Axis Lengths的第一轴的大小  
#如果大于4.0,就应选CCA（基于单峰模型，典范对应分析）  
#如果在3.0-4.0之间，选RDA和CCA均可  
#如果小于3.0, RDA的结果会更合理（基于线性模型，冗余分析）  
#以RDA分析为例  
sp0 <- rda(sp ~ 1, se)    
sp0  
plot(sp0)  
#加入所有环境变量排序，RDA分析  
sp1 <- rda(sp ~ ., se)    
sp1  
plot(sp1)  
#到这里RDA图已经出来了,很多文章也都直接放这张图,但如果想追求更美的话,还可以找ggplot2包借个衣服,包装自己  
#准备作图数据：提取RDA分析结果的数据,作为新图形元素  
new <- sp1$CCA  
new  
#提取并转换“样本”数据  
samples <- data.frame(sample = row.names(new$u),RDA1 = new$u[,1],RDA2 = new$u[,2])  
samples  
#提取并转换“物种”数据  
species <- data.frame(spece = row.names(new$v),RDA1 = new$v[,1],RDA2 = new$v[,2])  
species  
#提取并转换“环境因子”数据  
envi <- data.frame(en = row.names(new$biplot),RDA1 = new$biplot[,1],RDA2 = new$biplot[,2])  
envi  
#构建环境因子直线坐标  
line_x = c(0,envi[1,2],0,envi[2,2],0,envi[3,2],0,envi[4,2],0,envi[5,2],0,envi[6,2])  
line_x  
line_y = c(0,envi[1,3],0,envi[2,3],0,envi[3,3],0,envi[4,3],0,envi[5,3],0,envi[6,3])  
line_y  
line_g = c("pH","pH","T","T","S2","S2","NH4","NH4","NO2","NO2","Fe2","Fe2")  
line_g  
line_data = data.frame(x = line_x,y = line_y,group = line_g)  
line_data  
#载入ggplot2包  
library(ggplot2)  
#开始重绘RDA图  
#填充样本数据，分别以RDA1,RDA2为X,Y轴，不同样本以颜色区分  
ggplot(data = samples,aes(RDA1,RDA2)) + geom_point(aes(color = sample),size = 2) +  
  #填充微生物物种数据，不同物种以图形区分  
  geom_point(data = species,aes(shape = spece),size = 2) +   
  #填充环境因子数据，直接展示  
  geom_text(data = envi,aes(label = en),color = "blue") +  
  #添加0刻度纵横线  
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +  
  #添加原点指向环境因子的直线  
  geom_line(data = line_data,aes(x = x,y = y,group = group),color = "green") +  
  #去除背景颜色及多余网格线  
  theme_bw() + theme(panel.grid = element_blank())  
#大功告成，保存为矢量图等等  
ggsave("RDA2.PDF")  