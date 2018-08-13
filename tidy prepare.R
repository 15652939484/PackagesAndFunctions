" using 
  source('E:/Rcools/package_preparing_full_version.R', encoding = 'UTF-8')
to load this file
"



windowsFonts(
  # 中文字体
  lishu = windowsFont(family = "LiSu"),            # 隶书
  yahei = windowsFont(family = "Microsoft YaHei"), # 微软雅黑
  xinwei = windowsFont(family = "STXingwei"),      # 华文新魏
  kaiti = windowsFont(family = "KaiTi"),           # 楷体
  heiti = windowsFont(family = "SimHei"),          # 黑体
  # 英文字体
  arial = windowsFont(family = "Arial"),             # Arial字体
  newman = windowsFont(family = "Times New Roman"),  #Times New Roman字体   
  hand = windowsFont(family = "Lucida Calligraphy"), # Lucida手写体
  Helvetica = windowsFont(family = "Helvetica")      # 印刷体
)  







library(DescTools)
library(data.table)
library(agricolae)  ### LSD.test  函数带不动之时。。。
# devtools::install_github("xia-lab/MetaboAnalystR")
"ruguoyaoshiyong MetaboAn 进行分析"
library("openxlsx")
"数据分析的基本包,防止电脑带不动"
# 导言区
# 1.2安装及装载相应的包
library(openxlsx)
library(gplots)
require(stats)
library(stats)
# library(multcomp)
library(agricolae)
require(grid)
library(Hmisc)
library(grid)
library(magrittr)
library(checkmate)
library(forestplot)
library(Hmisc)
library(magrittr)
library(checkmate)
library(forestplot)
require("RColorBrewer")
library(plyr)
library(dplyr)## pipeline function
library(mice)
library(pROC)
library(openxlsx)#EXL接口的神器
library(MASS)#回归包
library(gcookbook)
library(plyr)#使用 . 作为强大的 “else element in the database"
library(reshape2)#数据整合重构的包
library(corrplot)
library(ellipse)#画响板图的包
library(leaps)
library(car)
library(VIM)
library(corrplot)
library(rmarkdown)
require(ggplot2)
# library(multcomp)
windowsFonts(TT = windowsFont("times"))









##所有安装包汇集于此
##

# 临时安装的包:先暂存于此

# install.packages("NMF");# library("NMF") #### 为了安装 aheatmap  , pheatmap的升级版, 结果发现没乱用,贼菜

# install.packages("stringi")
# install.packages("zoo")
# library("stringi")

windowsFonts(TT = windowsFont("times"))## 字体准备###


# install.packages("fBasics")

#####################################################################
#### Part One 基础环境的设定#########################################
#####################################################################

## packages updates
##升级R包：R的版本更迭之后，可以把老R包复制到新版本的library目录下，然后运行 
# update.packages(checkBuilt=TRUE, ask=FALSE)##relase when needed




# getwd()  #系统路径
# setwd("C:\\Users\\Rcool") ##更换系统路径


##升级R##########################################
# install.packages("installr")
# require(installr)
#updateR()
#installr()##运行后加手动选择的，可以更新多种软件。
#################------------------------------

###########功能语句：

# .libPaths()###查看包的默认路径
# .libPaths()## 同样可以增加R包的默认路径。
# search() ####搜索R包
# update.packages()##更新R包
# installed.packages() ###查看已经安装的R包的信息
# help(iris) ##查看官方数据集的背景
# vignette()###查看具有官方文档——pdf说明的包
# vignette("ggplot2")###查看某一个数据集的官方文档
# apropos("mean")#模糊查询查看包含"mean" 语句的函数，语句

#####################################################################
#### Part Two 所需包的检查安装#######################################
#####################################################################



##2.1 部分不太一般的工具包
#R 的工具包
# install.packages("installr")
# require(installr)
# installr::install.rtools()#初次使用要用左边的代码安装rtools
# library("rtools")

### R每次升级后，可能需要单独重新安装部分比较矫情的包
# install.packages("data.table")
# install.packages("yaml")
# install.packages("mvtnorm")


##2.2 自动安装需要但是尚未安装的包


"2.2.1 CRAN 来源的包 "
packages_installed <- installed.packages()   ###获取已经安装的包的信息
packages_installed <- packages_installed[,1] ###将已经安装的包的名字赋值给 packages_installed

packages_needed_to_be_checked <- c(#############将需要的R包列在下面，避免每次都安装
  "data.table",
  "tidyverse",#大神的数据整理包
  "later",
  "pls",
  "ggrepel",
  "ddalpha",
  "DescTools",
  "cowplot",
  "readr",
  # "PMCMRplus",
  "Rcpp",
  # "recharts", # library("recharts")##  貌似不支持ggplot 的图形交互，且又是另一套代码体系，因此不予考虑
  "ggplot2", #######最强R包，绘图
  "plotly" , ### 可视化交互 ggplot2  的包，必学
  "shiny", #交互式图表       
  "DescTools",## DunnettTest jianj  检验所需函数
  "stringr",###字符串处理专用，重要，尤其在需要计算字符串位数时，用nchar 可能会在utf-8下得不到想要的结果
  "cluster",#聚类分析包
  "rggobi", #随便找了一个，用来测试循环体……
  
  'epiDisplay', ######流行病学包
  "openxlsx",   ######打开elx 文件最实用的包，秒杀 read.csv,但是有时读取数值会莫名其妙的出现好多位有效数字，需要注意
  "VIM",       #####缺失值分析包
  "zoo",
  # "multicomp",
  "ellipse",
  "corrplot", ###相关图
  "reshape2", ##数据长变宽、宽变长的塑形包
  "plyr", ###数据归约处理经典包
  "gcookbook",
  "car", ##数据分析经典包
  "PMCMR",
  
  "utils",
  "rlang",
  #### great packages for metabolism analyse
  "muma",# see:@ An R Package for Metabolomics Univariate and Multivariate Statistical Analysis.
  "formatR",#谢益辉大神 ，整理代码的包
  "rpart",
  "rpart.plot",
  "maptree",
  "RWeka",
  "leaps",  
  "rJava", ###Java 环境
  "nortest",
  "colorspace",
  "dendextend",
  "kernlab",
  "raster",
  "gplots",
  # "agricolae",
  "Hmisc", 
  "devtools", ### device tools 
  "ggdendro",#绘制树状图
  "beepr",## 想起系统提示音
  "forestplot",  ###森林图
  "mice",###
  "pROC"###ROC 曲线
  # "multcomp"
)###多重比较用的包 ，注意最后一行不需要逗号，别逗

for (each_package in packages_needed_to_be_checked){
  if (each_package %in% packages_installed ==F){
    install.packages(each_package)
  }
}


# # " 2.2.2 Bioconductor 来源的包 "
# 


# install.packages("httpuv")

metr_pkgs <- c("Rserve", "ellipse", "scatterplot3d", "Cairo", "randomForest", "caTools", "e1071", "som", "impute", "pcaMethods", "RJSONIO", "ROCR", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "pheatmap", "SSPA", "sva", "Rcpp", "pROC", "data.table", "limma", "car", "fitdistrplus", "lars", "Hmisc", "magrittr", "methods", "xtable", "pls", "caret", "lattice", "igraph", "gplots", "KEGGgraph", "reshape", "RColorBrewer", "tibble","LMGene","httpuv")

list_installed <- installed.packages()

new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))

if(length(new_pkgs)!=0){
  source("http://bioconductor.org/biocLite.R") ### using http instead of the https . works sometimes.
  biocLite(new_pkgs, dependencies = TRUE, ask = FALSE)
  print(c(new_pkgs, " packages added..."))
}


# devtools::install_github("xia-lab/MetaboAnalystR", build_vignettes=TRUE)
devtools::install_github("xia-lab/MetaboAnalystR")

metanr_packages <- function(){
  
  metr_pkgs <- c("Rserve", "ellipse", "scatterplot3d", "Cairo", "randomForest", "caTools", "e1071", "som", "impute", "pcaMethods", "RJSONIO", "ROCR", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "pheatmap", "SSPA", "sva", "Rcpp", "pROC", "data.table", "limma", "car", "fitdistrplus", "lars", "Hmisc", "magrittr", "methods", "xtable", "pls", "caret", "lattice", "igraph", "gplots", "KEGGgraph", "reshape", "RColorBrewer", "tibble", "siggenes", "plotly",
                 "remotes")
  
  list_installed <- installed.packages()
  
  new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  
  if(length(new_pkgs)!=0){
    
    source("https://bioconductor.org/biocLite.R")
    biocLite(new_pkgs, dependencies = TRUE, ask = FALSE)
    print(c(new_pkgs, " packages added..."))
  }
  
  if((length(new_pkgs)<1)){
    print("No new packages added...")
  }
}
metanr_packages()


# biocLite("BiocUpgrade")## 生物学的包升级？
biocLite() ### 里面啥也不加，生物相关包自动升级
biocLite("xia-lab/MetaboAnalystR", dependencies = TRUE)
"trouble shooting"


"最重要的一行,莫过于设定镜像了.下面一行代码是手动设定镜像的,选一个中国的,速度飞起来"
# chooseBioCmirror() #####

"# biocValid()  " ### 生物学的包验真
# 自动检测及安装需要 从 Biolite 下载的包

##
bio_package <- c("MAIT",
                 "faahKO",###包含了 XCMS 等包
                 "metabolomics",
                 "MetaboAnalystR",
                 "MWASTools" ### 参考文献: doi: 10.1093/bioinformatics/btx477
)
## 安装生物学来源的几个包(安装语句不同)
bio_package_need_to_be_installed_num <- 0 ### 记录需要安装的生物报道额个数
for (each_bio_package in bio_package) {
  
  if (each_bio_package %in% installed.packages() ==F){
    bio_package_need_to_be_installed_num <- bio_package_need_to_be_installed_num+1 ### 计数 需要安装的bio 包的个数
    if(bio_package_need_to_be_installed_num ==1)### 第一个包 安装前, 运行指定程序, 准备生物包的安装环境
    {
      install.packages("BiocInstaller", repos="https://bioconductor.org/packages/3.3/bioc")  ## 如果 无法 运行  source("https://bioconductor.org/biocLite.R")## 尝试这一行
      # library("BiocInstaller")
      # source("https://bioconductor.org/biocLite.R")####生物学专用的 包的下载地址 , 若是 https 不可以, 就换用 http.
      source("http://bioconductor.org/biocLite.R")####生物学专用的 包的下载地址
    } ### 当https 网址无法登陆时，尝试，http网址。
    
    biocLite(each_bio_package) ### 特定的生物学包 安装语句
  }
}
# 
#   "生物雪来源的包，比较矫情，单独安装单独加载，谢谢"
library("BiocInstaller")
library("MWASTools")
# library("faahKO")
library("metabolomics")
library("chemometrics")
library("MAIT")
# library("fBasics")
library("kernlab")
library("MetaboAnalystR")
library(dplyr)## pipeline function

# "2.2.3 github 来源的包"

# git_package <- c("sneumann/xcms","jtilly/matchingR")
# need_to_store <- c()
# for( each_git_package in 1:length(git_package)){### 先找出还没有安装的包
#      if(each_git_package %in% installed.packages() ==F ){
#           need_to_store <- c(need_to_store,each_git_package)
#           }}
# if(length(each_git_package)!=0){### 如果找出了还没有安装的包,那么进行安装
#      ## 首先 读取来源文件
#      #主要应对了 原版 install_github函数 不定期出现的无法正常安装问题
#      #覆盖了 devtool 中的大家提到的传统 install_github函数, 使得中国人身处墙内,也能够方便地下载到github上面的R包
#      source("http://jtilly.io/install_github/install_github.R")### 非常重要: 提供了一个新的解决方案,
#      for( each_package in need_to_store){
#           install_github(each_package)
#      }
#      }





#####################################################################
#### Part Three 包的加载    #########################################
#####################################################################


# 3.0 新引入的尚未经过排序去重的包

for( each_package in packages_needed_to_be_checked){##### 批量加载 普通包的方法
  eval(paste("library(",each_package,")",sep=""))
}
library("PMCMR")

package_for_metabolite <- c("stringr","stringi","Rserve", "ellipse", "scatterplot3d", "Cairo", "randomForest", "caTools", "e1071", "som", "impute", "pcaMethods", "RJSONIO", "ROCR", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "pheatmap", "SSPA", "sva", "Rcpp", "pROC", "data.table", "limma", "car", "fitdistrplus", "lars", "Hmisc", "magrittr", "methods", "xtable", "pls", "caret", "lattice", "igraph", "gplots", "KEGGgraph", "reshape", "RColorBrewer", "tibble", "siggenes","LMGene","Biobase","httpuv")


for( each_package in package_for_metabolite){##### 批量加载生物信息的包的方法
  eval(paste("library(",each_package,")"))  
}

# library(Biobase)
library(LMGene)
"if LMGene 报错, 释放上面的行(里面不加引号)"

for( each_package in metr_pkgs){##### 批量加载生物信息的包的方法
  eval(paste("library(",each_package,")",sep=""))  
}
####3.2 library 载入包 ### 按照 字母顺序排序，可以通过EXL来进行排序和查重,########


###############################

#####################   *** ￥ ***  *** ￥ ***   #####################
##    所有的安装包都以加载完成                                  ######
##    v 1.0                                                     ######
##   zhoudi 2018-5-2                                            ######
##################     END  --- _____ ---  END      ##################


print("新包安装就位")




############### 自定义函数区
####自定义函数####
"搜索function,看看有什么自定义函数"
# getwd()

# " 导言区 "
#待完成工作： 寻找将不同表格批量输出到同一个exl的方法。
######函数的标识，首尾标识
########### function  demo function  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
##function overview
##function trunc
#######  function ending    #########￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥




########函数1 绘图函数  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####################
### for 20180422 魔改的 重新绘制分组点图和折线图  ###############################
draw_a_pic <- function(data){##将data赋值给 for_fig1.1
  for_fig1.1 <- data
  p1 <- ggplot(for_fig1.1,aes(x=group,y=(median),#group=1,
                              color=REE,
                              shape=REE,
                              # fill=REE,
                              group=REE
  ))
  p1 <- p1+geom_errorbar(aes(ymin=q25,ymax=q75),width=0.3,size=1,position=position_dodge(0.6))+#position=position_dodge(0.6)) 其实是让各个元素都错开一点的函数。
    geom_line(position=position_dodge(0.6),size=1.5)+
    geom_point(colour="black",
               size=4,
               position=position_dodge(0.6),
               fill="white")+#shape=REE
    # scale_y_continuous(breaks = c(0.000,0.002,0.004,0.006))+#调整data format 有效数字位数，采用后面接labels=c("")的形式调整，可以使数据更加的符合要求
    ylab(NULL)+
    xlab(NULL)+
    # xlab(NULL)+ylab("Concentration of REEs (ng/ml)")+# xlab("levels of Cr in periphery blood")+ylab("Concentration of REEs in periphery blood")+
    guides(fill=guide_legend(title="fjlkjffj"))+
    # guide_legend(title ="jjjjjjj")+
    # labs(fill="REEs")+#fill在此为图例的标题。
    geom_text(aes(label=label),#la.nbel 是标签变量名，本来想打label的，打错了，唉（错的也太远了吧，小伙子）
              size=5.3,position=position_dodge(0.6),family="TT",
              vjust=0.25,hjust=-0.39,colour="black")+
    theme(legend.position = c(0.11,0.84),
          legend.title = element_text(face="italic",family="TT",colour="black",size=24),
          legend.text  = element_text(              family="TT",colour="black",size=18),
          legend.background = element_rect(colour = "black",fill="grey100",size=6),#设置背景，用rect语句
          
          
          
          axis.text.x = element_text(family = "TT",
                                     # face="bold",#字体类型的设定语句同基础绘图时的font有点区别
                                     colour = "black",
                                     size = rel(2)),
          axis.text.y = element_text(angle=30,
                                     hjust = 1,
                                     vjust = 1,
                                     family = "TT",
                                     # face="bold",#字体类型的设定语句同基础绘图时的font有点区别
                                     colour = "black",
                                     size = rel(2)))+#rel 为当前基础字体的0.9倍
    scale_shape_manual(values=c(24,21,22,25))
  
  return(p1)
}######################自定义函数1 完结 ####################$4$$$$$$$$￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥#######

###########################自定义函数 2       $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$################
##计算中位数和四分位数
calculate_m_iqr <- function(input_frame){
  group <- input_frame$fac_Cr[1]
  pool_matrix <- NULL
  for(i in 2:ncol(input_frame)){###对于这个矩阵来讲，从2开始遍历一遍
    REE_name <- colnames(input_frame)[i]
    temp_value <- input_frame[,i]###将REE元素的测定结果，暂时赋给临时变量，便于表示
    median   <- median(temp_value) 
    q25      <- quantile(temp_value,c(0.25))
    q75      <- quantile(temp_value,c(0.75))  
    new_vector <- c(REE_name,group,median,q25,q75)
    pool_matrix <- rbind(pool_matrix,new_vector)
  }
  return(pool_matrix)
}
#############自定义函数2 完结##############$$$$$$$$$$$$￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥##########


###########################自定义函数 3       $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$################
## 输入文件名，返回文件所在的工作目录下的绝对路径

return_enough_path <- function(name_or_pattern,detail=T){
  if(detail==T){
    # cat(
    # "function overview 
    # 
    # 1：参数说明：
    # name_or_pattern####输入文件名 ，不要带后缀， 如果偏要带后缀， '.' 要写成 '\\.'
    # detail= T 默认，显示细节信息，也就是你正在看的这一段话，赋为其他值时，不显示细节，也就是说你将看不到这段话了
    # 源代码：
    # filename <- list.files(pattern=name_or_pattern,####输入文件名，不要带后缀， 如果偏要带后缀， '.' 要写成 '\\.'
    # recursive = T,###递归查找子文件夹，让此句为真，就可以系统地层级创建文件夹，使文件放置的更加有条理
    # include.dirs = T,###子文件夹的文件是否再次递归，本函数中，选择是
    # full.names = T###是否返回文件的全路径，还是仅返回部分路径
    # ) ##自定义函数展示函数的细节 detail=T or F, if F ,then the notes will be hidden.
    # "
    # )
    
  }
  filename <- list.files(pattern=name_or_pattern,####输入文件名，不要带后缀， 如果偏要带后缀， "." 要写成 "\\."
                         recursive = T,###递归查找子文件夹，让此句为真，就可以系统地层级创建文件夹，使文件放置的更加有条理
                         include.dirs = T,###子文件夹的文件是否再次递归，本函数中，选择是
                         full.names = T###是否返回文件的全路径，还是仅返回部分路径
  ) ##自定义函数展示函数的细节 detail=T or F, if F ,then the notes will be hidden.
  
  head <- getwd()
  tail <- sub("\\.","",filename)
  filename <- paste(head,tail,sep = "",collapse = "")
  # filename <- gsub("/","\\\\",filename)
  return(filename)
  
}
#############自定义函数3 完结##############$$$$$$$$$$$$￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥##########

#############自定义函数4 ###################################
#取特定位的有效数字，通过 函数取特定位数的有效数字 #####################

## 4.1 准备阶段，仅针对数字的处理
# "反思  该功能的最优解应当是 ：
# 1 先按照要求取一个round，得到初步结果 round(num)，值相等，但小数位数可能不规范的值
# 2 正则表达式截取 含正负号的整数部分 
# 3 通过对  round(num) 进行 先取绝对值  再加0.0001 (小数点后的0等于要保留的位数，这样，末尾的一就不会造成影响了)
# 4 正则表达式截取 小数点到最后的 1  之间的部分作为小数部分
# 5 拼接整数部分和小数部分，完成格式化输出，只需要五步
# 
# "

neat_round <- function(num,digits=3){  " version 5.0 "
  num <- round(num,digits = digits) 
  zheng_shu_bu_fen  <- trunc(num) 
  zheng_shu_char <- sub("([-+]?[0-9]+)\\.[0-9]*",
                        "\\1",num)###利用正则表达式提取整数部分，防止0开头的负小数时，符号被忽略
  #核心工作流程： 先取绝对值再加1,（确保不是0打头），乘以10的有效位数次方——最后需要的全部都跑到了小数点左边（可以被截取）
  # #然后刨除前面整数部分，取后面的小数部分进行衔接。
  new_xiao_shu <-  abs(num-zheng_shu_bu_fen)  ### 用原数值减整数部分直接得到小数部分,取绝对值,保障是正数. 且首位是0
  new_xiao_shu <- new_xiao_shu+0.1^(digits+1) ### 保留 n位 小数, 就加一个小数点后 n个0,1个1的数,使前面的零强制显示  
  xiao_shu_bu_fen <- substr(new_xiao_shu,3,2+digits)
  final <- paste(zheng_shu_char,".",xiao_shu_bu_fen,sep = "")
  return(final)
}





discarded_neat_round <- function(num,digits){
  num <- round(num,digits = digits) 
  zheng_shu_bu_fen  <- trunc(num) 
  zheng_shu_char <- sub("([-+]?[0-9]+)\\.[0-9]*",
                        "\\1",num)###利用正则表达式提取整数部分，防止0开头的负小数时，符号被忽略
  #核心工作流程： 先取绝对值再加1,（确保不是0打头），乘以10的有效位数次方——最后需要的全部都跑到了小数点左边（可以被截取）
  # #然后刨除前面整数部分，取后面的小数部分进行衔接。
  bigger_num <- abs(num)+1 
  bigger_trunk <- abs(zheng_shu_bu_fen)+1
  #先截取整数部分，然后，截取右边的digits位，作为小数部分
  xiao_shu_to_be_cut <-   as.character(trunc(bigger_num*(10^digits),0))  ##带有整数头的小数部分
  total_char         <-   nchar(xiao_shu_to_be_cut)  ###总的字符长度
  trunk_char        <-  nchar(bigger_trunk) ###前面整数部分的长度
  xiao_shu_bu_fen <-  substr(xiao_shu_to_be_cut,trunk_char+1,total_char)
  cat(num,"\n",
      bigger_num,"bigger_num \n",
      bigger_trunk,"bigger_trunk \n",
      bigger_num*(10^digits),"\n",
      trunc(bigger_num*(10^digits),0),"\n",
      xiao_shu_to_be_cut,"\n",
      xiao_shu_bu_fen,"\n"
  )
  final <- paste(zheng_shu_char,".",xiao_shu_bu_fen,sep = "")
  return(final)
}






discard_neat_round <- function(num,digits=3){ ### A function with misable life, which was definately caused by his stupid deveoper who can't figure out good and simple codes~
  num <- round(num,digits)###上来就四舍五入，这样免得后面麻烦
  ##之后就只管补充0占位就好了
  front_zero <- ""
  
  golden_value <- round(num,digits)###将金标准的保存
  zheng_shu_bu_fen <- trunc(num/1)
  xiao_shu_bu_fen <- mod(num,1)# 通过正则表达式提取
  
  
  if(grepl("[-+]?[0-9]+\\.0+",num)==T){###如果小数部分的前面有0，需要提取出来，作为前占位0，没有的话，就这么地了。
    front_zero <- gsub("[-+]?[0-9]+\\.(0+)[0-9]+","\\1",num)
  }
  xiao_shu_bu_fen <- xiao_shu_bu_fen*(10^digits)
  xiao_shu_bu_fen <- as.character(round(xiao_shu_bu_fen,0))
  xiao_shu_bu_fen <- paste(front_zero,xiao_shu_bu_fen,sep = "")
  if(grepl("\\.",num)==F){### if there isn't any 小数部分  如果是整数，需要单独计算需要的补零数，并且增加一个小数点
    buweishu <- digits
    zeros <- rep("0",buweishu)
    xiao_shu_bu_fen <- paste(zeros,sep="",collapse = "")
  }
  num <- as.character(paste(zheng_shu_bu_fen,".",xiao_shu_bu_fen,sep=""))
  temp_result <- 1
  temp_result <- paste(golden_value,"->",num,"=",zheng_shu_bu_fen,"+",xiao_shu_bu_fen,"\n",
                       "front_zero=",front_zero,"buweishu=",buweishu,"\n",sep=" ")
  return(temp_result)
  # return(num)
}

##注 通过 自动比较自编函数和金标准，检查功能能否被正确执行


# "函数 4.2  ——关键在于考虑正负号 + 考虑小数点 + 一定位数的有效数字保留"
### 小数模式，保留一定位数的小数 ###
set_digits <- function(vector,digits=3){
  cat("v 3.0, test \n")
  final <- c()
  ### 将 数据拆分为三部分： 头  身（数字部分）  尾，分别提取，然后再进行拼接_ 否则无法对数字部分进行四舍五入
  head <- ""
  num <- ""
  tail <- ""
  for(i in 1:length(vector)){
    if(is.na(as.numeric(vector[i]))==F){##如果是纯数字(转化为纯数字后非空），按照数字方法处理
      # cat(i,"\n") ##标识纯数字
      final[i] <- neat_round(as.numeric(vector[i]),digits = digits)  ##数字可以直接 用round 函数
    }
    if(is.na(as.numeric(vector[i]))==T){
      ###如果并不是纯数字，混杂有符号的话，那么就按照另一种方案进行处理
      head <- gsub("([-+<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\1",vector[i])
      num <- gsub("([-+<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\2",vector[i])
      tail <- gsub("([-+<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\3",vector[i])
      final[i] <- paste(head,neat_round(as.numeric(num),digits = digits),tail,sep="")
      cat(num[i],"-->",neat_round(as.numeric(num)),"\n")
    }
  }
  return(final)
}#############小数点后固定位数模式

## 检测效果

# neat_round(0.07,5)
# ### 检测函数的效果 
# for( i in c(-1000:-1,1:1000)){
#      temp <- 10/i
#      k <- neat_round(temp,digits=5)
#      kkk <- round(temp,5)
#      # cat(i, k,"   --->",kkk,"\n")
#      if(kkk != as.numeric(k)){cat(i,"you stupid","\n")} ### 如果结果不对,会报错
#      
#      }


# ## 化学中的有效数字模式###
# set_digits <- function(vector,digits=3){
#   final <- c()
#   ### 将 数据拆分为三部分： 头  身（数字部分）  尾，分别提取，然后再进行拼接_ 否则无法对数字部分进行四舍五入 
#   head <- rep("", length(vector))
#    num <- rep("", length(vector))
#   tail <- rep("", length(vector))
#   for(i in 1:length(vector)){
#     if(is.na(as.numeric(vector[i]))==F){##如果是纯数字(转化为纯数字后非空），按照数字方法处理
#       cat(i,"\n")
#       final[i] <- signif(as.numeric(vector[i]),digits = digits)  ##数字可以直接 用round 函数
#     } 
#     if(is.na(as.numeric(vector[i]))==T){
#       ###如果并不是纯数字，混杂有符号的话，那么就按照另一种方案进行处理
#         head[i] <- gsub("([+-<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\1",vector[i])
#          num[i] <- gsub("([+-<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\2",vector[i])
#         tail[i] <- gsub("([+-<>=]?)([0-9]+.[0-9]+)([*]{0,3})","\\3",vector[i])
#        final[i] <- paste(head[i],signif(as.numeric(num[i]),digits = digits),tail[i],sep="")
#     }
#   }
#   return(final)
# }
#####################################################  函数4 终于结束了？ ############################

## zhong文又他妈抽风了,淦!!
### 调整不好,不知是哪里的锅.
# stri_enc_toutf8("   *** ￥ ***  *** ￥ ***   ")
# 
# str_pad(stri_enc_toutf8("   Start  +++ $$$$$ +++  Start    "),70,side="right",pad = "#")
# str_pad(stri_enc_toutf8("  此函数通过遍历列表,将非空列表转赋给temp    "),70,side="right",pad = "#")
# 
# str_pad("  此函数通过遍历列表,将非空列表转赋给temp    ",70,side="right",pad = "#")

##########根据某些字符 生成漂亮的注释块######################
muti_notes <- function(word_vector){
  
  head <- str_pad("   Start  +++ $$$$$ +++  Start    ",70,side="both",pad = "#")
  body<- c()
  tail <- str_pad("     END  --- _____ ---  END      ",70,side="both",pad = "#")
  
  middle_line <- str_pad("  *** __ ***  *** __ ***   ",70,side="both",pad="#")
  for( i in 1:length(word_vector)){
    temp <- paste("##","   ",word_vector[i],"    ",sep = "")
    temp <- str_pad(temp,60,side = "right",pad = " ")
    temp <- str_pad(temp,70,side = "both",pad = "#")
    body <- c(body,temp)### join body by temp line by line
  }
  final <- c(head,body,middle_line,"\n\n",middle_line,body,tail)
  cat(paste(final),sep="\n")
  # return(final)
}
# demo：
# muti_notes(c("part 4","这是啥","没啥用的一个函数","你丫的咋这么闲"))







##################   Start  +++ $$$$$ +++  Start    ##################
##   data cleanning Function for metabolome analysis            ######
##   剔除随机缺失过多的变量                                     ######
##   保留的变量用LOD/sqart(2)替代                               ######
##   version 1.0          +###将负数\零 设定成NA                ######
##   zhoudi 2018-5-2                                            ######
#####################   *** ￥ ***  *** ￥ ***   #####################

data_cleanning <- function(dataset, ##数据集来源
                           start_col=1,##设定数据开始的列
                           start_row=1,##设定数据开始的行
                           id_for_control, ##对照组的样本id 如c(1:12)表示前十二行是对照组,其余的是实验组
                           detected_low_limit=0.60,##设定最低检出率,低于此检出率且组间差异小的,剔除
                           permitted_detected_ratio=2, ## 虽然检出率低,但是如果  1 组间缺失率差异大,
                           permitted_concentration_ratio=2##  或                 2 组间浓度差异大的,保留
){
  
  for(i in start_row:nrow(dataset)){
    for(j in start_col:ncol(dataset)){
      dataset[i,j] <-  as.numeric(dataset[i,j])
      if(is.na(dataset[i,j])==F){ #当非NA时,判断是否是负数或零,负数或零也当成NA计算.
        if(dataset[i,j]<=0){dataset[i,j] <- NA ###将负数\零 设定成NA}
        }
      }
    }
  }
  
  delete_col <- c()  #######存储将被删除的变量的列号。
  for(i in start_col:ncol(dataset)){
    length(dataset[,i][is.na(dataset[,i])]  )
    total_detected_ratio       <-  (nrow(dataset)- sum(is.na(dataset[,i])))/nrow(dataset)  # (总数-NA)/总数
    
    inter_group_undetected_ratio <-      (sum(is.na(dataset[-id_for_control,i]))/nrow(dataset[-id_for_control,])) / ( sum(is.na(dataset[id_for_control,i]))/nrow(dataset[id_for_control,]))
    ##组间 未检出率比
    mean_control    <- mean(dataset[ id_for_control,i],na.rm = T)
    mean_experiment <- mean(dataset[-id_for_control,i],na.rm = T)
    min_control    <- min(dataset[ id_for_control,i],na.rm = T)
    min_experiment <- min(dataset[-id_for_control,i],na.rm = T)
    
    if(total_detected_ratio<detected_low_limit & ##检出数量 比 总数量 ,检出率 小于
       inter_group_undetected_ratio<permitted_detected_ratio& ## 1 组间 未 检出率差异不大,
       inter_group_undetected_ratio  >1/permitted_detected_ratio& ## 1 组间 未 检出率差异不大,
       mean_control/mean_experiment <  permitted_concentration_ratio&
       mean_control/mean_experiment >  1/permitted_concentration_ratio){
      delete_col <- c(delete_col,i)
      next
    }else{ ## 若不符合上述排出条件,不删除,进行数据的替换. 用相应组别的平均值进行替换:注,对于多个剂量组的,理论上来讲最好通过
      dataset[ id_for_control,i][is.na(dataset[ id_for_control,i])==T] <- min_control    #用 对照组 的均值替换对照组 的 未检出
      dataset[-id_for_control,i][is.na(dataset[-id_for_control,i])==T] <- min_experiment #用 实验组 的均值替换实验组 的 未检出
    }
  }
  
  # 检查替换NA的效果，应当等于0
  if(length(delete_col)==0){cat(" every column qualified and none was deleted\n \n
                                num of NA : ",sum(is.na(dataset)),
                                "num of minor and zero: ",
                                sum(dataset<=0,na.rm = T),
                                "\n")
    return(dataset)}###如果一个需要删的都没有,直接返回 修改后的数据集
  else{ cat( colnames(dataset)[delete_col], "\n 
             was excluded because the low detection. \n 
             in addition, the missing data seems to have no relation with group")
    return(dataset[,-delete_col])}      ###如果有需要删除的行,        那么就剔除
}
#####################   *** ￥ ***  *** ￥ ***   #####################
##   data cleanning Function for metabolome analysis            ######
##   剔除随机缺失过多的变量                                     ######
##   保留的变量用LOD/sqart(2)替代                               ######
##   version 1.0                                                ######
##   zhoudi 2018-5-2                                            ######
##################     END  --- _____ ---  END      ##################    


##################   Start  +++ $$$$$ +++  Start    ##################
##    $$ 无用系列 002 $$                                        ######
##    假装一直在运行的伪代码                                    ######
##    只是为了应付消防检查                                      ######
##    你是得有多无聊啊,兄dei                                    ######
##         核心通过sys.sleep()实现                              ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-11                        ######
#####################   *** ￥ ***  *** ￥ ***   #####################
processing_raw_data <- function(unused_parameter){
  for(i in 1:100000){
    cat(" processing line ",i,"...\n")
    Sys.sleep(19.2) ### 让系统时间延迟.2秒, 使得程序延缓.2秒执行
    cat(" line ",i," calculated successfully \n")
    Sys.sleep(6)
    cat(" device status = ", sample(1:4,1,replace=F),"\n\n")
  }
}


#####################   *** ￥ ***  *** ￥ ***   #####################
##    $$ 无用系列 002 $$                                        ######
##    假装一直在运行的伪代码                                    ######
##    只是为了应付消防检查                                      ######
##    你是得有多无聊啊,兄dei                                    ######
##         核心通过sys.sleep()实现                              ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-11                        ######
##################     END  --- _____ ---  END      ##################





##################   Start  +++ $$$$$ +++  Start    ##################
##    $$ 缺失值与异常值 002 $$                                  ######
##    寻找单因素分组下的异常值                                  ######
##    对正态数据,采用均值±3倍标准差法,非正太数据                ######
##     对非正态数据,用上下四分位数加或减1.5倍iqr计算            ######
##        返回检验的结果                                        ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-15                        ######
#####################   *** ￥ ***  *** ￥ ***   #####################



outliner_dectection <- function(data_to_test_outliner){
  ### 计算基本信息
  q1 <- quantile(data_to_test_outliner,0.25,na.rm=T)
  q2 <- quantile(data_to_test_outliner,0.75,na.rm=T)
  iqr <- (q2-q1)
  mean_of_data <-   mean(as.numeric(data_to_test_outliner),na.rm = TRUE)
  sd_of_data   <-     sd(as.numeric(data_to_test_outliner),na.rm = TRUE)
  if(sd(as.numeric(data_to_test_outliner),na.rm=T)==0){ ### 如果所有值相等,正态性检验会报错,因此,将此种情况手动调过
    cat("所有的数值都相等")
    return(rep(T,length(data_to_test_outliner)))
  }
  if(shapiro.test(data_to_test_outliner)$p.value>0.05){# 正态性状态下的检验
    low <- mean_of_data-3*sd_of_data
    high <- mean_of_data+3*sd_of_data
  }else{#非正态性的话,用另一种策略
    low <- q1-1.5*iqr
    high <- q2+1.5*iqr
  }
  wether_outliner <-  data_to_test_outliner>=high | data_to_test_outliner<=low### 储存判定结果,异常值会显示为T,后续考虑删除.
  return(wether_outliner)
}

#####################   *** ￥ ***  *** ￥ ***   #####################
##    $$ 缺失值与异常值 002 $$                                  ######
##    寻找单因素分组下的异常值                                  ######
##    对正态数据,采用均值±3倍标准差法,非正太数据                ######
##     对非正态数据,用上下四分位数加或减1.5倍iqr计算            ######
##        返回检验的结果                                        ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-15                        ######
##################     END  --- _____ ---  END      ##################

##################   Start  +++ $$$$$ +++  Start    ##################
##    $$ 缺失值与异常值 003 $$                                  ######
##    处理列表对象,并将其中的空列表去除                         ######
##    将其中的空列表去除有点麻烦                                ######
##     此函数通过遍历列表,将非空列表转赋给temp                  ######
##        然后再反过来赋值给原列表,完成去空                     ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-15                        ######
#####################   *** ￥ ***  *** ￥ ***   #####################

delete_empty_list <- function(ori_list){
  new_list <- list()
  j=1
  for(i in 1:length(ori_list)){
    if(nrow(ori_list[[i]])>0){
      new_list[[j]] <- ori_list[[i]]
      j <- j+1
    }  
  }
  return(new_list)
}

#####################   *** ￥ ***  *** ￥ ***   #####################
##    $$ 缺失值与异常值 003 $$                                  ######
##    处理列表对象时                                            ######
##    将其中的空列表去除有点麻烦                                ######
##     此函数通过遍历列表,将非空列表转赋给temp                  ######
##        然后再反过来赋值给原列表,完成去空                     ######
##                                                              ######
##    v 1.0            zhoudi 2018-05-15                        ######
##################     END  --- _____ ---  END      ##################


















## 准备阶段, 自定义函数 组间比较并返回p值
compare_mutile_groups <-   function(data, #### 要比较的数据,是一个向量形式的单行或单列数据.
                                    group_vector, 
                                    ###输入分组的情况,如 c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,4,4,4,4,4) 表示 分组情况 
                                    group_order### 设定分组的顺序,第一个对照组,后面的建议计量由低到高,格式形如: c("control","low","middle","high") 
){
  # 输入数据,分组
  
  ## 根据分组,将分组变量转换成因子,且第一组在前.
  group_vector <- factor(group_vector,labels = group_order,levels = group_order)
  
  ### 合并去NA等
  data_combined <- data.frame(data=data,group_vector=group_vector)
  data_combined <- na.omit(data_combined)
  # bottom <-  data.frame(data=c(1,2,3),group_vector="low bee")### 临时测试多组比较时,方差齐性检验的效果和输出提取.结论: 返回的是总的p值.
  # data_combined <- rbind(data_combined,bottom)
  
  ##求p值,正态性检验,并且进行后续的分情况讨论.
  
  " 统计 非空的 数据个数, 如果为唯一值的话,就跳过正态性检验 "
  count_all_unique <- ( unique(data) %>% # 取唯一值
                          is.na( .)  ==F) %>% # 获取非 空值
    sum(.,na.rm = T) ## 计算总共 的非零个数
  
  if( count_all_unique==1){ ## 如果仅有一个 唯一值,那么不需要进行正态性检验,也不需要进行 方差齐性检验 . 直接让p值等于1...
    p_for_normal <- 1
    p_for_var    <- 1
  }else{
    p_for_normal <- shapiro.test(data)$p.value  
    
    ### levene 法 进行方差齐性检验.
    var_test <- leveneTest(y=data_combined[,"data"], ### 依次指定 要检测的值和 对应的分组变量. 进行方差齐性检验
                           group=data_combined[,"group_vector"],
                           # location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
                           # kruskal.test=FALSE,
                           # correction.method=c("none","correction.factor","zero.removal","zero.correction")
                           bootstrap = 1000, num.bootstrap=1000)#设定迭代和迭代次数
    
    p_for_var <-  var_test$`Pr(>F)`[1]
  }
  
  
  
  
  
  if(p_for_normal>=0.05 & p_for_var>=0.05){### 如果 正态性和方差齐性都满足,那么再进行接下来的检验.
    # 参数检验, 多组比一组
    method="parameter"
    p_matrix <- DunnettTest(data ~ group_vector,data=data_combined,
                            ## data ~ group_vector,data=data_combined,写法一, 在一个数据集内的数据
                            #### x= , g= 写法二, 分别指定检验的值,和分组的变量.
                            # alternative = c("two.sided", "greater", "less"),# 检验的方向
                            ### Dunnett 法就自带了矫正p值得方法,(通过测试,1设定 p值调整方法为none时, 4组比较和4组中的3组(亚组)比较返回的p值不同.(说明有对比较数进行调节)
                            #2 改变p值得矫正方法, p值不发生改变,说明此方法用的是默认的矫正方法,不需要设定.
                            control=group_order[1],## 选取对照 的顺序, 此处设定第1组.  格式如: control="1",
                            conf.level=0.95)### 置信区间.
    p_to_return <- p_matrix[[1]][,"pval"]## 返回组间比较的p值.
    # 将结果提取,p值输出.
  }else{#### 若正态性不满足,采用非参数检验
    ## 非参数检验,多组比一组
    method <- "nonparameter"
    
    temp <- dunn.test.control(   ### 此 函数基于 的是  Kruskal-Wallis-Test 检验。
      # data~group_vector,data=data_combined,
      ### 写法一: data~group_vector,data=data_combined,
      x=data_combined[,"data"],g=data_combined[,"group_vector"],### 写法二:x=weight,g=group,
      # p.adjust="none"
      # p.adjust="bonferroni"
      
      p.adjust="bonferroni"
    )
    p_to_return <- t(temp$p.value)
    # 将结果提取,p值输出.
  }
  final <- list()
  final[[1]] <- p_to_return
  final[[2]] <- method
  names(final) <- c("p value","method to test in groups")
  return(final)
}### 利用final [[1]]提取出p值.


##### 寻标函数集合#####

# 自定义多组间比较函数,和对代谢物 进行 寻标
# muti_notes(c("寻标法","针对个体间差异大的数据,采用挑选内部物质校正","获取各个物质相对表达量的方法",
#              "v 2.0  0523 by zhoudi"))
##################   Start  +++ $$$$$ +++  Start    ##################
#######   寻标法                                                 #####
#######   针对个体间差异大的数据,采用挑选内部物质校正            #####
#######   获取各个物质相对表达量的方法                           #####
#######   v 2.0  0523 by zhoudi                                  #####
#####################  *** __ ***  *** __ ***   ######################



### 为何 格格不入的乙酸会被选出来? 有待进一步验证. 看图,乙酸的浓度标准差很小.
## 海量代谢物时,同样面临此问题,如何平衡各种代谢物的浓度  如不同数量级的代谢物,  不同浓度标准差 的代谢物 
# 计算各个样本不同代谢物校正后的总cv,分组cv 和校正后的水平. #可以在获取了方案后, 单独执行.



## 准备阶段, 自定义函数 组间比较并返回p值
compare_mutile_groups <-   function(data, #### 要比较的数据,是一个向量形式的单行或单列数据.
                                    group_vector, 
                                    ###输入分组的情况,如 c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,4,4,4,4,4) 表示 分组情况 
                                    group_order### 设定分组的顺序,第一个对照组,后面的建议计量由低到高,格式形如: c("control","low","middle","high") 
){
  # 输入数据,分组
  
  ## 根据分组,将分组变量转换成因子,且第一组在前.
  group_vector <- factor(group_vector,labels = group_order,levels = group_order)
  
  ### 合并去NA等
  data_combined <- data.frame(data=data,group_vector=group_vector)
  data_combined <- na.omit(data_combined)
  # bottom <-  data.frame(data=c(1,2,3),group_vector="low bee")### 临时测试多组比较时,方差齐性检验的效果和输出提取.结论: 返回的是总的p值.
  # data_combined <- rbind(data_combined,bottom)
  
  ##求p值,正态性检验,并且进行后续的分情况讨论.
  p_for_normal <- shapiro.test(data)$p.value
  
  
  
  ### levene 法 进行方差齐性检验.
  var_test <- leveneTest(y=data_combined[,"data"], ### 依次指定 要检测的值和 对应的分组变量. 进行方差齐性检验
                         group=data_combined[,"group_vector"],
                         # location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
                         # kruskal.test=FALSE,
                         # correction.method=c("none","correction.factor","zero.removal","zero.correction")
                         bootstrap = 1000, num.bootstrap=1000)#设定迭代和迭代次数
  
  p_for_var <-  var_test$`Pr(>F)`[1]
  
  
  
  if(p_for_normal>=0.05&p_for_var>=0.05){### 如果 正态性和方差齐性都满足,那么再进行接下来的检验.
    # 参数检验, 多组比一组
    method="parameter"
    
    set.seed(19930618)## 设定 生日为随机数种子,使得结果具有可重复性...
    p_matrix <- DunnettTest(data ~ group_vector,data=data_combined,
                            ## data ~ group_vector,data=data_combined,写法一, 在一个数据集内的数据
                            #### x= , g= 写法二, 分别指定检验的值,和分组的变量.
                            # alternative = c("two.sided", "greater", "less"),# 检验的方向
                            ### Dunnett 法就自带了矫正p值得方法,(通过测试,1设定 p值调整方法为none时, 4组比较和4组中的3组(亚组)比较返回的p值不同.(说明有对比较数进行调节)
                            #2 改变p值得矫正方法, p值不发生改变,说明此方法用的是默认的矫正方法,不需要设定.
                            control=group_order[1],## 选取对照 的顺序, 此处设定第1组.  格式如: control="1",
                            conf.level=0.95)### 置信区间.
    p_to_return <- p_matrix[[1]][,"pval"]## 返回组间比较的p值.
    # 将结果提取,p值输出.
  }else{#### 若正态性不满足,采用非参数检验
    ## 非参数检验,多组比一组
    method <- "nonparameter"
    set.seed(19930618)## 设定 生日为随机数种子,使得结果具有可重复性...
    temp <- dunn.test.control(
      # data~group_vector,data=data_combined,
      ### 写法一: data~group_vector,data=data_combined,
      x=data_combined[,"data"],g=data_combined[,"group_vector"],### 写法二:x=weight,g=group,
      # p.adjust="none"
      p.adjust="bonferroni"
    )
    p_to_return <- t(temp$p.value)
    # 将结果提取,p值输出.
  }
  final <- list()
  final[[1]] <- p_to_return
  final[[2]] <- method
  names(final) <- c("p value","method to test in groups")
  return(final)
}### 利用final [[1]]提取出p值.


## 自定义函数, 寻标主函数!!


find_the_standed <- function(data,### 设定用于筛选的数据集,注意行名是样本名,不同列代表不同的代谢物
                             literation=50,#设定迭代次数
                             # row_num_for_control=c(1:6),  #对照组的行序号,后来改成输入分组信息和各组的分组顺序.
                             group_vector,
                             group_order,
                             p_limit=0.05,#设定组间的排除标准
                             cv_limit=0.2){#设定cv 的限值  
  #"第一部分, 先进行初步循环迭代,通过剔除组间差异大的变量,\n#达到保留组间表达差异小(表达水平保守)的变量的目的\n"
  # 变量作用说明:
  #each_literation 表示循环迭代进行的 次数
  #extract_variable 是列表 储存每次迭代后得到的结果. \
  #row_num 是小循环中表明行的变量
  # ratiomatrix 是矩阵除以 校正用内标和后获得的矩阵.
  
  group_vector <- factor(group_vector,levels=group_order,labels=group_order)
  
  
  for(each_literation in 1:literation){ ####迭代次数 20
    if(each_literation==1){
      extract_variable <- list()
      ori_data <- data ### 保存一个初始数据集
    }######初始化 用来保存提取变量名的列表
    
    
    
    total <- apply(data,1,sum)##initiallization!   计算行(每个样本)的和 , 每次循环data会改变,因此total也会不断改变
    ratio_matrix <- data/total## 通过  除以各行的汇总值,
    
    
    for( col_num in 1:ncol(ratio_matrix)){
      if(col_num==1){ 
        mean_cv <- 0  ############初始化
        
        cv_matrix_by_group <- NULL ## 用来储存 分组统计的cv值.
        kick_out <- rep(F,ncol(ratio_matrix)) }# 循环结束时将其中的变量踢出 为T时踢出,为F时,不踢出
      
      
      temp <- ratio_matrix[,col_num]## 逐列判断标志物 是否 可以作为内标 保留
      
      
      # 计算cv和平均 cv
      
      cv <- abs((sd(temp,na.rm = T)/mean(temp,na.rm = T) ))
      mean_cv <- mean(mean_cv*(col_num-1)+cv)/col_num  ## 计算出累积到此时的 平均 cv
      
      
      # 计算各组的组内 cv.
      # temp <- scfa_data[,5]
      each_cv_by_group <- by(data = temp,### 利用by函数,计算各个组的平均cv.
                             INDICES = group_vector,
                             function(each_part) {abs(sd(each_part,na.rm=T)/mean(each_part,na.rm=T))})
      cv_as_vector <-NULL### 用来储存每一列的各个 组的cv结果
      for(i in 1:length(each_cv_by_group)){cv_as_vector <- cbind(cv_as_vector,each_cv_by_group[[i]])}### 将 by函数的结果暂存到cv_as_vector中### 将列表cv 赋值成为向量.赋值出来,便于后续输出.
      cv_matrix_by_group <- rbind(cv_matrix_by_group,cv_as_vector) ### 将 组间cv记录.整理成一个矩阵,每一列是一个组的组内cv值
      #然后对这个矩阵 按照列来求 平均值,就得到了组内cv的均值.
      
      compare <- compare_mutile_groups(data=temp,
                                       ### 调用自定义的组间比较函数, 返回一个列表,取表一的值作为p值,表2的值表示是否服从正态分布. 
                                       #### 要比较的数据,是一个向量形式的单行或单列数据.
                                       group_vector=group_vector, 
                                       ###输入分组的情况,如 c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,4,4,4,4,4) 表示 分组情况 
                                       group_order=group_order### 设定分组的顺序,第一个对照组,后面的建议计量由低到高,格式形如: c("control","low","middle","high") 
      )
      p_vector <- compare[[1]]### 将结果中的p值提取出来
      p <- min(p_vector) ## 取最小的p值,作为检验 组间是否有差异的 p值
      
      if(cv>(cv_limit/(each_literation^0.10 ))|p <p_limit)           ###当CV 大于10% 或者  组件有显著差异 p<0.05 时， 考虑剔除该变量 
      { kick_out[col_num] <- T  }
    }
    
    
    
    
    
    
    #### 循环结束，剔除不达标的变量
    used_this_time <- colnames(data)## 在筛选变量前,将列名取出,之后便可以赋值并且可视化输出了.
    data <- data[,!kick_out]
    
    
    mean_cv_by_group <- apply( cv_matrix_by_group,2,mean)### 计算每组的平均cv
    extract_variable[[each_literation]] <- c( mean_cv,mean_cv_by_group, used_this_time)
    ## 作用于原始数据的话,各变量的平均cv(不分组显示,是一个汇总值)
    # 作用于原始数据的话,各个变量的 分组cv.
    ####  将 各组的平均cv,
    #保留的变量名保存.存在列表中.
    if(
      # ncol(data)==0|
      is.null(colnames(data))==T|each_literation==literation  ){### 当是空集的时候. 输出 不再进行比较.或者达到最大迭代次数时 输出结果
      extract_variable <- unique( extract_variable)### 列表去重
      # return(extract_variable)
      
      ##转化为数据框.
      for(i in 1:length(extract_variable)){  ### 如何将列表输入到一个文件中 : 可以通过xlsx函数, 或者是dput()函数,或者是下面的代码(适用于每个列表仅一行或1列的状况.)
        if(i ==1){ final <-NULL; maxnum <- length(extract_variable[[1]])}### 利用 列表中的第一个的长度进行初始化,本例中,第一个列表的元素是最多的.
        # find_out[[i]][1:4] <- as.numeric(find_out[[i]][1:4])
        final <- rbind(final,extract_variable[[i]])
        length_of_each_part <-  length(extract_variable[[i]])
        if(length_of_each_part<maxnum){
          final[i,(length_of_each_part+1):maxnum] <- ""## 将不够的位置用空补齐.
        }
      }
      
      
      
      colnames(final) <- c( #"组间差异的检验的p值",
        
        "总平均CV",c(paste(group_order,"组的平均cv")),c(paste("采用的标志物_",1:ncol(ori_data))))
      
      return( final)
      
    }## 如果 所有变量都被剔除, 退出循环.
  }
}
#####################  *** __ ***  *** __ ***   ######################
#######   寻标法                                                 #####
#######   针对个体间差异大的数据,采用挑选内部物质校正            #####
#######   获取各个物质相对表达量的方法                           #####
#######   v 2.0  0523 by zhoudi                                  #####
##################     END  --- _____ ---  END      ##################

#3计算不同 内标方案下的 数据集, cv改变
# 与原数据集相关的指标,及可视化相关.
# find_out <- read.xlsx("serch internals using R.xlsx")
# id_num=c(1:6,19:24)
# group_vector

##################   Start  +++ $$$$$ +++  Start    ##################
#######    寻标测试 002                                          #####
#######   输入寻标的结果数据集                                   #####
#######   指定开始的列数                                         #####
#######   返回数据集的各个变量的分组cv值                         #####
#######   v 1.0  zhoudi 0525 2018                                #####
#####################  *** __ ***  *** __ ***   ######################

show_cv_step_by_step <- function( raw_data=scfa_data[,-c(1:3)],### 原始数据
                                  result=find_out,# 第一次寻标处理的结果
                                  group_vector,
                                  id_num=c(1:6,19:24),### 原始数据中每行的id, 只要是唯一值就行.
                                  group_order
                                  ##start_col=4### 从第几列开始, 可以设定成自动读取的,但是太麻烦了,而且容易出现错误,因此先不考虑,貌似是不需要的,因为 该语句用colnames(raw_data) %in% extract_var  来判断是否提取代谢物,后面的范围多了一点也没有关系,不影响判定结果;.
){
  ### 读取数据,解析每次提取出的变量####
  
  internal_stand_matrix <- result### 储存前面返回的提取内标结果.## 此步骤 不再必要.懒得改了而已,避免产生新bug
  n_for_loop <- nrow(internal_stand_matrix)## 根据数据的行数,确定循环的次数
  
  ###提取出 循环的信息, 并且每次循环生成一个向量,储存 每次用来做内标的值的按行汇总的和
  #(第一次不标准化, 因此首个元素是全1,其余的是按照内标组合后加和的结果)#####
  # 最终的结果用 列表形式的pooled_matrix_in_list 变量储存
  pooled_matrix_in_list <- NULL
  pooled_matrix_in_list[[1]] <- raw_data### 第一个数据集储存 原始数据
  for( each_loop in 1:n_for_loop){### 每次循环,首先使用相应的内标变量,计算一个标准物总浓度,
    #再通过 矩阵除这个总浓度, 得到每个点的标准化值. 得到了每次的矩阵.
    
    ## 读取历次提取的变量, 依据这些变量,将子集选出,进而,计算结果.
    extract_var <- internal_stand_matrix[each_loop,]### 读取该次提取的变量.
    sub_raw_data <- raw_data[,( colnames(raw_data) %in% extract_var  )]
    temp_sum_by_row <- apply(sub_raw_data,1,sum)
    each_ratio <- raw_data/temp_sum_by_row
    pooled_matrix_in_list[[each_loop+1]] <-  each_ratio # 将各个矩阵拼接成为一个列表, 供后面使用########################
  }
  # return(pooled_matrix_in_list)## 含有各个矩阵的列表. # check point
  
  ############### 遍历列表,结合group_vector和group_order变量,分别对其中的每一个矩阵按照 列 , 分组求取 cv值.
  #将结果按照行整理后,保存在一个数据矿中.
  cv_matrix <- NULL ### 储存每一 次 的cv , 每一行就是是一次迭代结果后的各组分变量的平均cv
  
  for(each_list in pooled_matrix_in_list){ #依次分组求cv, 循环变量即是 列表的各个 组成元素
    each_list <- cbind(id_num,group_vector,each_list)
    melted    <- melt(each_list,
                      id.vars = c("id_num","group_vector"),
                      variable.name = "metabolite",
                      value.name = "value")
    rearranged <- dcast(data=melted,
                        formula = id_num ~ metabolite+group_vector
    )
    rearranged <- rearranged[,-1]##剔除第一列,不需要
    
    sd_in_line <- apply(rearranged,2,sd,na.rm=T)
    mean_in_line <- apply(rearranged,2,mean,na.rm=T)
    cv_in_line <- sd_in_line/mean_in_line ### 计算 变异系数.
    cv_matrix <- rbind(cv_matrix,cv_in_line)
    # return(rearranged)
    # return(cv_in_line)
  }
  final <- NULL ### 将 各个数据集和迭代结果的数据框返回
  final[[1]] <-  pooled_matrix_in_list
  final[[2]] <- cv_matrix
  return(final)
  ### 对数据框进行分析处理和可视化.
}

















# "自定义函数调用完成"






###  正在研究中的函数 ####
"新编写的函数"
run_meta <- function(data_in_R,temp_file_path){ #### 将数据集写到某个固定位置,再读出来
  
  write_excel_csv(x=data_in_R,path=temp_file_path)
  mSet<-InitDataObjects("conc", "stat", FALSE)
  
  
  ### 增加一个 读取 可变的数据框, 但是写到固定位置和固定的文件名, 这样就可以从固定的位置读取 文件进行后面的处理和分析了.
  
  # 先将文件写入到一个固定的位置
  # 再将该固定位置的文件读取 出来
  
  mSet<-Read.TextData(mSet,temp_file_path, "rowu", "disc") ### 读取文件 
  
  # # 不需要的 数据前处理
  mSet<-SanityCheckData(mSet) # # 数据核验,必要, 区分 数值和字符串 等
  mSet<-ReplaceMin(mSet); ## 替换最小值？ 处理缺失数据
  #
  # mSet<-Normalization(mSet, "MedianNorm", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ratio=FALSE, ratioNum=20)
  ###  options
  
  # "可选方案 "
  # c("QuantileNorm", "CompNorm","SumNorm","MedianNorm","SpecNorm",
  #   "LogNorm","CrNorm", ##short for cube root 均方根正态化
  #   "MeanCenter", "AutoNorm", "ParetoNorm","RangeNorm")
  
  
  
  mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA) # 正态性 检验
  mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA) # 正态性 情况
  
  # 单因素分析###
  mSet<-PlotHCTree(mSet, "tree_0_", "png", 72, width=NA, "euclidean", "ward.D") ## 热图的聚类树?
  mSet<-PlotHeatMap(mSet, "heatmap_0_", "png", 72, width=NA, "norm", "row", "euclidean", "ward.D","bwm", "overview", T, T, NA, T, F) ## 热图
  
  mSet<-FC.Anal.unpaired(mSet, 2.0, 0)  ## 改变倍数,  非配对数据
  mSet<-PlotFC(mSet, "fc_0_", "png", 72, width=NA) # 倍数改变作图
  mSet<-Ttests.Anal(mSet, F, 0.05, FALSE, TRUE) # t检验结果
  mSet<-PlotTT(mSet, "tt_0_", "png", 72, width=NA) # t检验作图
  mSet<-Volcano.Anal(mSet, FALSE, 2.0, 0, 0.75,F, 0.1, TRUE, "raw") # 火山图
  # mSet<-PlotVolcano(mSet, "volcano_0_",1, "png", 72, width=NA)# 火山图 报错,不知道为了什么
  
  mSet<-PlotCorrHeatMap(mSet, "corr_0_", "png", 72, width=NA, "col", "pearson", "bwm", "overview", F, F, F, 100) ## 相关性热图 代谢物的
  mSet<-PlotCorrHeatMap(mSet, "corr_1_", "png", 72, width=NA, "row", "pearson", "bwm", "overview", F, F, F, 999) ##  相关性热图 样本的
  
  
  ## 多元统计方法###
  # 主成分分析
  mSet<-PCA.Anal(mSet)
  mSet<-PlotPCAPairSummary(mSet, "pca_pair_0_", "png", 72, width=NA, 5) # 参数6: 使用的主成分的个数
  mSet<-PlotPCAScree(mSet, "pca_scree_0_", "png", 72, width=NA, 5)
  mSet<-PlotPCA2DScore(mSet, "pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
  mSet<-PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2,"scatter", 1);
  mSet<-PlotPCABiplot(mSet, "pca_biplot_0_", "png", 72, width=NA, 1,2)
  mSet<-PlotPCA3DScoreImg(mSet, "pca_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
  
  
  ## 偏最小二乘分析
  mSet<-PLSR.Anal(mSet, reg=TRUE)
  mSet<-PlotPLSPairSummary(mSet, "pls_pair_0_", "png", 72, width=NA, 5)
  mSet<-PlotPLS2DScore(mSet, "pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
  mSet<-PlotPLS3DScoreImg(mSet, "pls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
  mSet<-PlotPLSLoading(mSet, "pls_loading_0_", "png", 72, width=NA, 1, 2,"scatter", 1);
  mSet<-PLSDA.CV(mSet, "L",5, "Q2")
  mSet<-PlotPLS.Classification(mSet, "pls_cv_0_", "png", 72, width=NA)
  mSet<-PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)
  mSet<-PLSDA.CV(mSet, "T",5, "Q2")
  mSet<-PlotPLS.Classification(mSet, "pls_cv_1_", "png", 72, width=NA)
  mSet<-PLSDA.Permut(mSet, 400, "bw") ### 采用 bw作为依据进行置换检验
  # mSet<-PLSDA.Permut(mSet, 400, "bw") ### 采用 bw作为依据进行置换检验
  mSet<-PlotPLS.Permutation(mSet, "pls_perm_1_", "png", 72, width=NA)
  # mSet<-PLSDA.Permut(mSet, 400, "accu") ### 采用 准确度 作为依据进行置换检验
  mSet<-PLSDA.Permut(mSet, 400, "accu") ### 采用 准确度 作为依据进行置换检验
  mSet<-PlotPLS.Permutation(mSet, "pls_perm_2_", "png", 72, width=NA)
  
  # 稀疏性偏最小二乘分析
  mSet<-SPLSR.Anal(mSet, 5, 10, "same")
  mSet<-PlotSPLSPairSummary(mSet, "spls_pair_0_", "png", 72, width=NA, 5)
  mSet<-PlotSPLS2DScore(mSet, "spls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
  mSet<-PlotSPLS3DScoreImg(mSet, "spls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
  mSet<-PlotSPLSLoading(mSet, "spls_loading_0_", "png", 72, width=NA, 1,"overview");
  mSet<-PlotSPLSDA.Classification(mSet, "spls_cv_0_", "Mfold", "png", 72, width=NA)
  
  
  # 正交偏最小二乘分析
  mSet<-OPLSR.Anal(mSet, reg=TRUE)
  mSet<-PlotOPLS2DScore(mSet, "opls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
  mSet<-PlotOPLS.Splot(mSet, "opls_splot_0_", "png", 72, width=NA, "all");
  mSet<-PlotOPLS.MDL(mSet, "opls_mdl_0_", "png", 72, width=NA)
  #
  
  # ###
  mSet<-SAM.Anal(mSet, "d.stat", FALSE, TRUE)
  mSet<-PlotSAM.FDR(mSet, 0.3, "sam_view_0_", "png", 72, width=NA)
  mSet<-SetSAMSigMat(mSet, 0.3)
  mSet<-PlotSAM.Cmpd(mSet, "sam_imp_0_", "png", 72, width=NA)
  # mSet<-PlotOPLS.Permutation(mSet, "opls_perm_1_", "png", 72, 400, width=NA)
  mSet<-PlotOPLS.Permutation(mSet, "opls_perm_1_", "png", 72, 400, width=NA)
  
  # mSet<-EBAM.A0.Init(mSet, FALSE, TRUE)
  # mSet<-PlotEBAM.A0(mSet, "ebam_view_0_", "png", 72, width=NA)
  # mSet<-EBAM.Cmpd.Init(mSet, "z.ebam", 0.0, FALSE, TRUE)
  # mSet<-SetEBAMSigMat(mSet, 0.9);
  # mSet<-PlotEBAM.Cmpd(mSet, "ebam_imp_0_", "png", 72, width=NA)
  # 
  # mSet<-PlotHCTree(mSet, "tree_0_", "png", 72, width=NA, "euclidean", "ward.D") ## 热图的聚类树?
  # mSet<-PlotHeatMap(mSet, "heatmap_0_", "png", 72, width=NA, "norm", "row", "euclidean", "ward.D","bwm", "overview", T, T, NA, T, F) ## 热图
  # 
  # mSet<-SOM.Anal(mSet, 1,3,"linear","gaussian")
  # mSet<-PlotSOM(mSet, "som_0_", "png", 72, width=NA)
  # mSet<-Kmeans.Anal(mSet, 3)
  # mSet<-PlotKmeans(mSet, "km_0_", "png", 72, width=NA)
  # mSet<-RF.Anal(mSet, 500,7,1)
  # mSet<-PlotRF.Classify(mSet, "rf_cls_0_", "png", 72, width=NA)
  # mSet<-PlotRF.VIP(mSet, "rf_imp_0_", "png", 72, width=NA)
  # mSet<-PlotRF.Outlier(mSet, "rf_outlier_0_", "png", 72, width=NA)
  # mSet<-RSVM.Anal(mSet, 10)
  # mSet<-PlotRSVM.Classification(mSet, "svm_cls_0_", "png", 72, width=NA)
  # mSet<-PlotRSVM.Cmpd(mSet, "svm_imp_0_", "png", 72, width=NA)
  mSet<-SaveTransformedData(mSet) ### 保存转换后的数据
  # 
  # 
  
  "  "## 增加 自动读取其中的 csv文件, 并对其中的参数进行判断,转存到某个对象中,然后输出到表格中."""
  
  
}


################## 代谢组学 自定义函数 #######
"根据不同 的 筛选原则,  对 Qc样本 不达标的代谢物进行剔除 "


################## 代谢组学 自定义函数 #######

### 用 raw_data作为输入对象 #########
# kick_out_by_RSD_water <- function(Qc=data,cut_off)# 该函数废弃了,更名为: kick_out_by_RSD_data 
"根据不同 的 筛选原则,  对 Qc样本 不达标的代谢物进行剔除 "
kick_out_by_RSD_data <- function(Qc=Qc,cut_off,raw_data=raw_data,water_or_lipid){
  xx <- apply(Qc,2,function(each_col){
    each_col <- as.numeric(each_col)
    RSD=sd(each_col,na.rm=T)/mean(each_col,na.rm=T)
    return(RSD)#### 返回RSD的判断结果
  })
  zz <- apply(Qc,2,function(each_col){#### 符合  cut_off 值 : 如10% 或者 15% 原则的清单
    each_col <- as.numeric(each_col)
    RSD=sd(each_col,na.rm=T)/mean(each_col,na.rm=T)
    RSD<= cut_off  %>%
      return( .)#### 返回RSD的判断结果
  })
  
  test <- data.frame(metabolite_name=meta_name,RSD=xx,wether_qulified=zz)
  write.xlsx(test,file=paste("Qc RSD using",cut_off,water_or_lipid,".xlsx"))
  qualified_rate <-  sum(test$wether_qulified==T)/nrow(test)
  cat(paste(water_or_lipid,"\n    There are"  ,sum(test$wether_qulified==T),  "of Qc with a RSD < ",cut_off,
            " The qualified percentage was",
            100*round(qualified_rate,4),"%")) 
  cutted_data <<- zz  %>% raw_data[,.]
  cat("\n\nnew processed data has been saved in: ",paste("\n                          Qc RSD using",cut_off,water_or_lipid,".xlsx"))
  # return(cutted_data)
}




################## 代谢组学 自定义函数 #######


" all the  packages has loaded!!"