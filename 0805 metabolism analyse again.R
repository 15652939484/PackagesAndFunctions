"重制版代谢组学数据分析
  代码规范: 
  1 采用 注释书写框架, 分割线和注释应当区分开 
  2 文件路径及文件名中不得有中文... 
  3 变量名中也尽量别有中文 
  4 文件应当从指定文件夹中读取,输出到指定文件夹
  5 变量命名规则应当 以小写,下划线间隔,不加s 为第一优先级
      变量名应该避免频繁更改,降低代码维护的难度,增加可读性.
  6 有新增内容时,务必增加到右侧,或者最后,不要在前面插入新的东西.
7 需要调整参数的点, 前面增加checkpoint语句
8 using English instead of Chinese
9 hashtag# Rawdata # Supportive_imformation and # Output before each file inorder to Seperate them in a better way!
10  put every comma(',') before each new line instead of the end of the original line, which'll make a great convenient when reset the parameter"

"
## 给每次分析 搞一个名字 RSD 取值+ 归一化方法
创建一个数据框 存储 每次操作的各种结果
提取 各个 需要检测的值, 汇总放入 该数据矿中
## 
"

options(scipen = 200)
source('E:/Rcools/package_preparing_full_version.R', encoding = 'UTF-8')

# 创建文件并写入文件. 用.\\代指工作目录
# dir.create(".\\abs\\",showWarnings =F ,recursive=T )
# write.csv(1,".\\abs\\东方大厦.csv")
# write.csv(1,".\\abs\\东方大厦.csv")


getwd()
setwd("E:\\Rcools\\0000 metabolism analyse\\")
getwd()


"checkpoint 5"
collect_the_finding <- NULL ## using this data.frame to collect the summary of the result in each method.


"checkpoint 0: define the sheet_names by myself"
sheet_names <- c("Origin90d_meta_liver","Origin90d_lipid_liver"
                 ,"d90_meta_serum","d90_lipid_ serum"
                 ,"d30_meta_f","d30_lipid_f"
                 ,"d90_meta_f","d90_lipid_f")

# sheet_index <- 2

# cut_off <- 0.10

# for(sheet_index in 1:length(sheet_names)){ ############# 最高层级 读取数据, 无需存档, 因为反正这个是挂机任务, 不差 每次读取 exl 这几分钟
# for(sheet_index in 1:length(sheet_names)){ ############# 最高层级 读取数据, 无需存档, 因为反正这个是挂机任务, 不差 每次读取 exl 这几分钟
for(sheet_index in 1:2){ ############# 最高层级 读取数据, 无需存档, 因为反正这个是挂机任务, 不差 每次读取 exl 这几分钟
  "checkpoint 2 设定 RSD的cut off值"
  # for( cut_off in c(0.05,0.10,0.15,0.20)){ #  "checkpoint 2 设定 RSD的cut off值"
  for( cut_off in c(0.10,0.15)){ #  "checkpoint 2 设定 RSD的cut off值"
    data <- read.xlsx("E:\\Rcools\\0000 metabolism analyse\\0805 meta pooled data.xlsx", ## 
                      sheet= sheet_names[sheet_index])

    
    "checkpoint 1"
    # type <- "water" ### type <- "lipid"  OR type <- "water" ##指定 亲水相还是疏水相  # there is no need to sepecified the type anymore!
    # annotation <- sheetnames(i) ## 批量循环 表格中的名字,  data <- read.xlsx("0805 meta pooled data.xlsx",sheet= annotation)  然后进行后续分析.
    annotation <- paste("Output",sheet_names[sheet_index]) ## 批量循环 表格中的名字,  data <- read.xlsx("0805 meta pooled data.xlsx",sheet= annotation)  然后进行后续分析.
    
    
    
    "checkpoint 3 设定分组信息"
    # group_vector <-  c(1:6,19:24)
    group_vector <-  c(rep  ("Control",6 ),                     rep("High",6))
    sample_id    <-  c(paste("Control",1:6,sep="_"),paste("High",1:6,sep="_"))
    nrow(data)
    
    deleted <- (data$Metabolite.name == "Unknown" | (grepl("w/o MS2",data$Metabolite.name)==T)   )
    # data [8419:8427,1:4]
    
    "看看删掉了多少行"
    (data$Metabolite.name == "Unknown" | grepl("w/o MS2",data$Metabolite.name) ==T) %>% sum()
    data <- data[!deleted,]
    head(data)
    # View(data)
    " 获取 用于分析的初始数据集 "
    "checkpoint 2 设定 RSD的cut off值"
    
    
    
    data <- t(data)
    # colnames(data)
    # rownames(data)
    meta_name <- data["INCHIKEY",]
    # meta_name <- data["Alignment.ID",]
    
    Qc <- data[grepl("QC",rownames(data)),]
    colnames(Qc) <- colnames(data)
    Qc <- data.frame(Qc)
    
    
    
    # kick_out_by_RSD_data (Qc=Qc,cut_off=cut_off) 
    # 输出值为 data
    
    kick_out_by_RSD_data (Qc=Qc,
                          cut_off=cut_off,
                          raw_data=data,
                          water_or_lipid=annotation)
    
    # 提取出来了 切分的子集
    # cat("还剩下的符合要求的代谢物数目为",ncol(cutted_data))###
    
    # cutted_data     <- data.frame(t(cutted_data))
    # names_for_cols <- gsub(";.*","",zhizhang$Metabolite.name) ## 暂时不需要获取代谢物名称??
    
    cutted_data <- data.frame(cutted_data,stringsAsFactors = F)
    
    colnames(cutted_data) <- cutted_data["INCHIKEY",]
    " 填补替代变量名 "
    colnames(cutted_data)[colnames(cutted_data) %>% is.na() %>% which()] <- cutted_data[ "Alignment.ID", (colnames(cutted_data) %>% is.na() %>% which())]
    
      paste("Angle_ID instead",colnames(cutted_data) %>% is.na() %>% which() %>% (cutted_data[ "Alignment.ID", . ]) )
    to_be_extracted         <-  grepl("[0-9]",rownames(cutted_data)) & (grepl("[Qq][Cc]",rownames(cutted_data))  == F ) & (grepl("isotopic",rownames(cutted_data))!=T )
    to_be_extracted_with_Qc <- (grepl("[0-9]",rownames(cutted_data)) | (grepl("[Qq][Cc]",rownames(cutted_data))  ==T) ) & (grepl("isotopic",rownames(cutted_data))!=T ) # 包含了Qc样本
    cutted_data_with_Qc <- cutted_data[to_be_extracted_with_Qc ,]
    cutted_data <- cutted_data[to_be_extracted ,]
    # checkpoint 3 # 设定编号
    rownames(cutted_data) <- sample_id ## 手动设定编号 样本信息
    
    cat("Now peak with NA is up to",sum(is.na(sub_data))) 
    
    
    wether_qualified <- apply(cutted_data,2,function(each_col){ is.na(each_col) <- 0 })  ##  substitute NA with zero
    # wether_qualified <- apply(cutted_data,2,function(each_col){ (unique(each_col) %>% length() ) >1  })  ##  值全部相同的列剔除(即使是数值全部一样也没有意义 )
    wether_qualified <- apply(cutted_data,2,function(each_col){ sd(each_col,na.rm=T)!=0 })  ##  值全部相同的列剔除(即使是数值全部一样也没有意义 )
    sum(wether_qualified)
    
    cutted_data <- cutted_data[,wether_qualified]## 提取亚组
    ### 不符合要求
    cat(sum(wether_qualified==F) ," peaks contains zero  (NA in this condition ) 
    \n    and was kicked out \n ")
    
    
    # 注: 凡是有NA或者未检出样本的 代谢物,均会被剔除,采取宁缺毋滥的策略进行分析(不过可能会漏掉在A组表达,B组不表达的代谢物,具体问题还需要具体分析(主要看是否是与分组相关的未检出)") ### 
    # 输出数据
    cutted_data <- apply(cutted_data,2,function(each_col){ each_col <- as.numeric(as.character(each_col))})
    write.xlsx(sub_data,paste("D:\\百度云同步盘\\2017-周迪实验数据及步骤记录\\分析结果可视化\\代谢组学\\",cut_off, "原则下 亲水相最终合格的数据",Sys.Date(),".xlsx"))
    write.xlsx(cutted_data,rownames=T,
               append=T ,
               row.names=T,
               paste(   "qualified peaks", ### 还是采用exl比较保险, csv虽然快,但是处理有效数字 比较费劲... 万一因为有效数字问题导致显著性检验结果改变,得不偿失.
                        "with Qc RSD as", cut_off, "   ", annotation , 
                        "performed at",Sys.Date(),".xlsx"))
    
    write.xlsx(cutted_data_with_Qc,rownames=T,
               append=T ,
               row.names=T,
               paste(   "Qc in this file qualified peaks", ### 还是采用exl比较保险, csv虽然快,但是处理有效数字 比较费劲... 万一因为有效数字问题导致显著性检验结果改变,得不偿失.
                        "with Qc RSD as", cut_off, "   ", annotation , 
                        "performed at",Sys.Date(),".xlsx"))
    
    
    # 一. 方法筛选, 利用模型的 置换检验, R2Y  R2X 和  挑选合适的模型, 挑选合适的 RSD, 以及前处理方法 ##############
    # 进行统计分析并且挑选模型
    "从源码中找到 glog校正的方法 "
    
    
    
    
    
    #"# 1.2  对数据集进行各种变换, 将结果输出为一个列表,进行后续处理." #####
    for_adj <- c("sum","median")
    for_transform <- c("log","cube root","reverse","glog","square root")#  "reverse" 倒数
    for_normal  <- c("auto","Pareto","Range scaling")
    
    
    # 创建唯一的索引, 遍历每种组合.
    combine_them <- NULL
    combine_them[[1]] <- for_adj
    combine_them[[2]] <- for_transform
    combine_them[[3]] <- for_normal
    
    for(num in 111:999){ # 筛选和规矩的
      if(num==111){line <- matrix(NA,nrow=999,ncol=3)}
      temp_num <- num
      for( i in 1:3){ ### 循环 3 次
        index <- temp_num %% 10
        temp_num      <- (temp_num) %/% 10
        if( index <= length(combine_them[[i]]) & index > 0 ){
          line[num,i] <- combine_them[[i]][index]
        } else{line[num,i] <- NA } ## 之后检测 NA, 有NA的就删除该行
      }
    }  
    
    
    ### 所有可能的组合,去重.
    all_possible_combination <- apply(line,1,function(each_row){
      sum(is.na(each_row))==0 }### 筛选出 没有NA的保留
    ) %>% line[.,] %>% unique()
    
    all_possible_combination <- data.frame(id=1:nrow(all_possible_combination),all_possible_combination)
    
    
    for( i in 1:nrow(all_possible_combination)){
      if(i==1){ data_in_list <-list() }
      temp_line  <- all_possible_combination[i,]
      each_adj   <- temp_line[2]
      each_trans <- temp_line[3]
      each_norm  <- temp_line[4]
      temp_data  <- cutted_data ### 赋值语句, 提供初始数据, 后面的数据根据这个计算,得出衍生的子数据集
      
      if(each_adj=="sum"){  ## 浓度校正#### 按照行进行
        temp_data <- apply(temp_data,1,function(each_row){
          new_row <- each_row/sum(each_row,na.rm = T)
          return(new_row)
        }) %>% t()  ### 要记得转置
      } else if (each_adj=="median"){
        temp_data <- apply(temp_data,1,function(each_row){
          new_row <- each_row/median(each_row,na.rm = T)
          return(new_row)
        }) %>% t()
      }
      # 数据转换 不用在意行 OR 列进行
      
      if(each_trans=="log"){temp_data <- log(temp_data)
      } else if (each_trans=="cube root"){  ## 矩阵 OR 数据框做分母时, 结果会转置,需要转置回来才能下一步分析._ 一派胡言... 怎么搞得... 根本就是搞反了好伐.
        temp_for_temp_data <- abs(temp_data)^(1/3)  ##-8的立方根结果不唯一，而且涉及到复数。 因此采用上述计算
        temp_for_temp_data[temp_data<0] <- -temp_for_temp_data
        temp_data <- temp_for_temp_data
      } else if(each_trans=="square root"){
        temp_for_temp_data <- sqrt(abs(temp_data)) ## 防止负数时 搞出一个 复数 出来.
        temp_for_temp_data[temp_data<0] <-  -temp_for_temp_data
        temp_data <- temp_for_temp_data
      } else if(each_trans=="reverse"){
        temp_data <- (1/temp_data)  
      }else if(each_trans=="glog"){
        min.val <- min(abs(temp_data[temp_data != 0]))/10
        temp_data <- apply(temp_data, 2, function(each_col){
          return(glog(each_col,min.val))
        })
      }
      # }else if(each_trans=="glog"){
      #   ### 借用指定的语句, 产生文件,并且设定路径D:\\ ,替换完成后, 将文件路径变回来.
      #   setwd("E:\\Rcools\\") ## 这句话不同人的电脑不一样, 其实本来也是是不必要的,加了一层保险而已
      #   ori_wd <- getwd()
      #   setwd("D:\\")
      #   temp_data  <- data.frame(sub_for_my_scale[,1:2],temp_data)    
      #   # temp_data  <- cbind(sub_for_my_scale[,1:2],temp_data)    
      #   write_excel_csv(temp_data ,path="temp_for_glog.csv")
      #   mSet<-InitDataObjects("conc", "stat", FALSE)
      #   mSet<-Read.TextData(mSet,"temp_for_glog.csv", "rowu", "disc") ### 读取文件 
      #   # # 不需要的 数据前处理
      #   mSet<-SanityCheckData(mSet) # # 数据核验,必要, 区分 数值和字符串 等
      #   mSet<-ReplaceMin(mSet) ## 替换最小值？ 处理缺失数据
      #   mSet<-Normalization(mSet, "NULL", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)
      #   mSet<-SaveTransformedData(mSet) ### 保存转换后的数据
      #   temp_data  <-  read.csv("data_normalized.csv")
      #   temp_data <- temp_data[,3:ncol(temp_data)] ### 去掉头两列, 相当于完全还原了原始数据.
      #   setwd(ori_wd) 
      # } 
      # normalization 按照列进行
      if(each_norm=="auto"){
        temp_data <- apply(temp_data,2,function(each_col){ new_col <- (each_col-mean(each_col,na.rm=T))/sd(each_col,na.rm=T)
        })  }else if (each_norm=="Pareto"){
          temp_data <- apply(temp_data,2,function(each_col){ new_col <- (each_col-mean(each_col,na.rm=T))/sqrt(sd(each_col,na.rm=T))})
        }else if (each_norm=="Range scaling"){
          temp_data <- apply(temp_data,2,function(each_col){ new_col <- (each_col-mean(each_col,na.rm=T))/(max(each_col,na.rm=T)-min(each_col,na.rm=T))})
        }
      data_in_list[[i]] <- temp_data
    }
    
    # View(data_in_list[[1]])
    "data were in the list: \n
  data_in_list 
\n and the .. were in the 
"
    new_vector <- NULL
    new_vector <- apply(all_possible_combination,1,function(each_col){
      new_line <- NULL
      for(each_element in each_col){
        new_line <- paste(new_line,each_element,sep=" ")
      }
      return(new_line)
    })
    
    
    "注意改变 3个参数, 他们分别是:
    
    
    
    
    
    
    "   
    
    " 第 2 章 "
    ########## 对数据集进行正式处理 ##############################
    "## 通过 data_in_list[[i]] 来对不同变换的数据集进行调用,通过 all_possible_combination[i]获取 使用的方法"
    
    for (i in 1:2){
      if(i ==1) { collect_the_finding <- NULL }
      data_in_list[[i]] <- data.frame(data_in_list[[i]])
      ## checkpoint 4 设定分组和 样本编号
      data_in_list[[i]] <- data.frame(sample_id=sample_id,
                                      group=group_vector
                                      ,data_in_list[[i]])
      file_path <-  paste("F:\\百度云同步盘\\2017-周迪实验数据及步骤记录\\0805 代谢组学\\",annotation,"\\",cut_off,new_vector[i],sep="")
      # 新建工作目录
      if(file.exists(file_path)==F){   dir.create( paste(file_path,"\\",sep="") ,recursive = T) }
      setwd(file_path) ## 修改工作目录
      # 进行 网站中的数据分析. 绘图.
      run_meta(data_in_R = data_in_list[[i]],temp_file_path = "D:\\metaboliomic analysis\\temple file.csv")
    }
    
  }# bracket_for_cutoff value
}# final_ bracket










# 终章: 将数据提取并且 汇总为表格:
"using sheetnum and output to build the annotation
and using cutoff to help build the "

for (i in 1: length(data_in_list)){ ## 搜集数据, 最后可以单独弄一个循环体.
  ## checkpoint 4 设定分组和 样本编号
  file_path <-  paste("F:\\百度云同步盘\\2017-周迪实验数据及步骤记录\\0805 代谢组学\\",annotation,"\\",cut_off,new_vector[i],sep="")
  # 新建工作目录
  setwd(file_path) ## 修改工作目录
  # 进行 网站中的数据 提取及分析
  opls_model <- read.csv("oplsda_model.csv")
  R2X <- opls_model[1,-1] %>% as.numeric() %>%    sum()
  R2Y <- opls_model[2,-1] %>% as.numeric() %>%    sum()
  Q2Y <- opls_model[3,-1] %>% as.numeric() %>%    sum()
  if(R2X>=0.5 & R2Y>=0.5 & Q2Y>= 0.5 ){ opls_model_summary ="Qualified"}
  if(R2X>=0.6 & R2Y>=0.6 & Q2Y>= 0.6 ){ opls_model_summary ="Pretty_good"}
  if(R2X>=0.7 & R2Y>=0.7 & Q2Y>= 0.7 ){ opls_model_summary ="Awesome"}
  sum_R2X_R2Y <-  R2X+R2Y
  
  whether_rubust <- ""
  whether_qualified_pls_cv <- ""
  whether_split_in_pca <- ""
  
  ########### 提取 火山圖中 p<0.05 改变倍数大于 2 的变量...
  
  volcano <- read.csv("volcano.csv")
  qualified_in_volcano <-  ((volcano$log2.FC.%>% abs()  >=2) & (volcano$raw.pval <0.05) ) %>% volcano[ . ,1]
  number_qualified_with_2fold_and_p0.05 <- qualified_in_volcano  %>% length()        
  volcano_for_output  <-  paste(qualified_in_volcano,collapse = "_whats_up_man_")
  # opls_da 模型中可以用作后续分析的标志物 #######
  opls_data <- read.csv("oplsda_splot.csv")
  qualified_in_opls_with_pcorr_0.3 <-   (opls_data$p.corr..1.  %>% abs() >=0.3) %>%  opls_data[.,1]
  number_qualified_with_pcorr_0.3  <-   length(qualified_in_opls_with_pcorr_0.3)
  pcorr_0.3_for_output <-  paste(qualified_in_opls_with_pcorr_0.3,collapse = "_whats_up_man_")
  
  ## pls 模型 第一主成分的 VIP值 大于 1 的 变量
  pls_data <-  read.csv("plsda_vip.csv")
  qualified_pls_VIP <-  ((pls_data$Comp..1 %>% abs()) >=1 ) %>% pls_data[.,1]
  number_qualified_with_VIP_1 <- qualified_pls_VIP %>% length()
  VIP_1_for_output <-  paste(qualified_pls_VIP , collapse = "_whats_up_man_")
  
  " 提取出 要比较的数据  包括但不限于: 
  OPLS 模型  oplsda_model.csv 的 三个指标加和...
  oplsda_splot.csv 中的pcorr
  几种不同前处理方法下的有意义的代谢物数目                   模型
  plsda_vip.csv 成分1的得分 大于...
  
  fold_change.csv
  volcano.csv
  # 将显著性的结果用 _whats_up_man_ 衔接最后 拆分解旋. 形如strsplit('sssdsss',split = 'dss')
  
  "
  # checkpoint 5 using "collect_the_finding to save the summary report of each method.
  temp_summary_in_line <- data.frame(annotation=annotation,RSD_cutoff=cut_off,normalization_method=new_vector[i]
                                     ,R2X=R2X,R2Y=R2Y,Q2Y=Q2Y,opls_model_summary=opls_model_summary
                                     ,whether_rubust=whether_rubust,whether_qualified_pls_cv = whether_qualified_pls_cv,whether_split_in_pca =whether_split_in_pca
                                     ,number_with_2fold_p0.05=number_qualified_with_2fold_and_p0.05,qualified_in_volcano=volcano_for_output
                                     ,number_qualified_with_pcorr_0.3 = number_qualified_with_pcorr_0.3, qualified_with_pcorr0.3=pcorr_0.3_for_output
                                     ,number_qualified_with_VIP_1 = number_qualified_with_VIP_1, qualified_VIP_1=VIP_1_for_output)
  ## 若变量不存在, 那么就怎样怎样... ## 在脚本的一开始,将次数据集搞成空.
  collect_the_finding = rbind(collect_the_finding,temp_summary_in_line) 
}










## 第五章 曲终人不散 做图及表述优化 ### 将 要使用的文件的 id 增加一列,用来存储  化合物名称, 便于后续的 提取和做图.
" for better plot, we can use the metabolite with significant changes to draw pictures, which is little tricky but really works "



























split("asdfdsdf",f="join",sep="sd")


exists("collect_the_finding")
diyici <- data.frame(x=1:5,y=2:6)
dierci <- NULL
dierci <- cbind(dierci,z=2:4)
demo(split)
split
getwd()
paste(1,2,sep="dkfjalj")

# 二. 保存      初筛后的结果 此时提取的先是 特征峰, 后面才要搞成特定的代谢物呢..  ,并替换好 ID 用于  后续的分析  ################
# 三. 利用      不带Qc的 做一套系统的分析 ################
# 四. 利用      带  Qc的 做一个主成分分析 ################
# 四. 