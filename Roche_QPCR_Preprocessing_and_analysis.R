
########### DATA STORAGE, QC and NORMALIZATION Script ##########################################################

######################### DIRECTORY CHECK and CREATION #########################################################

#rm(list=ls(all=TRUE))
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tibble")
#install.packages("optparse")

options(warn=-1)
mainDir = "."
subDir1 = "Roche_original_files"
subDir2 = "Additional_Input_Files"
subDir3 = "Norm"
subDir4 = "DB"
subDir5= "QC_Backup"


options(someUniqueTag.mainDir = mainDir)
options(someUniqueTag.subDir = "subDir")


              tryCatch(if (!file_test("-d", file.path(mainDir, subDir1,subDir2,subDir3,subDir4,subDir5))){ 
              if(file_test("-f", file.path(mainDir, subDir1, subDir2, subDir3, subDir4, subDir5))){
              stop("Path can't be created because a file with that name already exists.")
              } else {
              dir.create(file.path(mainDir, subDir1))
              dir.create(file.path(mainDir, subDir2))
              dir.create(file.path(mainDir, subDir3))
              dir.create(file.path(mainDir, subDir4))
              dir.create(file.path(mainDir, subDir5))
              }
           })

library(dplyr)
library(stringr)
library(tibble)
library(optparse)



################################# COPY FILES FROM TMP TO Roche_original_files FOLDER ###########################

options(warn=0)
current.folder = "."
Roche_original_data = "./Roche_original_files"
Additional_Input_Files = "./Additional_Input_Files"

              list.of.files_Roche_original <- list.files(current.folder, "[0-9]+_short.txt$|[0-9].txt$", full.names=T) 
              file_cp=file.copy(list.of.files_Roche_original, Roche_original_data)                       
                  list.of.files_Additional_Input_Files <- list.files(current.folder, "*Annotation*", full.names=T) 
                  file_cp=file.copy(list.of.files_Additional_Input_Files, Additional_Input_Files)    

junk <- dir(path = ".", pattern = "*Annotation*")
file.remove(junk)

################################ APPEND FILES WITH COMMON FILE NAME (ExNN) #################################### 

list_of_files = list.files(path = ".", "*R[1-3].txt|[1-3]+_short.txt$")

#spl_files=strsplit(list_of_files, "_", fixed= TRUE)
#spl_files_= lapply(spl_files,FUN=function(x){paste(x[2],sep="_")})
#spl_files_=as.data.frame(spl_files_)
#spl_files_=t(spl_files_)
#spl_files_= as.data.frame(spl_files_)
#tb= table(spl_files_[,1])
#tb= as.data.frame(tb)
#check=var(tb[,2]) == 0
#if(check[1] ==FALSE) {

exec <-  list_of_files %>% 
  str_split_fixed("_",4) %>% 
  as_tibble() %>% 
  mutate(replicas = str_split_fixed(V4, ".txt",2)[,1]) %>% 
  group_by(V2) %>% 
  dplyr::summarise(execute = ifelse (n() == 3, 1, 0))


names(exec)[1] <- "Experiment"
#for (exp in seq(along = exec$Experiment)){

#  if (exec[exp,]$execute == 1){
#    message("Experiment:", exec[exp,]$Experiment,"--> OK, RUN")
#    print("DOING SOMETHING")
#  } else{
#    message("Experiment:", exec[exp,]$Experiment,"--> FAIL")
#    print("DOING NOTHING")

#  }


#if(exec$execute ==1) {
list_of_files = list.files(path = ".", "[0-3].txt")
list_of_files_sh = tryCatch(list.files(path = ".", "[0-9]+_short.txt$"))
if(length(list_of_files_sh)!=0 & length(list_of_files)!=0){
             #files concatenation
             file.prefixes <- unique(sapply(list.files(path=".", pattern=".+[0-9].txt"), substr, 1,13)) 
             file.prefixes_sh <- unique(sapply(list.files(path=".", pattern="[0-9]+_short.txt$"), substr, 1,13))
             file.prefixes_sh = as.data.frame(file.prefixes_sh)
             file.prefixes_sh[,1] <- sub("$", "_short", file.prefixes_sh[,1])
             file.prefixes_sh = unlist(file.prefixes_sh)
             file.prefixes_sh = as.vector(file.prefixes_sh)
             file.prefixes_all <- unique(as.vector(rbind(file.prefixes,file.prefixes_sh)))
             z_orig <- strsplit(file.prefixes_all,"_",fixed=TRUE)
             z_ps= lapply(z_orig,FUN=function(x){paste(x[2])})
             sb =subset(exec, exec$execute==1)
             z_sb=z_ps%in%sb$Experiment
             z=unlist(z_ps[z_sb])
             file.list <- lapply(substr(z, 1, 11), function(x)list.files(pattern=paste("",x, ".*.txt" ,sep=""), path="."))         
             file.list_names <- as.data.frame(unlist(file.list))
             file.list_names=as.data.frame(sapply(file.list_names, function(x) gsub("_R+[0-9]|.txt", "", x)))
             file.list_names= unique(file.list_names[,1])
             file.list_names= as.character(file.list_names)
             names(file.list) <- c(file.list_names)              
                   tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.delim(file, header=FALSE)))   
                   joined.tables <- lapply(tables, function(t)do.call(cbind, t))                                                                                                                                                                        
                         for (prefix in names(joined.tables))write.table(joined.tables[[prefix]], paste(prefix, "_combined.txt", sep=""), sep = "\t", row.names = F)
                            Approved_QC_removal= lapply(file.list, function(x) file.remove(x))
        } else{
        if(length(list_of_files_sh)==0 & length(list_of_files)!=0){
          file.prefixes <- unique(sapply(list.files(path=".", pattern=".+[0-9].txt"), substr, 1,13))
          file.prefixes_all=file.prefixes
          z_orig <- strsplit(file.prefixes_all,"_",fixed=TRUE)
             z_ps= lapply(z_orig,FUN=function(x){paste(x[2])})
             sb =subset(exec, exec$execute==1)
             z_sb=z_ps%in%sb$Experiment
             z=unlist(z_ps[z_sb])
             file.list <- lapply(substr(z, 1, 11), function(x)list.files(pattern=paste("",x, ".*.txt" ,sep=""), path="."))           
             file.list_names <- as.data.frame(unlist(file.list))
             file.list_names=as.data.frame(sapply(file.list_names, function(x) gsub("_R+[0-9]|.txt", "", x)))
             file.list_names= unique(file.list_names[,1])
             file.list_names= as.character(file.list_names)
             names(file.list) <- c(file.list_names)               
                   tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.delim(file, header=FALSE)))
                   joined.tables <- lapply(tables, function(t)do.call(cbind, t))
                            Approved_QC_removal= lapply(file.list, function(x) file.remove(x))
                 
        }else{
             if(length(list_of_files_sh)!=0 & length(list_of_files)==0){
             file.prefixes_sh <- unique(sapply(list.files(path=".", pattern="[0-9]+_short.txt$"), substr, 1,13))
             file.prefixes_sh = as.data.frame(file.prefixes_sh)
             file.prefixes_sh[,1] <- sub("$", "_short", file.prefixes_sh[,1])
             file.prefixes_sh = unlist(file.prefixes_sh)
             file.prefixes_sh = as.vector(file.prefixes_sh)
             file.prefixes_all =file.prefixes_sh
             z_orig <- strsplit(file.prefixes_all,"_",fixed=TRUE)
             z_ps= lapply(z_orig,FUN=function(x){paste(x[2])})
             sb =subset(exec, exec$execute==1)
             z_sb=z_ps%in%sb$Experiment
             z=unlist(z_ps[z_sb])
             file.list <- lapply(substr(z, 1, 11), function(x)list.files(pattern=paste("",x, ".*.txt" ,sep=""), path="."))       
             file.list_names <- as.data.frame(unlist(file.list))
             file.list_names=as.data.frame(sapply(file.list_names, function(x) gsub("_R+[0-9]|.txt", "", x)))
             file.list_names= unique(file.list_names[,1])
             file.list_names= as.character(file.list_names)
             names(file.list) <- c(file.list_names)
             #Compl_repl_cases= Filter(function(x) length(x) == 3, file.list)
             #file.list= Compl_repl_cases                  
                   tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.delim(file, header=FALSE))) 
                   joined.tables <- lapply(tables, function(t)do.call(cbind, t))                                                                                                                                                                       
                         for (prefix in names(joined.tables))write.table(joined.tables[[prefix]], paste(prefix, "_combined.txt", sep=""), sep = "\t", row.names = F)
                            Approved_QC_removal= lapply(file.list, function(x) file.remove(x))
         }     
      }
 }

    #}      
# }


############################## REMOVE FILES YOU DO NOT NEED #################################


#junk <- dir(path = ".", pattern = "[0-9].txt|short.txt")
#file.remove(junk)



################################# QC on COMBINED FILES ######################################


                    list_of_combined_files = list.files(path = ".", "*combined.txt")
                    for (list_of_combined_file in list_of_combined_files) {
                         QC <- read.delim(list_of_combined_file, header = TRUE, stringsAsFactor = F, na.strings = "")
                         names(QC) = QC[2,]
                         QC = QC[-c(1:2),]
                         pos <- grep(c("Pos"), names(QC))
                         Name <- grep(c("Name"), names(QC))
                         Cp <- grep(c("Cp"), names(QC))
                         Status <- grep(c("Status"), names(QC))
                         pos_list= QC[, pos]
                         Name_list= QC[, Name]
                         Cp_list= QC[, Cp]
                         Status_list = QC[, Status]
                         parsed_QC = cbind(pos_list[,1], Name_list[,1], Cp_list, Status_list)
                         
                         
######################################### N of AVAILABLE DATA COUNTING ############################################################ 

                        
                             N_of_available_data = data.frame(unlist(lapply(apply(parsed_QC[,3:5], 1, table), function(x) sum(x))))
                             names(N_of_available_data) = c("N_of_available_data")
                             cb = cbind(parsed_QC, N_of_available_data)                         
                                x= which(is.na(cb[,c(3:5)]), arr.ind=TRUE)
                                x = as.data.frame(x[order(x[,1]),], stringsAsFactors = F)
                                x$col[x$col ==1]  <- "R1"
                                x$col[x$col ==2]  <- "R2"
                                x$col[x$col ==3]  <- "R3"
                                     y= which(!is.na(cb[,c(3:5)]), arr.ind=TRUE)
                                     y = as.data.frame(y[order(y[,1]),], stringsAsFactors = F)
                                     rb = rbind(x, y)
                                     rb = as.data.frame(rb[order(rb[,1]),], stringsAsFactors = F)
                                     rb$col[rb$col ==1]  <- 0
                                     rb$col[rb$col ==2]  <- 0
                                     rb$col[rb$col ==3]  <- 0
                                     r= split(rb, rb[,1])
                                     r= do.call(rbind, lapply(r,function(x) x$col))
                                     Missing_values <- as.data.frame(paste(r[,1], r[,2], r[,3], sep=","))
                                     names(Missing_values) = "Missing_values"                                  
                                     cb = cbind(cb, Missing_values)
                                     cb_35_highlight= cbind(cb, Missing_values)
                                     
                                     
######################################### SUMMARY STATISTICS ####################################################################          
                            
                                         cb[, 3:5][cb[, 3:5] >= 35] <- NA
                                         sd = as.data.frame(apply(cb[,c(3:5)], 1, sd, na.rm = T)) #con 1 solo numero --> NA
                                         names(sd) = c("SD")
                                         cb = cbind(cb, sd)
                                         cb[,3:5] = as.numeric(as.matrix(cb[,3:5]))
                                         na_count <- as.data.frame(apply(cb[,3:5], 1, function(x) sum(is.na(x))))                                    
                                         Mean <- apply(cb[,3:5], 1, mean, na.rm = T)
                                         cb = cbind(cb, Mean)
                                         cb$Mean <- ifelse(na_count[,1] > 1, "NA", cb$Mean)
                                         cb$Mean[cb$N_of_available_data ==1] <- "NA"                                         
                                         SD_check <- sd$SD>0.4
                                         SD_check= as.numeric(SD_check)
                                         SD_check = replace(SD_check, SD_check==0, "YES")
                                         SD_check = replace(SD_check, SD_check==1, "NO")
                                         SD_check = as.data.frame(as.character(SD_check))
                                         names(SD_check) = c("SD_QC")
                                         cb_SD_check = cbind(cb, SD_check) 
                                             conditional_check_subset= subset(cb_SD_check, cb_SD_check$N_of_available_data == 3 & cb_SD_check$SD_QC == "NO")
                                             conditional_check_subset <-  conditional_check_subset[complete.cases(conditional_check_subset[,3:5]), ]
                                             if(nrow(conditional_check_subset)!=0){
                                             conditional_check = t(apply(conditional_check_subset[,3:5], 1, function(x) x[-which.max(abs(x-mean(x)))]))
                                             conditional_sd = cbind(conditional_check, Conditional_SD = apply(as.data.frame(conditional_check), 1, sd)) 
                                                   conditional_mean = cbind(conditional_check, Conditional_Mean = apply(as.data.frame(conditional_check), 1, mean)) 
                                                        x=rep(cbind("R1", "R2", "R3"), nrow(conditional_check_subset[,3:5])) 
                                                        x= as.data.frame(x)
                                                        y= as.data.frame(unlist(as.data.frame(unlist(t(conditional_check_subset[,3:5])))))
                                                        cb_rep = cbind(y, x)
                                                        z=nrow(cb_rep)/3
                                                        cb_rep_Index=as.data.frame(rep(1:z, each=3, 1))
                                                        cb_rep=cbind(cb_rep, cb_rep_Index)
                                                        names(cb_rep)=c("Ct", "Replicate", "Index") 
                                                        Outlier = t(apply(conditional_check_subset[,3:5], 1, function(x) x[which.max(abs(x-mean(x)))]))                                                    
                                                        Outlier= as.data.frame(t(Outlier))
                                                        Outlier_Index=as.data.frame(rep("R4", nrow(Outlier)))
                                                        Outlier_Index=cbind(as.data.frame(Outlier[,1]), Outlier_Index[,1])                                                    
                                                        j= nrow(Outlier_Index)
                                                        Outlier_rep_Index=as.data.frame(rep(1:j, each=1, 1))
                                                        Outlier_Index= cbind(Outlier_Index, Outlier_rep_Index)
                                                        names(Outlier_Index)=c("Ct", "Replicate", "Index")                                                    
                                                        rb= rbind(cb_rep, Outlier_Index)
                                                        rb <- rb[order(rb$Index),]
                                                        myrb <- split(rb, rb$Index)
                                                        #sorted_myrb <- lapply(myrb, function(df){df[order(df$Ct),]})                                                      
                                                        mynonDupeList <- lapply(myrb, function(myrb) myrb[((duplicated(myrb$Ct) | duplicated(myrb$Ct, fromLast=TRUE)) & myrb$Ct[myrb$Replicate == "R4"]),])
                                                        mynonDupeList = as.data.frame(do.call("rbind", mynonDupeList))
                                                        mynonDupeList=mynonDupeList[mynonDupeList[,2]!="R4", ]
                                                        rl <- rle(mynonDupeList[,1])
                                                        rl=cbind(mynonDupeList, as.data.frame(rep(rl$lengths != 1 , times = rl$lengths)))
                                                        mynonDupeList=rl[rl[,4]==FALSE, ]
                                                        Outlier_R=as.data.frame(mynonDupeList[,2])
                                                   conditional_check_subset_pos= as.data.frame(as.character(conditional_check_subset[,1])) 
                                                   cb_conditional_check= cbind(conditional_check_subset_pos, conditional_sd, conditional_mean[,3], Outlier_R[,1])
                                                   cb_SD_check[,1] = as.character(cb_SD_check[,1])
                                                   cb_conditional_check[,1] = as.character(cb_conditional_check[,1])
                                                   cb_SD_check[cb_SD_check[,1]] <- cb_conditional_check[match(cb_SD_check[,1L],cb_conditional_check[,1L]),-1L]                                                                                   
                                                   Conditional_SD= as.data.frame(cb_SD_check[cb_SD_check[,1]][,c(3:5)])
                                                   names(Conditional_SD) = c("Conditional_SD", "Conditional_Mean", "Outlier_R")
                                                   Conditional_SD[,1:2][is.na(Conditional_SD[,1:2])] <- 0
                                                   Conditional_SD$Conditional_SD <- ifelse(na_count[,1] > 1, "NA", Conditional_SD$Conditional_SD)
                                                   Conditional_SD$Conditional_Mean <- ifelse(na_count[,1] > 1, "NA", Conditional_SD$Conditional_Mean)
                                                   cb_SD_check = cbind(cb, SD_check) 
                                                   cb = cbind(cb_SD_check, Conditional_SD[,c(1:3)])
                                                          cb$Conditional_SD <- ifelse(cb$N_of_available_data ==2, cb$SD, Conditional_SD$Conditional_SD)                                
                                                          cb$Conditional_Mean <- ifelse(cb$N_of_available_data ==2, cb$Mean, Conditional_SD$Conditional_Mean)                                                         
                                              SD_check_conditional_QC <- cb$Conditional_SD>0.4
                                              SD_check_conditional_QC = as.numeric(SD_check_conditional_QC)
                                              SD_check_conditional_QC = replace(SD_check_conditional_QC, SD_check_conditional_QC==0, "YES")
                                              SD_check_conditional_QC = replace(SD_check_conditional_QC, SD_check_conditional_QC==1, "NO")                                     
                                              SD_check_conditional_QC  <- ifelse(na_count[,1] > 1, "NA", SD_check_conditional_QC )
                                              SD_check_conditional_QC = as.data.frame(as.character(SD_check_conditional_QC))
                                              names(SD_check_conditional_QC) = c("Conditional_SD_QC")
                                              cb = cbind(cb, SD_check_conditional_QC)
                                              
                                        ########## Ct==35.01 HIGHLIGHT #######################################################################
                                        
                                        Ct_35_check= as.data.frame(unlist(as.data.frame(unlist(t(cb_35_highlight[,3:5])))))  
                                        x=rep(cbind("R1", "R2", "R3"), nrow(cb_35_highlight[,3:5])) 
                                        Ct_35_check_subset= cbind(Ct_35_check, x)
                                        Ct_35_check_subset$Rejected_Ct <- ifelse(as.matrix(Ct_35_check_subset[,1]) >= 35, 35, 0)
                                        cb_Ct_35_check = cbind(as.data.frame(Ct_35_check_subset$Rejected_Ct), x)
                                        names(cb_Ct_35_check) = c("Ct_35_check", "Ct_35_R")
                                        d= split(cb_Ct_35_check, cb_Ct_35_check[,2])
                                        d = cbind(d$R1, d$R2, d$R3)
                                        R1 <- ifelse(d[,1] == 35, as.character(d[,2]), 0 )
                                        R2 <- ifelse(d[,3] == 35, as.character(d[,4]), 0 )
                                        R3 <- ifelse(d[,5] == 35, as.character(d[,6]), 0 )
                                        Ct_35_check_assembly <- as.data.frame(paste(R1, R2, R3, sep=","))
                                        names(Ct_35_check_assembly) = c("Ct_35_check")                                      
                                    cb = cbind(cb, Ct_35_check_assembly)
                                    cb[,3:5] = cb_35_highlight[,3:5]
                                write.table(cb, list_of_combined_file, append = F, quote = F, row.names = F, col.names = T, sep = "\t")
                             }else{ Ct_35_check= as.data.frame(unlist(as.data.frame(unlist(t(cb[,3:5])))))  
                                        x=rep(cbind("R1", "R2", "R3"), nrow(cb[,3:5])) 
                                        Ct_35_check_subset= cbind(Ct_35_check, x)
                                        Ct_35_check_subset$Rejected_Ct <- ifelse(as.numeric(Ct_35_check_subset[,1]) >= 35, 35, 0)
                                        cb_Ct_35_check = cbind(as.data.frame(Ct_35_check_subset$Rejected_Ct), x)
                                        names(cb_Ct_35_check) = c("Ct_35_check", "Ct_35_R")
                                        d= split(cb_Ct_35_check, cb_Ct_35_check[,2])
                                        d = cbind(d$R1, d$R2, d$R3)
                                        R1 <- ifelse(d[,1] == 35, as.character(d[,2]), 0 )
                                        R2 <- ifelse(d[,3] == 35, as.character(d[,4]), 0 )
                                        R3 <- ifelse(d[,5] == 35, as.character(d[,6]), 0 )
                                        Ct_35_check_assembly <- as.data.frame(paste(R1, R2, R3, sep=","))
                                        names(Ct_35_check_assembly) = c("Ct_35_check")                                      
                                    cb = cbind(cb_SD_check, Ct_35_check_assembly)
                                write.table(cb, list_of_combined_file, append = F, quote = F, row.names = F, col.names = T, sep = "\t")
                               
                       }

                }
                
                

########################### COLNAMES FILES ######################################################


                         Set_colnames <- function(fileName, check.names=FALSE, header = TRUE, stringsAsFactor = FALSE, sep = "\t",...){
                         Data <- read.delim(fileName, header = header, check.names = check.names, stringsAsFactor = stringsAsFactor, sep = sep, ...)
                            y = fileName
                            z <- strsplit(y,"_",fixed=TRUE)
                            z= lapply(z,FUN=function(x){paste(x[1],x[2], x[3],sep="_")})
                            standard_names = colnames(Data)
                            colnames(Data) = c(paste0(z, "_Pos"), paste0(z, "_Name"), paste0(z, "_R1"), paste0(z, "_R2"), paste0(z, "_R3"), paste0(z, "_status_R1"), paste0(z, "_status_R2"), paste0(z, "_status_R3"), standard_names[9:ncol(Data)])
                            names(Data) = gsub("X", "", colnames(Data))
                            srt = Data[ , order(names(Data))]
                            write.table(Data, file=paste0(y, ""), col.names = T, row.names = F, sep = "\t")
                            return(Data)}                                                          
                            files <- list.files(pattern = "_combined.txt")
                             for (i in 1: length(files)){
                                b <- lapply(files, Set_colnames)}


#The case of a common Annotation File
               files <- list.files(path = ".", "[A-Z]+_combined\\.txt")
                   for (files in files) {
                   Input <- read.delim(files, header=TRUE) # load file
                   names(Input) = gsub("X", "", colnames(Input))
                   Ann = read.delim("./Additional_Input_Files/Annotation.txt", header = TRUE)
                   cb = cbind(Input, Ann[2:ncol(Ann)])
                   names(files) = gsub(pattern = "\\.txt$", "", files)
                   write.table(cb, paste(names(files), "_annotated.txt", sep = ""),  col.names= T, row.names= F, quote= F, sep = "\t") #NB: sovrascrive
                   }
junk <- dir(path = ".", pattern = "[A-Z]+_combined\\.txt")
file.remove(junk)


######################################################################################################################################################################

#The case of different Annotation files
                   
                   files_ann <- gsub(pattern = "\\.txt$", "",(basename(list.files(path = ".", glob2rx("*Annotation*short*"), recursive = T))))
                   file.prefixes=unlist(lapply(files_ann, function(x) strsplit(x, "_")[[1]][3]))
                   file.names=lapply(files_ann, function(x) strsplit(x, "_")[[1]][2:5])
                   file.names=lapply(file.names, function(x) paste(x, collapse = "_"))      
                   files <- list.files(path = ".", "*_short_combined.txt") 
                   if (length(files) !=0) {
                   file.list <- lapply(file.prefixes, function(x)list.files(pattern=paste("",x,".*.txt",sep=""), path="."))
                   names(file.list) <- c(file.names)
                       tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.delim(file, header=TRUE))) 
                       setwd("./Additional_Input_Files")
                       file.list_ann <- lapply(file.prefixes, function(x)list.files(pattern=paste("",x,".*.txt",sep=""), path="."))
                       names(file.list_ann) <- c(file.names)
                       tables_ann <- lapply(file.list_ann, function(filenames)lapply(filenames, function(file)read.delim(file, header=TRUE)))
                       setwd('..')
                         DF_combined <- mapply(function(x, y) merge(x, y, by = 1, all = T), x = tables, y = tables_ann, SIMPLIFY = F)
                         #names = lapply(DF_combined, function(x) ncol(x))
                         #names=names(DF_combined[1][[1]][1:names[[1]]]) 
                         #names = gsub("X", "", names)
                         DF_combined=lapply(DF_combined, function(x) setNames(x, sub("X", "", names(x))))
                    for (file.names in names(file.list))write.table(DF_combined[[file.names]], paste(file.names, "_combined_annotated.txt", sep=""), sep = "\t", row.names = F, col.names = T)
                  }
                    

junk <- dir(path = ".", pattern = "*short_combined\\.txt")
file.remove(junk)

#readline(prompt="Press [enter] to continue")

pause = function()
{
    if (interactive()) {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
    closeAllConnections() 
}

ps = pause()
  
 
#######################################################################################################################################################################

# Condition1: you don't need to repeat the experiment
#install.packages("xlsx")
#library(xlsx)
current.folder = "."
df <- read.delim("QC_file.txt", header = T)
files <- list.files(current.folder, "*_annotated.txt", full.names=F)
if(nrow(df) ==0 ){ 
  for (files in files) {
  myfiles <- read.delim(files, header = TRUE, stringsAsFactor = F)  
  names(myfiles) = gsub("X", "", colnames(myfiles))
  myfiles = myfiles[!grepl("_status_",names(myfiles))]
  myfiles$SD <- ifelse(myfiles$Conditional_SD == 0, myfiles$SD, myfiles$Conditional_SD)
  myfiles$Mean <- ifelse(myfiles$Conditional_Mean == 0, myfiles$Mean, myfiles$Conditional_Mean)
  write.table(myfiles, files, append = F, quote = F, row.names = F, col.names = T, sep = "\t")
   }
   files <- list.files(current.folder, "*_annotated.txt", full.names=F)
  current.folder = "."
  Norm = "./Norm"
  file_cp=file.copy(files, Norm)     #Warning because of an empty file but it works fine
  targetdir_backup = "./QC_Backup"
  Approved_QC= lapply(files, function(x) file.copy(paste (current.folder, x , sep = "/"), paste (targetdir_backup,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
  junk <- dir(path = ".", pattern = "*_annotated.txt")
  file.remove(junk)
}


############################################# GOOD EXPERIMENTS WILL BE MOVED IN THE NORM FOLDER ##########################################################################
               
               list_of_annotated_files = list.files(path = ".", "*annotated.txt")  
               for (list_of_annotated_files in list_of_annotated_files){     
               Annotated_files <- read.delim(list_of_annotated_files, header = TRUE, stringsAsFactor = F, check.names=FALSE)
               names(Annotated_files ) = gsub("X", "", colnames(Annotated_files))
               df <- read.delim("QC_file.txt", header= T, stringsAsFactor = F)                       
                      file.prefixes=as.data.frame(unlist(lapply(list_of_annotated_files, function(x) strsplit(x, "_")[[1]][2])))
                          df_file_names = as.data.frame(unlist(lapply(df[,1], function(x) strsplit(x, "_")[[1]][2])))
                              list_of_annotated_files= as.data.frame(list_of_annotated_files)
                              mtch = file.prefixes[,1] %in% df_file_names[,1]
                                files_to_be_moved = subset(file.prefixes, ! file.prefixes[,1] %in% df_file_names[,1]) 
                                files_to_be_moved = as.character(files_to_be_moved[,1])
                                file.list <- lapply(files_to_be_moved, function(x)list.files(pattern=paste("", x, ".*.txt" ,sep=""), path="."))
                                origindir = "."
                                targetdir = "./Norm"
                                targetdir_backup = "./QC_Backup"
                                Approved_QC= lapply(file.list, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                Approved_QC= lapply(file.list, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir_backup,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                Approved_QC_removal= lapply(file.list, function(x) file.remove(x))
                              }
                                      
 ########################################### ADD R4 WHEN  REQUIRED ####################################################################################################                                 
                                                                        
                                      
                                   list_of_annotated_files = list.files(path = ".", "*annotated.txt")
                                   if(length(list_of_annotated_files) !=0){
                                      #names(Annotated_files ) = gsub("X", "", colnames(Annotated_files))
                                      df_QC_file <- read.delim("QC_file.txt", header= T, stringsAsFactor = F)
                                      df_QC_file[df_QC_file==""] <- 0
                                      df_Samples= rle(df_QC_file[, 2] == 0)
                                      df_Samples$values <- df_Samples$values & df_Samples$lengths >= 1
                                      df_Samples= df_QC_file[!inverse.rle(df_Samples), ]
                                      df_Samples[df_Samples==0] <- ""

                                      #df_Samples=df_Samples[complete.cases(df_Samples),]
                                      #df_Samples[,3][df_Samples[,3]==""] <- "NA"
                                      df_file_names_Samples = as.data.frame(unlist(lapply(df_Samples[,1], function(x) strsplit(x, "_")[[1]][2])))
                                      file_name_to_write_table_Samples=unlist(lapply(unique(df_file_names_Samples[,1]), function(pattern_Ex) {lapply(c("combined_annotated.txt"), function(pattern_test){grep(pattern = paste0(pattern_Ex, ".*", pattern_test, "$"), x = list_of_annotated_files,  value = T)})}))
                                          for (file_name_to_write_table_Samples in file_name_to_write_table_Samples) {
                                        
                                          Annotated_files <- read.delim(file_name_to_write_table_Samples, header = TRUE, stringsAsFactor = F, check.names=FALSE)                                       
                                          file.prefixes=as.data.frame(unlist(lapply(file_name_to_write_table_Samples, function(x) strsplit(x, "_")[[1]][2])))
                                          df_file_names = as.data.frame(unlist(lapply(df_Samples[,1], function(x) strsplit(x, "_")[[1]][2])))                                   
                                             Samples_to_match=  data.frame(unlist(strsplit(as.character(df_Samples[,2]), ';')))
                                             Samples_to_match= as.data.frame(unique(Samples_to_match[,1]))
                                             Pts_Names=grep("*_Name", as.character(names(Annotated_files)))
                                             mtch= match(Annotated_files[grepl("*_Name", names(Annotated_files))][,1], Samples_to_match[,1])
                                             cb= cbind(mtch, Annotated_files)
                                             new_DF <- cb[is.na(cb[,1]),]
                                             new_DF= new_DF[,-1]
                                             write.table(new_DF, file_name_to_write_table_Samples, append= F, quote=F, row.names = F, col.names = T, sep = "\t")
                                             origindir= "."
                                             targetdir = "./Norm"
                                             targetdir_backup = "./QC_Backup"
                                             Approved_QC= lapply(file_name_to_write_table_Samples, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                             Approved_QC= lapply(file_name_to_write_table_Samples, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir_backup,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                             file.remove(file_name_to_write_table_Samples)
                                        }     
                                
                                      list_of_annotated_files_R4= list.files(path = ".", "*R4.txt")
                                      if(length(list_of_annotated_files_R4) !=0){
                                      list_of_annotated_files = list.files(path = ".", "*annotated.txt")
                                      names(Annotated_files ) = gsub("X", "", colnames(Annotated_files))
                                      df_QC_file <- read.delim("QC_file.txt", header= T, stringsAsFactor = F)
                                      df_QC_file[df_QC_file==""] <- 0
                                      is.na.rle <- rle(df_QC_file[, 2] != 0)
                                      is.na.rle$values <- is.na.rle$values & is.na.rle$lengths == 3
                                      df= df_QC_file[!inverse.rle(is.na.rle), ]
                                      df[df==0] <- ""
                                      file.prefixes=as.data.frame(unlist(lapply(list_of_annotated_files, function(x) strsplit(x, "_")[[1]][2])))
                                      df_file_names = as.data.frame(unlist(lapply(df[,1], function(x) strsplit(x, "_")[[1]][2])))
                                      file_name_to_write_table=unlist(lapply(unique(df_file_names[,1]), function(pattern_Ex) {lapply(c("combined_annotated.txt"), function(pattern_test){grep(pattern = paste0(pattern_Ex, ".*", pattern_test, "$"), x = list_of_annotated_files,  value = T)})}))        
                                      for (file_name_to_write_table in file_name_to_write_table) {
                                      Annotated_files <- read.delim(file_name_to_write_table, header = TRUE, stringsAsFactor = F, check.names=FALSE)
                                      df_sub=df[df[,1] %in% colnames(Annotated_files),]   
                                      df_sub_file= unlist(df_sub[,3])
                                      df_sub_file_pos= as.data.frame(df_sub_file)
                                      df_sub_file=df_sub_file[df_sub_file != ""]
                                      list_of_files = list.files(path = ".", df_sub_file)
                                      dList =read.delim(list_of_files, header= F, stringsAsFactor = F)
                                      dList=dList[-1,]
                                      names(dList)= dList[1,]
                                      dList=dList[-1,]
                                      names_quad=strsplit(list_of_files, "\\.")[[1]][1]
                                      names_status_1=strsplit(list_of_files, "\\_")[[1]][1:3]
                                      StatusR4= "_status_R4"
                                      name_status <- paste(paste0(names_status_1,  collapse="_"), StatusR4, sep = "")                                      
                                      Cp_quad=as.data.frame(dList$Cp)
                                      Cp_quad_Status= as.data.frame(dList$Status)
                                      names(Cp_quad)=names_quad
                                      names(Cp_quad_Status)=name_status
                                      cb= cbind(Annotated_files, Cp_quad, Cp_quad_Status)
                                      Pos = which(!df_sub_file_pos == "")
                                          R1 <- "R1"
                                          R2 <- "R2"
                                          R3 <- "R3"
                                          if(Pos == 1){                 
                                            df_reduced <- cb[, -( grep(paste0(R1 , "$" ) , colnames(cb),perl = TRUE) ) ]
                                          } else if(Pos == 2){ 
                                            df_reduced <- cb[, -(grep(paste0(R2 , "$") , colnames(cb),perl = TRUE))]
                                          } else if(Pos == 3){df_reduced <- cb[, -(grep(paste0(R3 , "$") , colnames(cb),perl = TRUE))]} 
                                      srt = df_reduced[ , order(names(df_reduced))]
                                      srt_35_log = df_reduced[ , order(names(df_reduced))]
                                      srt[, 3:5][as.matrix(srt[, 3:5]) >= 35] <- NA
                                      sd = as.data.frame(apply(srt[,c(3:5)], 1, sd, na.rm = T))
                                      srt[,5]=as.numeric(as.character(srt[,5]))
                                      mn = as.data.frame(apply(srt[,c(3:5)], 1, mean, na.rm = T))
                                      names(sd) = c("SD_new_with_R4")
                                      names(mn) = c("Mean_new_with_R4")
                                      SD_check <- sd$SD_new_with_R4>0.4
                                      SD_check= as.numeric(SD_check)
                                      SD_check = replace(SD_check, SD_check==0, "YES")
                                      SD_check = replace(SD_check, SD_check==1, "NO")
                                      SD_check = as.data.frame(as.character(SD_check))
                                      names(SD_check) = c("SD_new_with_R4_QC")
                                      accepted_final= cbind(srt, sd, mn, SD_check)
                                      accepted_final = accepted_final[ , order(names(accepted_final))]
                                      accepted_final[,3:5]=srt_35_log[,3:5]
                                      accepted_final[,3:5][accepted_final[,3:5]==""] <- NA


########################################## N of AVAILABLE DATA COUNTING WITH R4 ###########################################################################
                                            
                                            N_of_available_data = data.frame(unlist(lapply(apply(accepted_final[,3:5], 1, table), function(x) sum(x))))
                                            names(N_of_available_data) = c("N_of_available_data_with_R4")
                                            cb = cbind(accepted_final, N_of_available_data)                         
                                            x= which(is.na(cb[,c(3:5)]), arr.ind=TRUE)
                                            x = as.data.frame(x[order(x[,1]),], stringsAsFactors = F)
                                            x$col[x$col ==1]  <- "R1"
                                            x$col[x$col ==2]  <- "R2"
                                            x$col[x$col ==3]  <- "R4"
                                                y= which(!is.na(cb[,c(3:5)]), arr.ind=TRUE)
                                                y = as.data.frame(y[order(y[,1]),], stringsAsFactors = F)
                                                rb = rbind(x, y)
                                                rb = as.data.frame(rb[order(rb[,1]),], stringsAsFactors = F)                                                
                                                rb$col[rb$col ==1]  <- 0
                                                rb$col[rb$col ==2]  <- 0
                                                rb$col[rb$col ==3]  <- 0
                                                r= split(rb, rb[,1])
                                                 r_sorted= do.call(rbind, lapply(r,function(x) x$col))                                              
                                                   Missing_values <- as.data.frame(paste(r_sorted[,1], r_sorted[,2], r_sorted[,3], sep=","))
                                                   names(Missing_values) = "Missing_values_with_R4"                                  
                                                     cb = cbind(cb, Missing_values)
                                                     cb_35_highlight= cbind(cb, Missing_values)
                                                     
                                                     
###################################################### 35 HIGHLIGHT ######################################################################################
                                            
                                            
                                        Ct_35_check= as.data.frame(unlist(as.data.frame(unlist(t(cb_35_highlight[,3:5])))))  
                                        x=rep(cbind("R1", "R2", "R4"), nrow(cb_35_highlight[,3:5])) 
                                        Ct_35_check_subset= cbind(Ct_35_check, x)
                                        Ct_35_check_subset$Rejected_Ct <- ifelse(as.matrix(Ct_35_check_subset[,1]) >= 35, 35, 0)
                                        cb_Ct_35_check = cbind(as.data.frame(Ct_35_check_subset$Rejected_Ct), x)
                                        names(cb_Ct_35_check) = c("Ct_35_check", "Ct_35_R")
                                        d= split(cb_Ct_35_check, cb_Ct_35_check[,2])
                                        d = cbind(d$R1, d$R2, d$R4)
                                        R1 <- ifelse(d[,1] == 35, as.character(d[,2]), 0 )
                                        R2 <- ifelse(d[,3] == 35, as.character(d[,4]), 0 )
                                        R4 <- ifelse(d[,5] == 35, as.character(d[,6]), 0 )
                                        Ct_35_check_assembly <- as.data.frame(paste(R1, R2, R4, sep=","))
                                        names(Ct_35_check_assembly) = c("Ct_35_check_with_R4")                                      
                                    cb_35 = cbind(cb, Ct_35_check_assembly) 
                                    cb_35_subm = cbind(cb, Ct_35_check_assembly)             
                                    cb_35[, 3:5][as.matrix(cb_35[, 3:5]) >= 35] <- NA
                                    na_count <- as.data.frame(apply(cb_35[,3:5], 1, function(x) sum(is.na(x)))) 
                                    
                                    
####################################################CONDITIONAL MEAN AND SD ACCORDING TO THE SD_QC WITH R4 #################################################
                                 
                                 
                                 conditional_check_subset= subset(cb_35, cb_35$N_of_available_data_with_R4 == 3 & cb_35$SD_new_with_R4_QC == "NO")
                                 conditional_check_subset[,5]=as.numeric(as.character(conditional_check_subset[,5]))
                                             if(nrow(conditional_check_subset)!=0){
                                              conditional_check = t(apply(conditional_check_subset[,3:5], 1, function(x) x[-which.max(abs(x-mean(x)))]))
                                              conditional_sd = cbind(conditional_check, Conditional_SD = apply(as.data.frame(conditional_check), 1, sd)) 
                                              conditional_mean = cbind(conditional_check, Conditional_Mean = apply(as.data.frame(conditional_check), 1, mean)) 
                                                        x=rep(cbind("R1", "R2", "R4"), nrow(conditional_check_subset[,3:5])) 
                                                        x= as.data.frame(x)
                                                        y= as.data.frame(unlist(as.data.frame(unlist(t(conditional_check_subset[,3:5])))))
                                                        cb_rep = cbind(y, x)
                                                        Outlier = t(apply(conditional_check_subset[,3:5], 1, function(x) x[which.max(abs(x-mean(x)))]))
                                                        Outlier= as.data.frame(t(Outlier))
                                                        mtch = match(cb_rep[,1], Outlier[,1])
                                                        cb_rep = cbind(cb_rep, mtch)
                                                        subset <-  cb_rep[complete.cases(cb_rep), ]
                                                        Outlier_R =  as.data.frame(subset[,2])
                                                   conditional_check_subset_pos= as.data.frame(as.character(conditional_check_subset[,2])) 
                                                   cb_conditional_check= cbind(conditional_check_subset_pos, conditional_sd, conditional_mean[,3], Outlier_R[,1])
                                                   cb_35[,2] = as.character(cb_35[,2])
                                                   cb_conditional_check[,1] = as.character(cb_conditional_check[,1])
                                                   cb_35[cb_35[,2]] <- cb_conditional_check[match(cb_35[,2L],cb_conditional_check[,1L]),-1L]                                     
                                                   Conditional_SD= as.data.frame(cb_35[cb_35[,2]][,c(3:5)])
                                                   names(Conditional_SD) = c("Conditional_SD_with_R4", "Conditional_Mean_with_R4", "Outlier_R_with_R4")
                                                   Conditional_SD[,1:2][is.na(Conditional_SD[,1:2])] <- 0
                                                   Conditional_SD$Conditional_SD_with_R4 <- ifelse(na_count[,1] > 1, "NA", Conditional_SD$Conditional_SD_with_R4)
                                                   Conditional_SD$Conditional_Mean_with_R <- ifelse(na_count[,1] > 1, "NA", Conditional_SD$Conditional_Mean_with_R)                                      
                                                   cb = cbind(cb_35_subm, Conditional_SD[,c(1:3)])
                                                          cb$Conditional_SD_with_R4 <- ifelse(cb$N_of_available_data_with_R4 ==2, cb$SD_new_with_R4, Conditional_SD$Conditional_SD_with_R4)                                                       
                                                          cb$Conditional_Mean_with_R4 <- ifelse(cb$N_of_available_data_with_R4 ==2, cb$Mean_new_with_R4, Conditional_SD$Conditional_Mean_with_R)                                                         
                                              SD_check_conditional_QC <- cb$Conditional_SD_with_R4>0.4
                                              SD_check_conditional_QC = as.numeric(SD_check_conditional_QC)
                                              SD_check_conditional_QC = replace(SD_check_conditional_QC, SD_check_conditional_QC==0, "YES")
                                              SD_check_conditional_QC = replace(SD_check_conditional_QC, SD_check_conditional_QC==1, "NO")                                     
                                              SD_check_conditional_QC  <- ifelse(na_count[,1] > 1, "NA", SD_check_conditional_QC)
                                              SD_check_conditional_QC = as.data.frame(as.character(SD_check_conditional_QC))
                                              names(SD_check_conditional_QC) = c("Conditional_SD_QC_with_R4")
                                              cb = cbind(cb, SD_check_conditional_QC)
                                              cb=cb[,c(1,2,3,4,5,6,7,8,17,16,19,14,20,10,9,18,11,12,23,13,24,25,21,15,22,26,27,28,29,30)]
                                              write.table(cb, file_name_to_write_table, append= F, quote=F, row.names = F, col.names = T, sep = "\t")
                                             }else{
                                              cb_35_subm=cb_35_subm[,c(1,2,3,4,5,6,7,8,17,16,19,14,20,10,9,18,11,12,23,13,24,25,21,15,22,26)]
                                              write.table(cb_35_subm, file_name_to_write_table, append= F, quote=F, row.names = F, col.names = T, sep = "\t")}                                 
                                targetdir = "./Norm"
                                targetdir_backup = "./QC_Backup"
                                Approved_QC= lapply(file_name_to_write_table, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                Approved_QC= lapply(file_name_to_write_table, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir_backup,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                             } 
                             pause = function(){
                               if (interactive()) {
                                  invisible(readline(prompt = "Press <Enter> to continue..."))}
                                else{cat("Press <Enter> to continue...")
                                  invisible(readLines(file("stdin"), 1))}
                                    closeAllConnections()}
                                    ps = pause()
                      
                      origindir = "."
                      targetdir = "./Norm"
                      targetdir_backup = "./QC_Backup"
                      list_of_annotated_files = list.files(path = ".", "*annotated.txt")
                      for (list_of_annotated_files in list_of_annotated_files) {                                     
                                      Annotated_files <- read.delim(list_of_annotated_files, header = TRUE, stringsAsFactor = F, check.names=FALSE)
                                      Exit_condition= grep("R4", names(Annotated_files), value = TRUE)                          
                                      if(length(Exit_condition) !=0){                        
                                         Approved_QC= lapply(list_of_annotated_files, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
                                         Approved_QC= lapply(list_of_annotated_files, function(x) file.copy(paste (origindir, x , sep = "/"), paste (targetdir_backup,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))}                  
                                         file.remove(list_of_annotated_files)
                      }
                      
                }        
                      

current.folder = "."
Roche_original_data = "./Roche_original_files"
              list.of.files_Roche_original <- list.files(current.folder, "*R4\\.txt", full.names=T) 
              file_cp=file.copy(list.of.files_Roche_original, Roche_original_data)        
junk <- dir(path = ".", pattern = "*R4\\.txt")
file.remove(junk)
list_of_annotated_files = list.files(path = ".", "*annotated.txt")
                      for (list_of_annotated_files in list_of_annotated_files) {                                     
                                      Annotated_files <- read.delim(list_of_annotated_files, header = TRUE, stringsAsFactor = F, check.names=FALSE)
                                      Exit_condition= grep("R4", names(Annotated_files), value = TRUE)                          
                                      if(length(Exit_condition) !=0){  
                                      file.remove(list_of_annotated_files)
                                      }                   
             }

 
}            
           



############################################### NORMALIZATION ###################################################################
#1: remove columns you do not need     


setwd("./Norm")


list.of.files_to_Norm <- list.files(".", "*combined_annotated.txt", full.names=F) 
for (list.of.files_to_Norm in list.of.files_to_Norm) {
  To_Norm = read.delim(list.of.files_to_Norm, header = TRUE, stringsAsFactor = F, check.names=FALSE)
  Exit_condition= grep("Conditional_SD_with_R4*", names(To_Norm), value = TRUE) 
   Length_exit_condition= length(Exit_condition) !=0
     if(Length_exit_condition[1] == TRUE){
       NameList = c("TARGET", "HK", "SD_new_with_R4", "Mean_new_with_R4",  "Conditional_SD_with_R4", "Conditional_Mean_with_R4", grep("*_Name", as.character(names(To_Norm)),value= TRUE ), "N_of_available_data_with_R4")
       col.num <- which(colnames(To_Norm) %in% NameList)
       To_Norm <- To_Norm[,sort(c(col.num))]
       SD_new_with_R4=as.data.frame(lapply(To_Norm$SD_new_with_R4,as.numeric))
       SD_new_with_R4= as.data.frame(t(SD_new_with_R4))
       rownames(SD_new_with_R4) = NULL
       names(SD_new_with_R4)= c("SD_new_with_R4")
       Conditional_SD_with_R4=as.data.frame(lapply(To_Norm$Conditional_SD_with_R4, as.numeric))
       Conditional_SD_with_R4= as.data.frame(t(Conditional_SD_with_R4))
       rownames(Conditional_SD_with_R4) = NULL
       names(Conditional_SD_with_R4)= c("Conditional_SD_with_R4")
       SD=apply(cbind(SD_new_with_R4,Conditional_SD_with_R4),1,sum,na.rm = FALSE)
           Mean_new_with_R4=as.data.frame(lapply(To_Norm$Mean_new_with_R4, as.numeric))
           Mean_new_with_R4= as.data.frame(t(Mean_new_with_R4))
           rownames(Mean_new_with_R4) = NULL
           names(Mean_new_with_R4)= c("Mean_new_with_R4")
           Conditional_Mean_with_R4=as.data.frame(lapply(To_Norm$Conditional_Mean_with_R4, as.numeric))
           Conditional_Mean_with_R4= as.data.frame(t(Conditional_Mean_with_R4))
           rownames(Conditional_Mean_with_R4) = NULL
           names(Conditional_Mean_with_R4)= c("Conditional_Mean_with_R4")
             Mean=apply(cbind(Mean_new_with_R4, Conditional_Mean_with_R4),1,sum,na.rm = FALSE)
                To_Norm=To_Norm[ , -which(names(To_Norm) %in% c("SD_new_with_R4","Conditional_SD_with_R4", "Mean_new_with_R4", "Conditional_Mean_with_R4"))]
                To_Norm$SD= SD
                To_Norm$Mean= Mean
                names(list.of.files_to_Norm) = gsub(pattern = "\\.txt$", "", list.of.files_to_Norm)
                file.names=strsplit(names(To_Norm[1]), "_")[[1]][1:3]
                file.names=paste(file.names, collapse = "_")  
                  write.table(To_Norm, paste(file.names, "_To_Normalize.txt", sep = ""),  col.names= T, row.names= F, quote= F, sep = "\t")
                  #file.remove(list.of.files_to_Norm)
              }else {

            list.of.files_to_Norm <- list.files(".", "*combined_annotated.txt", full.names=F) 
            for (list.of.files_to_Norm in list.of.files_to_Norm) {
            To_Norm <- read.delim(list.of.files_to_Norm, header = TRUE, stringsAsFactor = F, check.names=FALSE)
            #To_Norm <- read.delim("010317_Ex01_A_short_combined_annotated.txt", header = TRUE, stringsAsFactor = F, check.names=FALSE)
            Exit_condition= grep("Conditional_SD_with_R4$", names(To_Norm), value = TRUE)
            Length_exit_condition= length(Exit_condition) !=0
            if(Length_exit_condition[1] == FALSE) {
              NameList = c("TARGET", "HK", "SD", "Mean",  "Conditional_SD", "Conditional_Mean", grep("*_Name", as.character(names(To_Norm)),value= TRUE ), "N_of_available_data")
       col.num <- which(colnames(To_Norm) %in% NameList)
       To_Norm <- To_Norm[,sort(c(col.num))]
       SD=as.data.frame(lapply(To_Norm$SD,as.numeric))
       SD= as.data.frame(t(SD))
       rownames(SD) = NULL
       names(SD)= c("SD")
       Conditional_SD=as.data.frame(lapply(To_Norm$Conditional_SD, as.numeric))
       Conditional_SD= as.data.frame(t(Conditional_SD))
       rownames(Conditional_SD) = NULL
       names(Conditional_SD)= c("Conditional_SD")
       SD=apply(cbind(SD,Conditional_SD),1,sum,na.rm = FALSE)
           Mean=as.data.frame(lapply(To_Norm$Mean, as.numeric))
           Mean= as.data.frame(t(Mean))
           rownames(Mean) = NULL
           names(Mean)= c("Mean")
           Conditional_Mean=as.data.frame(lapply(To_Norm$Conditional_Mean, as.numeric))
           Conditional_Mean= as.data.frame(t(Conditional_Mean))
           rownames(Conditional_Mean) = NULL
           names(Conditional_Mean)= c("Conditional_Mean")
             Mean=apply(cbind(Mean, Conditional_Mean),1,sum,na.rm = FALSE)
                To_Norm=To_Norm[ , -which(names(To_Norm) %in% c("SD","Conditional_SD", "Mean", "Conditional_Mean"))]
                To_Norm$SD= SD
                To_Norm$Mean= Mean
                file.names=strsplit(names(To_Norm[1]), "_")[[1]][1:3]
                file.names=paste(file.names, collapse = "_")
                write.table(To_Norm, paste(file.names, "_To_Normalize.txt", sep = ""),  col.names= T, row.names= F, quote= F, sep = "\t")
                  #file.remove(list.of.files_to_Norm)
                
               } 
                list.of.files_to_Norm <- list.files(".", "*combined_annotated.txt", full.names=F) 
              for (list.of.files_to_Norm in list.of.files_to_Norm) {
              To_Norm = read.delim(list.of.files_to_Norm, header = TRUE, stringsAsFactor = F, check.names=FALSE)
              NameList = c("TARGET", "HK", "SD", "Mean", grep("*_Name", as.character(names(To_Norm)),value= TRUE ), "N_of_available_data")
              col.num <- which(colnames(To_Norm) %in% NameList)
              To_Norm <- To_Norm[,sort(c(col.num))]
              file.names=strsplit(names(To_Norm[1]), "_")[[1]][1:3]
              file.names=paste(file.names, collapse = "_")
              write.table(To_Norm, paste(file.names, "_To_Normalize.txt", sep = ""),  col.names= T, row.names= F, quote= F, sep = "\t")
                  
           }
          }     
        }
      }
list.of.files_to_Norm <- list.files(".", "*combined_annotated.txt", full.names=F) 
file.remove(list.of.files_to_Norm)
#setwd('..')
    

 ############################################## NORMALIZATION ###############################################################
                          
option_list = list(make_option(c("-k", "--k_param"), type = "integer", default = NULL, help = "the k parameter", metavar = "integer"))
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)
#print(opt$k_param)
                      list.of.files_to_Norm <- list.files(".", "*_To_Normalize.txt", full.names=F)      
                          for (list.of.files_to_Norm in list.of.files_to_Norm) {
                              Annotated_files <- read.delim(list.of.files_to_Norm, header = TRUE, stringsAsFactor = F, check.names=FALSE)
                              #Annotated_files <- read.delim("010317_Ex01_A_To_Normalize.txt", header = TRUE, stringsAsFactor = F, check.names=FALSE)
                              HK.sub <- subset(Annotated_files, Annotated_files$HK ==1)
                              HK.sub_HK_NAME= unique(HK.sub$TARGET)
                              Annotated_files=Annotated_files[ , grepl("*Name|TARGET|Mean", names(Annotated_files) ) ]
                              Annotated_files=Annotated_files[order(Annotated_files[,1]),]
                              Annotated_files=Annotated_files[ave(Annotated_files[,1], Annotated_files[,1], FUN = length) > 1, ]
                              Annotated_files=Annotated_files[order(Annotated_files$TARGET),]
                              Annotated_files<-subset(Annotated_files, Annotated_files$TARGET != "")
                              colnames_Norm_File=as.data.frame(unique(Annotated_files$TARGET))
                              spl=split(Annotated_files, Annotated_files$TARGET)
                              x=  do.call(cbind, lapply(spl, data.frame, stringsAsFactors=FALSE))  
                              pts= x[,1]
                              #rp = replicate(1, 1:length(pts))  
                              #pts= paste(pts,"_", rp, sep = "")
                              x=x[, -grep("*TARGET|*_Name", colnames(x))]                           
                              rownames(x) = pts                           
                              names(x) <- gsub(".Mean", "", names(x), fixed = TRUE)                             
                              names(x)=as.character(t(colnames_Norm_File))
                              HK= c("HPRT1", "GAPDH", "GUSB", "TBP") #Per ogni paziente bisogna fare la media degli HK
                              idx <- match(HK, names(x))
                              HK_DF <- x[,idx] 
                              mn_HK_per_pts= as.data.frame(apply(HK_DF, 1, mean)) #mean HK per pts for all the 4 HK                                                
                              #k=25.012586069
                              k=opt$k_param
                              SF=mn_HK_per_pts-k
                              m1 <- x == 35.01
                              x[] <- lapply(x, function(i) i-SF[,1])
                              x[m1] <- 35.01
                              x= x[, !names(x) %in% HK.sub_HK_NAME]
                              #x=-1*x[,1:ncol(x)] 2exp-deltaCt
                              #x= 2^(x[,1:ncol(x)]) 2exp-deltaCt
                              x= as.data.frame(cbind(rownames(x), x))
                              colnames(x)[1] <- "Patients"
                              list.of.files_to_Norm_renamed=gsub("_To_Normalize","",list.of.files_to_Norm)
                              list.of.files_to_Norm_renamed=gsub(".txt","", list.of.files_to_Norm_renamed)
                              write.table(x, paste(list.of.files_to_Norm_renamed, "_Normalized.txt", sep=""),  col.names= T, row.names= F, quote= F, sep = "\t")
                              file.remove(list.of.files_to_Norm)
}
#}
#}



























           











