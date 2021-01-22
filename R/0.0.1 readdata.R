######################说明##########################
### readdata xlsx，txt，csv数据读取函数
### 使用说明
# name 文件名
# sheet xlsx中sheet名
# rowNames 行名
# skip 跳行
# header 表头
# silent 是否显示详细报错
# errorstop 是否报错停止
### 返回数据说明
# data.frame
### R包使用

####xlsx读取####
readxlsx <- function(name,
                     sheet = 1,
                     rowNames = F,
                     startRow = 1,
                     colNames = T,
                     ...){

  # name <-
  # sheet <- 1
  # rowNames <- F
  # startRow <- 1
  # colNames <- T

  if(is.object(name)){ text <- paste0("读取",sheet,"数据")
  }else{text <- paste0("读取",name,"中",sheet,"数据")}
  print(text)

  data <- openxlsx::read.xlsx(xlsxFile = name,
                              sheet = sheet,
                              rowNames = rowNames,
                              startRow = startRow,
                              colNames = colNames,
                              ...)
  if(colNames){
    data1 <- openxlsx::read.xlsx(xlsxFile = name,
                                 sheet = sheet,
                                 colNames = F,
                                 rows = startRow,
                                 rowNames = rowNames,
                                 ...)
    if(dim(data)[2] != dim(data1)[2]){
      data1 <- openxlsx::read.xlsx(xlsxFile = name,
                                   sheet = sheet,
                                   colNames = F,
                                   rows = startRow,
                                   rowNames = F,
                                   ...)
    }
    names(data)<-data1
  }

  print("读取完毕")
  return(data)
}

####csv读取####
readcsv <- function(name,
                    rowNames = F,
                    skip = 0,
                    header = T,
                    ...){

  # name <- "./test/0.0.1 readdata/LM2019-0092-pos-M.csv"
  # rowNames <- F
  # skip <- 0
  # header <- T

  print(paste0("读取",name,"中数据"))

  data <- read.csv(file = name,
                   header = header,
                   check.names = F,
                   encoding = "UTF-8",
                   stringsAsFactors = F,
                   skip = skip,
                   row.names={if(rowNames){1}else{NULL}},
                   ...)

  if(header){
    if(names(data)[1] == "" |names(data)[2] == ""){
      warning(paste0("读取",skip,"行，表头为空，将读取",skip+1,"行作为表头"),immediate. = T)
      data <- readcsv(name = name,
                      rowNames = rowNames,
                      skip = skip+1,
                      header = header,
                      ...)
    }
  }

  print("读取完毕")
  return(data)
}

####读取xlsx，csv，txt文件####
readdata<-function(name,
                   sheet = 1,
                   rowNames = F,
                   skip = 0,
                   header = T,
                   silent = T,
                   errorstop = F,
                   ...){

  # name
  # sheet <- 1
  # rowNames <- F
  # skip <- 0
  # header <- T
  # silent <- T
  # errorstop <- F

  data <- NULL

  data_try<-try({

    if(is.character(name)){
      if(grepl(".txt$",name)){ data <- readcsv(name = name,
                                               rowNames = rowNames,
                                               skip = skip,
                                               header = header,
                                               sep = "\t",quote = "",...)
      }else if(grepl(".csv$",name)){ data <- readcsv(name = name,
                                                     rowNames = rowNames,
                                                     skip = skip,
                                                     header = header,
                                                     ...)
      }else if(grepl(".xlsx$",name)){ data <- readxlsx(name = name,
                                                       sheet = sheet,
                                                       rowNames = rowNames,
                                                       startRow = skip+1,
                                                       colNames = header,
                                                       ...)
      }else{
        stop(paste0(name,"文件名不符合格式"))
      }
    }else if(is.object(name)){
      data <- readxlsx(name = name,
                       sheet = sheet,
                       rowNames = rowNames,
                       startRow = skip+1,
                       colNames = header,
                       ...)
    }else{
      stop(paste0(name,"文件名不符合格式"))
    }

  },silent = silent)

  if(class(data_try) == "try-error" & errorstop){ stop(paste0(name,"文件未提取到数据"))
  }else if(class(data_try) == "try-error"){ warning(paste0(name,"文件未提取到数据"),immediate. = T)}

  return(data)
}

####读取多个文件####
readmuldata<-function(name,
                   sheet = 1,
                   rowNames = F,
                   skip = 0,
                   header = T,
                   silent = T,
                   errorstop = F,
                   ...){

  if( length(sheet) > 1 ){
    data <- NULL
    for ( i in 1:length(sheet)) {
      data1 <- readdata(name = name,
                        sheet = sheet[i],
                        rowNames = rowNames,
                        skip = skip,
                        header = header,
                        silent = silent,
                        errorstop = errorstop,
                        ...)
      data <- c(data,list(data1))
      names(data)[i] <- sheet[i]
    }

  }else{
    data<-readdata(name = name,
                   sheet = sheet,
                   rowNames = rowNames,
                   skip = skip,
                   header = header,
                   silent = silent,
                   errorstop = errorstop,
                   ...)
  }

  return(data)
}
