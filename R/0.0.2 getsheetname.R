####################说明############################
### collectsheetname xlsx，txt，csv数据读取sheet函数
### 使用说明
# name 文件名
# needgroup 选取sheet
# silent 是否显示详细报错
# errorstop 是否报错停止
### 返回数据说明
# 向量或列表
### R包使用
# openxlsx

#####sheet名提取####
sheetname <- function(name,
                      silent = T,
                      errorstop = T){

  # name <- "./test/0.0.1 readdata/数据矩阵.xlsx"
  # name <- "./test/0.0.1 readdata/LM2019-0092-neg-M.csv"
  # silent <- T
  # errorstop <- F

  group <- "1"

  group_try <- try({

    if(is.character(name)){
      if(grepl(".txt$",name)){ group <- "1"
      }else if(grepl(".csv$",name)){ group <- "1"
      }else if(grepl(".xlsx$",name)){ group <- openxlsx::getSheetNames(file = name)
      }else{ stop("非xlsx，csv，txt文件，使用默认值1")}
    }else if(is.object(name)){ group <- openxlsx::sheets(wb = name)}

  },silent = silent)


  if(class(group_try) == "try-error" & errorstop){ stop(paste0(name,"文件未提取到sheet"))
  }else if(class(group_try) == "try-error"){ warning(paste0(name,"文件未提取到sheet,使用默认值1"),immediate. = T)}

  return(group)
}

####sheet名选取####
getgroup <- function(group,
                     needgroup = NULL,
                     silent = F,
                     errorstop = T){
  # group <- "1"
  # needgroup <- "2"
  # silent <- T
  # errorstop <- F

  group1 <- group

  gettry <- try({

    if (length(group1) == 0) { stop("group为空")}

    if(is.null(needgroup)){
    }else if(is.numeric(needgroup)){ group1 <-  group[1:length(group) %in% needgroup]
    }else if(is.character(needgroup)){ group1 <-  group[group %in% needgroup]
    }else if(length(needgroup) == 0){ stop("needgroup为空")
    }else if(is.list(needgroup)){
      group1 <- list()
      for ( i in 1:length(needgroup)) {
        group2 <- getgroup(group = group,needgroup = needgroup[[i]])
        group1 <- c(group1,list(group2))
      }
    }

    if(any(duplicated(needgroup)) & !is.list(needgroup) & !is.null(needgroup)){ warning("needgroup输入有重复")
    }else if(length(group1) != length(needgroup) & !is.null(needgroup)){ warning("needgroup输入超出范围")}

    if (length(group1) == 0) { stop("未提取到需要的group")}

  },silent = silent)

  if(class(gettry) == "try-error" & errorstop){ stop("group为空")
  }else if(class(gettry) == "try-error"){ warning("group为空",immediate. = T)}

  return(group1)
}

####sheet名提取及选取####
collectsheetname <- function(name,
                             needgroup = NULL,
                             silent = T,
                             errorstop = T){

  # name <- "./test/0.0.1 readdata/数据矩阵.xlsx"
  # needgroup <- NULL
  # silent <- T
  # errorstop <- F

  group <- sheetname(name = name,
                     silent = silent,
                     errorstop = errorstop)

  group <- getgroup(group = group,
                    needgroup = needgroup,
                    silent = silent,
                    errorstop = errorstop)

  return(group)
}
