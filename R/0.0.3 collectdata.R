#######################说明#########################
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


####选取列####
collectlist <- function(data,
                        needlist = NULL,
                        silent = F,
                        errorstop = T){
  if(!is.data.frame(data)){ stop("数据非dataframe格式")
  }else if(dim(data)[2] == 0){ stop("数据为空")}

  data1 <- data

  gettry <- try({
    if(is.null(needlist)){ data1 <- data
    }else if(is.numeric(needlist)){ data1 <-  data[,1:dim(data)[2] %in% needlist,drop = F]
    }else if(is.character(needlist)){ data1 <-  subset(data,select = needlist[needlist %in% names(data)])
    }else if(length(needlist) == 0){ stop("needlist为空")}

    if(any(duplicated(needlist)) & !is.null(needlist)){ warning("needlist输入有重复")
    }else if(dim(data1)[2] != length(needlist) & !is.null(needlist)){ warning("needgroup输入超出范围")}

    if (dim(data1)[2] == 0) { stop("未提取到需要的数据")}

  },silent = silent)

  if(class(gettry) == "try-error" & errorstop){ stop("数据为空")
  }else if(class(gettry) == "try-error"){ warning("数据为空",immediate. = T)}

  return(data1)
}

####选取行####
collectrange <- function(data,
                        range = NULL,
                        silent = F,
                        errorstop = T){
  if(!is.data.frame(data)){ stop("数据非dataframe格式")
  }else if(dim(data)[1] == 0){ stop("数据为空")}

  data1 <- NULL

  gettry <- try({
    if(is.null(range)){ data1 <- data
    }else if(is.numeric(range)){ data1 <-  data[1:dim(data)[1] %in% range,,drop = F]
    }else if(length(range) == 0){ stop("range为空")}

    if(any(duplicated(range)) & !is.null(range)){ warning("range输入有重复")
    }else if(dim(data1)[1] != length(range) & !is.null(range)){ warning("range输入超出范围")}

    if (dim(data1)[1] == 0) { stop("未提取到需要的数据")}

  },silent = silent)

  if(class(gettry) == "try-error" & errorstop){ stop("数据为空")
  }else if(class(gettry) == "try-error"){ warning("数据为空",immediate. = T)}

  return(data1)
}

####选取行加列####
collectdata <- function(data,
                        needlist = NULL,
                        range = NULL,
                        silent = F,
                        errorstop = T){
  data1 <- data

  if(is.data.frame(data1)){
    data1 <- collectlist(data = data1,
                         needlist = needlist,
                         silent = silent,
                         errorstop = errorstop)

    data1 <- collectrange(data = data1,
                          range = range,
                          silent = silent,
                          errorstop = errorstop)
  }else if(is.list(data1)){
    for ( i in 1:length(data1)) {
      data1[[i]] <- collectdata(data = data1[[i]],
                           needlist = needlist,
                           range = range,
                           silent = silent,
                           errorstop = errorstop)
    }
  }

  return(data1)
}


