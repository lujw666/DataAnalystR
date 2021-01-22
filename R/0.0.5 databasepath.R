
# 系统位置
ospath <- function(path=NULL){
  #获取系统
  os <- R.Version()$os

  if(grepl("w32",os)){ path1 <- "//10.50.1.177/代谢/"
  }else if(grepl("linux",os)){ path1 <- "/public/"}

  if(!is.null(path)){ path1 <- paste0(path1,path)}

  return(path1)
}

# 数据库位置
databasepath <- function(database="LM-database/database/",path=""){

  path1 <- paste0(database,path)
  path1 <- ospath(path1)

  return(path1)
}

# shiny获取路径
where <- function(path = "",
                  shiny = T){

  if(shiny){
    library(shiny)
    app <- shinyApp(ui= bootstrapPage(
      textInput(inputId = "id",
                label = "工作目录:",
                value = path)),
      server=function(input, output, session) {
        session$onSessionEnded(function(){stopApp(isolate(input$id))})
      })
    serve <- runApp(app)
  }else{
    serve <- path
  }

  return(serve)
}

# 路径+系统
where_os <- function(path = "",
                     ...){
  wd <- where(path = path,...)
  wd <- ospath(path = wd)
  return(wd)
}

# 工作空间转移
gowhere <- function(path = "",...){
  wd <- where_os(path = path,...)

  print("移动工作空间到:")
  print(wd)

  if(file.exists(wd)){ setwd(wd)
  }else{ print("目录不存在")}

  print("现工作空间为:")
  print(getwd())
}

# 去包工作目录
gopackage <- function(){
  gowhere(path = "分析脚本/github/RMETA3",
          shiny = F)
}
