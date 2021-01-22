#用于文件的保存
#保存xlsx文件，当文件存在时，添加sheet


#首行加粗，字体Arial
savexlsx1<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  openxlsx::writeData(wb,sheet=sheet1,data)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}

#无格式
savexlsx2<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  openxlsx::writeData(wb,sheet=sheet1,data)
  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}


#首行首列加粗，字体Arial
savexlsx3<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  openxlsx:: writeData(wb,sheet=sheet1,data,rowNames = TRUE)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:(dim(data)[2]+1),gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=2:(dim(data)[1]+1),cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 2:(dim(data)[2]+1),gridExpand=T)
  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}


#首行加粗，字体Arial,部分字体变红
savexlsx4<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  openxlsx::writeData(wb,sheet=sheet1,data)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  mode3<-openxlsx::createStyle(fontName = "Arial",fontColour = "red")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode3,rows=which(is.na(data[,"URL"]))+1,cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}


#首行加粗，字体Arial
savexlsx5<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }
  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  openxlsx:: writeData(wb,sheet=sheet1,data,rowNames = TRUE)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:(dim(data)[2]+1),gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=2:(dim(data)[1]+1),cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 2:(dim(data)[2]+1),gridExpand=T)
  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}



addsheet1<-function(data=data,wb=wb,sheet=sheet){
  if(is.null(data)){return(wb)}
  if(dim(data)[1]==0){return(wb)}

  wb1<-wb

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  openxlsx::addWorksheet(wb1,sheet1)
  openxlsx::writeData(wb1,sheet=sheet1,data)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb1,sheet=sheet1,style = mode1,rows=1,cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb1,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 1:dim(data)[2],gridExpand=T)

  return(wb1)
}


savexlsx6<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,"说明")
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,"说明")
    openxlsx::addWorksheet(wb,"说明")
  }

  openxlsx::writeData(wb,sheet=sheet1,data)
  openxlsx::writeData(wb,sheet="说明",data.frame("说明"=c("绿色背景为富集到通路的差异代谢物","蓝色背景为有KEGG ID，但没有富集到通路的差异代谢物","无背景色为没有KEGG ID的差异代谢物")))
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  mode3<-openxlsx::createStyle(fontName = "Arial",fgFill= "palegreen")
  mode4<-openxlsx::createStyle(fontName = "Arial",fgFill= "powderblue")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode4,rows=c(2:(dim(data)[1]+1))[!is.na(data[,"KEGG"])],cols = 1:dim(data)[2],gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode3,rows=c(2:(dim(data)[1]+1))[!is.na(data[,"ID Annotation"])],cols = 1:dim(data)[2],gridExpand=T)

  openxlsx::addStyle(wb,sheet="说明",style = mode1,rows=1,cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet="说明",style = mode2,rows=4,cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet="说明",style = mode4,rows=3,cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet="说明",style = mode3,rows=2,cols = 1,gridExpand=T)


  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}


savexlsx7<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }


  openxlsx::writeData(wb,sheet=sheet1,data)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:(dim(data)[2]+1),gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=2:(dim(data)[1]+1),cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 2:(dim(data)[2]+1),gridExpand=T)

  mode3<-openxlsx::createStyle(fontName = "Arial",fgFill= "red")
  for (i in 1:dim(data)[1]) {
    for (j in 1:dim(data)[2]) {
      if(is.na(data[i,j])){
        openxlsx::addStyle(wb,sheet=sheet1,style = mode3,rows=i+1,cols =j,gridExpand=T)
      }
    }
  }

  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}

savexlsx8<-function(data=data,name=name,sheet=sheet){
  if(is.null(data)){return()}
  if(dim(data)[1]==0){return()}
  text<-paste0("在",name,"中保存",sheet,"表格")
  print(text)
  if(file.exists(name)){
    wb<-openxlsx::loadWorkbook(name)
  }else{wb<-openxlsx::createWorkbook()}

  if(nchar(sheet)>31){
    maxn<-floor(nchar(sheet)/3)
    n<-ceiling((nchar(sheet)-31)/3)
    sheet1<-paste0(substr(sheet,1,maxn-n),substr(sheet,maxn+1,2*maxn-n),substr(sheet,2*maxn+1,3*maxn-n))
  }else{
    sheet1<-sheet
  }

  tryfetch<-try({
    openxlsx::addWorksheet(wb,sheet1)
  },silent = T)
  if(class(tryfetch)=="try-error"){
    openxlsx::removeWorksheet(wb,sheet1)
    openxlsx::addWorksheet(wb,sheet1)
  }


  openxlsx::writeData(wb,sheet=sheet1,data)
  mode1<-openxlsx::createStyle(fontName = "Arial",textDecoration = "bold")
  mode2<-openxlsx::createStyle(fontName = "Arial")
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=1,cols = 1:(dim(data)[2]+1),gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode1,rows=2:(dim(data)[1]+1),cols = 1,gridExpand=T)
  openxlsx::addStyle(wb,sheet=sheet1,style = mode2,rows=2:(dim(data)[1]+1),cols = 2:(dim(data)[2]+1),gridExpand=T)

  mode3<-openxlsx::createStyle(fontName = "Arial",fgFill= "red")
  for (i in 1:dim(data)[1]) {
    for (j in 1:dim(data)[2]) {
      if(isFALSE(data[i,j])){
        openxlsx::addStyle(wb,sheet=sheet1,style = mode3,rows=i+1,cols =j,gridExpand=T)
      }
    }
  }

  mode4<-openxlsx::createStyle(fontName = "Arial",fgFill= "green")
  for (i in 1:dim(data)[1]) {
    for (j in 1:dim(data)[2]) {
      if(isTRUE(data[i,j])){
        openxlsx::addStyle(wb,sheet=sheet1,style = mode4,rows=i+1,cols =j,gridExpand=T)
      }
    }
  }

  openxlsx::saveWorkbook(wb, file = name, overwrite = TRUE)
  print("保存完毕")
}

