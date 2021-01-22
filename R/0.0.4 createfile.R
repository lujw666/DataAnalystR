#创建文件夹

dirfile <- function(filename,force=F){

  if(!file.exists(filename)){

    dir.create(filename,
               recursive = T,
               mode = "0777")

  }else{
    print(paste0(filename,"文件夹已存在"))

    if(force){

      print(paste0(filename,"文件夹强制删除重新创建"))
      unlink(filename,
             recursive = T,
             force = T)
      dir.create(filename,
                 recursive = T,
                 mode = "0777")
    }

  }

}

cpfile <- function(from,to){

  os <- R.Version()$os

  if(grepl("w32",os)){

    file.copy(from = dir(from, full.names = T),to = to,recursive = T)

  }else if(grepl("linux",os)){

    system(paste0("cp -r ",from,"/* '",to,"/'"))

  }

}
