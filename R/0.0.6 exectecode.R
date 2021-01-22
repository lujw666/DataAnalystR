ExecuteCode <- function(){
  os <- R.Version()$os

  usrdatabase <- getdatabase(databasepath = databasepath(path = "usr"),
                             database = "usr.db",
                             name = "usr")

  if(grepl("w32",os)){

    ip <- sub(pattern = ".*: ",
              replacement = "",
              x = grep(pattern = "IPv4",
                       x = system("ipconfig",intern = T),
                       value = T))

    if(ip %in% usrdatabase$IP){code <- usrdatabase$工号[usrdatabase$IP == ip]
    }else{code <- "LM0085"}

  }else if(grepl("linux",os)){
    username <- system("whoami",intern = T)

    if(username %in% usrdatabase$username){code <- usrdatabase$工号[usrdatabase$username == username]
    }else{code <- "LM0085"}

  }

  return(code)
}


ChoiceCode <- function(){

  usrdatabase <- getdatabase(databasepath = databasepath(path = "usr"),
                             database = "usr.db",
                             name = "usr")
  code <- usrdatabase$工号
  code <- unique(code)

  return(code)

}
