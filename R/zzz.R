.onLoad = function(libname, pkgname){
    for (package in c("data.table", "plyr", "pROC","MBESS", "ez","BayesFactor","ggplot2")) {
      library(package, character.only=T)
    }
}