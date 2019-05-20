.onLoad = function(libname, pkgname){
    for (package in c("data.table", "plyr", "pROC","MBESS", "ez", "psychReport","BayesFactor","TOSTER","ggplot2")) {
      if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
      }
    }
}