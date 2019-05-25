.onLoad = function(libname, pkgname){
    for (package in c( "pROC","MBESS", "ez","BayesFactor" )) {
      library(package, character.only=T)
    }
}