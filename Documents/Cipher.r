cipher <- function(string, seed = 0){
 alph <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
 if(seed == 0){
   seed <- sample(1:1000000, 1)
 } else {seed=seed
 }
 set.seed(seed)
 ciph <- sample(alph,26, replace=FALSE)
 string <- tolower(string)
 n <- nchar(string)
 new <- rep(0, n)

 for(i in 1:n){
   t <- substr(string,i, i)
   if(t %in% alph){
     j <- grep(t, alph)
     new[i] <- ciph[j]
   }else{new[i]<-t 
   }
 }
 cat(new, sep= "")
 return(seed)
}


#decipher doesnt work yet

decipher <- function(string, seed){
  alph <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  string <- tolower(string)
  set.seed(seed)
  ciph <- sample(alph, 26, replace = FALSE)
  n <- nchar(string)
  new <- rep(0, n)
  for(i in 1:n){
    t <- substr(string,i, i)
    if(t %in% alph){
      j <- grep(t, ciph)
      new[i] <- alph[j]
    }else{new[i]<-t 
    }
  }
  cat(new, sep= "")
}


#try it with a predetermined seed
secret <- "This is a Secret Code"
cipher(secret, 3)

uhoh<-"lfky ky e ynjgnl jqhn"
decipher("lfky ky e ynjgnl jqhn",3)


#not specified seed

secret <- "This is a Secret Code"
seed<-cipher(secret)
uhoh<-"rcti ti j iqhxqr hseq"
decipher(uhoh,seed)

