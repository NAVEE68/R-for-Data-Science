#function for accuracy
acc<-function(cf1){
  Totp<-cf1[2,1]+cf1[2,2]
  TP<-cf1[2,2]
  c<-TP/Totp
  c
}