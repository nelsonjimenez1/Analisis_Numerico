i = 1
totE_x = 0
totE_y = 0
totE_z = 0
contx = 0
bool = 0
bool2 = 0
bool3 = 0
repeat
{
  if(!is.na(abs(xx[i]-XX[i])/xx[i])  && !is.infinite(abs(xx[i]-XX[i])/xx[i]))
    bool = 1
  if(!is.na(abs(yy[i]-YY[i])/yy[i]) && !is.infinite(abs(yy[i]-YY[i])/yy[i]))
    bool2 = 1
  if(!is.na(abs(zz[i]-ZZ[i])/zz[i]) && !is.infinite(abs(zz[i]-ZZ[i])/zz[i]))
    bool3 =  1
  
  if(bool == 1 && bool2 == 1 && bool3 == 1)
  {
    totE_x = totE_x + abs(xx[i]-XX[i])/xx[i]
    totE_y = totE_y + abs(yy[i]-YY[i])/yy[i]
    totE_z = totE_z + abs(zz[i]-ZZ[i])/zz[i]
    contx = contx  + 1
    bool = 0
    bool2 = 0
    bool3 = 0
    cat("error absoluto x ", abs(xx[i]-XX[i]), "y ",abs(yy[i]-YY[i]), "z ",abs(zz[i]-ZZ[i]),"error relativo x ",  abs(xx[i]-XX[i])/xx[i],"y " ,abs(yy[i]-YY[i])/yy[i], "z ",abs(zz[i]-ZZ[i])/zz[i],"i ",i,"\n")
  }
  
  i = i + 1
  
  if(length(XX)+1 == i)
  {
    break;
  }
}

totE_x
i = 1
j = 1
bool = 0
bool2 = 0
bool3 = 0
repeat
{
  if(!is.na(abs(xxx[i]-XXX[j])/xxx[i]) && !is.infinite(abs(xxx[i]-XXX[j])/xxx[i]))
    bool = 1
  if(!is.na(abs(yyy[i]-YYY[j])/yyy[i]) && !is.infinite(abs(yyy[i]-YYY[j])/yyy[i]))
    bool2 = 1
  if(!is.na(abs(zzz[i]-ZZZ[j])/zzz[i]) && !is.infinite(abs(zzz[i]-ZZZ[j])/zzz[i]))
    bool3 = 1
  
  if(bool == 1 && bool2 == 1 && bool3 == 1)
  {
    totE_x = totE_x + abs(xxx[i]-XXX[j])/xxx[i]
    totE_y = totE_y + abs(yyy[i]-YYY[j])/yyy[i]
    totE_z = totE_z + abs(zzz[i]-ZZZ[j])/zzz[i]
    contx = contx + 1
    bool = 0
    bool2 = 0
    bool3 = 0
    cat("error absoluto x ", abs(xxx[i]-XXX[j]), "y ",abs(yyy[i]-YYY[j]), "z ",abs(zzz[i]-ZZZ[j]),"error relativo x ",  abs(xxx[i]-XXX[j])/xxx[i],"y " ,abs(yyy[i]-YYY[j])/yyy[i], "z ",abs(zzz[i]-ZZZ[j])/zzz[i],"i ",i,"\n")
  }
  
  i = i + 1
  j = j + 1
  
  if(length(xxx)+1 == i)
  {
    i = 3300
    j = 15000
  }
  if(length(XXX)+1 == j)
  {
    break; 
  }
}

i = 1
j = 1
bool = 0
bool2 = 0
bool3 = 0
repeat
{
  if(!is.na(abs(xxxx[i]-XXXX[j])/xxxx[i]) && !is.infinite(abs(xxxx[i]-XXXX[j])/xxxx[i]))
    bool = 1
  if(!is.na(abs(yyyy[i]-YYYY[j])/yyyy[i]) && !is.infinite(abs(yyyy[i]-YYYY[j])/yyyy[i]))
    bool2 = 1
  if(!is.na(abs(zzzz[i]-ZZZZ[j])/zzzz[i]) && !is.infinite(abs(zzzz[i]-ZZZZ[j])/zzzz[i]))
    bool3 = 1
  
  if(bool == 1 && bool2 == 1 && bool3 == 1)
  {
    totE_x = totE_x + abs(xxxx[i]-XXXX[j])/xxxx[i]
    totE_y = totE_y + abs(yyyy[i]-YYYY[j])/yyyy[i]
    totE_z = totE_z + abs(zzzz[i]-ZZZZ[j])/zzzz[i]
    contx = contx + 1
    bool = 0
    bool2 = 0
    bool3 = 0
    cat("error absoluto x ", abs(xxxx[i]-XXXX[j]), "y ",abs(yyyy[i]-YYYY[j]), "z ",abs(zzzz[i]-ZZZZ[j]),"error relativo x ",  abs(xxxx[i]-XXXX[j])/xxxx[i],"y " ,abs(yyyy[i]-YYYY[j])/yyyy[i], "z ",abs(zzzz[i]-ZZZZ[j])/zzzz[i],"i ",i,"\n")
  }
  
  i = i + 1
  j = j + 1
  
  if(length(xxxx)+1 == i)
  {
    i = 4500
    j = 20000
  }
  if(length(XXXX)+1 == j)
  {
    break; 
  }
}

cat("Error relativo en x es ", totE_x/contx, "\n")
cat("Error relativo en y es ", totE_y/contx, "\n")
cat("Error relativo en z es ", totE_z/contx, "\n")

