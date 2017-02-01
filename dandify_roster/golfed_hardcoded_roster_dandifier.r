library(xml2)
r=read_html(readline("Enter filename: "))
x=xml_text;a=xml_find_all;p=paste;b="/../following-sibling::";n="position()"
e=a(r,p("//th[b='CRN']",b,"*[1]/td[2]",sep=""))
g=function(m,s)x(a(r,p("(//th[b='Seq'])[",s,"]",b,"tr[",n,">1 and ",n,"<last()-1]/td[",match(m,y),"]",sep="")))
y=c("Seq","SID","Last Name","First Name","Level","Units","Class","Major","Grade","Status","Status Date","Email")
write.csv(Reduce(function(d,i)`if`(!length(g("Seq",i)),setNames(d,c("CRN","Section",y)),rbind(d,data.frame(x(e[i]),x(a(r,p("(//th[b='SEC'])[",i,"]",b,"*[1]/td[6]",sep=""))),sapply(y,g,i,simplify=F)))),seq(e),matrix(NA,0,14)),"roster.csv",row.names=F)
