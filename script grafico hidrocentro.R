# funcion error de medicion
# error <- function(med, mayor=TRUE){
#     dif1 <- med[,2]-med[,1]
#     dif2 <- med[,4]-med[,3]
#     if(mayor==TRUE){
#         dvm <- dif1-dif2
#         er <- ifelse(dif2==0, 0, 100*(dvm/dif2))
#     }else{
#         dvm <- dif2-dif1
#         er <- ifelse(dif1==0, 0, 100*(dvm/dif1))
#     }
#     return(er)
# }



# Informacion medidor
# assign("modelo", input$mod, envir=.GlobalEnv)
# assign("serie", input$serie, envir=.GlobalEnv)
# assign("diametro", input$diam, envir=.GlobalEnv)
# assign("clase", input$clase, envir= .GlobalEnv)

# Informacion caudales Q
# assign("qmin", input$qmin, envir= .GlobalEnv)
# assign("qt", input$qt, envir= .GlobalEnv)
# assign("qn", input$qn, envir= .GlobalEnv)
# assign("qmax", input$qmax, envir= .GlobalEnv)

# mpli<- c(input$mpqmini,input$mpqti,input$mpqni,input$mpqmaxi)
# mplf<- c(input$mpqminf,input$mpqtf,input$mpqnf,input$mpqmaxf)
# muli<- c(input$muqmini,input$muqti,input$muqni,input$muqmaxi)
# mulf<- c(input$muqminf,input$muqtf,input$muqnf,input$muqmaxf)
# med <- data.frame(mpli,mplf,muli,mulf)
# mayor <- FALSE
# if(input$vm3=="mayor") mayor <- TRUE
# er <- error(med, mayor)

# Informacion mediciones lectura inicial y final
# mediciones <- data.frame(med,er)
# colnames(mediciones)<- c(colnames(med),"error")
# assign("mediciones",mediciones,envir=.GlobalEnv)

q0 <- c(qmin,qt,qn,qmax)
# cambio escala
q0[2] <- q0[2]*(0.325*q0[4]/120)
q0[3] <- q0[3]*(0.75*q0[4]/1500)
q <- q0[c(1,2,2,3,4)]
lim <- c(rep(5,2),rep(2,3))
d <- data.frame(q,lim)

g <- ggplot(d, aes(x=q,y=lim))+
    geom_line(colour="gray99",size=1)+
    annotate("segment", x=d[1:3,1] ,y=-d[1:3,2], xend=d[c(2,3,5),1],
             yend=-d[c(2,3,5),2], colour="gray99", size=0.7)+
    annotate("rect", xmin=d[c(1,3),1],ymin=-d[c(1,3),2], xmax=d[c(2,5),1],
             ymax=d[c(2,5),2], fill="gray95",alpha=0.6)+
    
    annotate("rect", xmin=d[2:3,1],ymin=c(-d[2,2],d[3,2]), xmax=d[c(5,5),1],
             ymax=c(-d[5,2],d[1,2]), fill="dodgerblue4",alpha=0.9)+
    annotate("rect", xmin=d[c(1,1),1],ymin=c(-d[1,2]-1,d[1,2]), xmax=d[c(5,5),1],
             ymax=c(-d[1,2],d[1,2]+1), fill="dodgerblue4",alpha=0.9)+
    
    annotate("segment", x=d[1,1] ,y=0, xend=d[5,1], yend=0, colour="gray0",
             size=0.7, linetype="dotted")+
    
    annotate("pointrange", x=d[-3,1], y=er, ymin=er, ymax=er,
             colour = "red", size = 1, alpha=0.7)+
    annotate("segment", x=d[-c(3,5),1], y=er[-4], xend=d[-c(1,3),1], yend=er[-1],
             colour = "red", size = 1, alpha=0.7)+
    
    theme_bw()
print(g)

