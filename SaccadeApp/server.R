#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=400*1024^2) #Sets max file size to 400 MB
library(shiny)
library(rmatio)
library(signal)
library(pracma)
library(ggplot2)

maxVelocities <- function(dat){
  xpos = dat[["right_horizontal_eye"]][[1]]
  ypos = dat[["right_vertical_eye"]][[1]]
  time = dat[["eyelink_time"]][[1]]
  index = c(1:length(xpos))
  e = data.frame(index,xpos,ypos,time)
  e = na.omit(e)
  e$xpos = sgolayfilt(e$xpos,3,11)
  e$ypos = sgolayfilt(e$ypos,3,11)
  e$time = e$time - e$time[1]
  f = (e$xpos[2:dim(e)[1]]-e$xpos[1:dim(e)[1]-1])/(e$time[2:dim(e)[1]]-e$time[1:dim(e)[1]-1])
  g = (e$ypos[2:dim(e)[1]]-e$ypos[1:dim(e)[1]-1])/(e$time[2:dim(e)[1]]-e$time[1:dim(e)[1]-1])
  velocity = sqrt(f^2+g^2)
  #Find Max Sacchade Velocities;
  maxsacc = c();
  temp = velocity;
  n=200;
  m=1;
  prev = 0;
  s = dat[["PC_start_x"]][[1]] 
  en = dat[["PC_end_x"]][[1]] 
  stime = dat[["PC_trial_start"]][[1]] 
  while(n<length(velocity) && m<(length(s)+1)) {
    if(n<length(velocity) && !is.na(velocity[n]) && (velocity[n])>120 && (velocity[n])<1000) {
      beginning = n;
      start = s[m];
      end1 = en[m];
      time = stime[m];
      while(n<length(velocity)-4 && !is.na(velocity[n]) && (velocity[n])>100 && (velocity[n])<1000) {
        if(n<length(velocity)-4 && !is.na(velocity[n+1]) && !is.na(velocity[n]) && !(abs((velocity[n+1])-(velocity[n]))<200)) {#changed
          temp[n+1] = NA;
          if(velocity[n+2]>1000 || velocity[n+3]>1000) {
            if(velocity[n+2]>1000) {
              temp[n+2] = NA;
              if(velocity[n+3]>1000) {
                temp[n+3] = NA;
                n = n+4;
              }
              else {
                n = n+3;
              }
            }
            else if(velocity[n+3]>1000) {
              temp[n+3] = NA;
              n = n+4;
            }
          }
          else{
            n = n+2;
          }
        }
        else{
          n = n+1;
        }
      }
      velocity = temp;
      if(time-2<e$time[beginning] && time+2>e$time[beginning]) {
        if(!is.na(velocity[beginning]) && start-5<e$xpos[beginning] && start+5>e$xpos[beginning] && end1-4<e$xpos[n] && end1+4>e$xpos[n]) { 
          if(!is.na(max(velocity[beginning:n])) && max(velocity[beginning:n]) > 1000) {
            maxsacc = c(maxsacc,m,beginning,start-end1,NA);
            m = m+1;
            prev = 0;
          }
          else if(abs(e$xpos[n] - e$xpos[beginning])>2) {
            maxsacc = c(maxsacc,m,beginning,e$xpos[beginning]-e$xpos[n],max(velocity[beginning:n]));
            m = m+1;
            prev = 0;
          }
        }
      }
      else {
        if(time-2>e$time[beginning] || (time-2<e$time[beginning] && n<2000)) {
          n = n+500;
        }
        else {
          maxsacc = c(maxsacc,m,beginning,start-end1,NA);
          m = m+1;
          n = n-2000;
          prev = prev+1;
        }
      }
    }
    n=n+1;
}
  m = matrix(maxsacc,nrow=length(maxsacc)/4,ncol=4,byrow=TRUE)
  m
}
fitCurve<-function(dat){
  d = maxVelocities(dat[["data"]])
  displacement = d[,3]
  velocities = d[,4]
  df = data.frame(displacement,velocities)
  df = na.omit(df)
  f1 = fitPositive(df)
  f = fitNegative(df)
  f6 = rbind.data.frame(f,f1)
  f6
}
fitEqn<-function(df,x0){
  df1 = df[df$displacement>0,]
  xdata = df1$displacement
  fun = function(x,xdata) x[1]-x[1]/(1+x[2]*xdata)
  x = lsqcurvefit(fun,x0,df1$displacement,df1$velocities)
  x
}
fitPositive<-function(df){
  df1 = df[df$displacement>0,]
  xdata = df1$displacement
  fun = function(x,xdata) x[1]-x[1]/(1+x[2]*xdata)
  x0 = c(800,0.1)
  x = lsqcurvefit(fun,x0,df1$displacement,df1$velocities)
  b = x[["x"]]
  pred = fun(b,c(0:40))
  displacement = c(0:40)
  f1 = data.frame(displacement,pred)
  f1
}
fitNegative<-function(df){
  df2 = df[df$displacement<0,]
  xdata = df2$displacement
  fun = function(x,xdata) x[1]-x[1]/(1+x[2]*xdata)
  x0 = c(800,-0.1)
  x = lsqcurvefit(fun,x0,df2$displacement,df2$velocities)
  b = x[["x"]]
  pred = fun(b,c(-40:0))
  displacement = c(-40:0)
  f = data.frame(displacement,pred)
  f
}
comparison<-function(dat,dat1){
  #Find model parameters for data set 1
  f6 = fitCurve(dat)
  type = rep("Data Set 1",dim(f6)[1])
  f6 = cbind.data.frame(f6,type)
  #Find model parameters for data set 2
  f5 = fitCurve(dat1)
  type = rep("Data Set 2",dim(f5)[1])
  f5 = cbind.data.frame(f5,type)
  f = rbind.data.frame(f6,f5)
  f
}


shinyServer(function(input, output) {
  output$saccPlot1 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Data Set 1 Position Data",type="l")
  })
  output$saccPlot2 <- renderPlot({
    req(input$file2)
    dat = read.mat(input$file2$data)
    d = dat[["data"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Data Set 2 Position Data",type="l")
  })
  output$vigPlot1 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    f = fitPositive(df)
    f1 = fitNegative(df)
    ggplot(df,aes(displacement,velocities))+geom_point()+geom_line(data=f,aes(displacement,pred))+geom_line(data=f1,aes(displacement,pred))+xlab("Saccade Amplitude")+ylab("Saccade Vigor (Peak Velocity)")+ggtitle("Data Set 1 Saccade Vigor-Amplitude Profile")+annotate(geom="text",x=-20,y=200,label="Nasal",size=7)+annotate(geom="text",x=20,y=200,label="Temporal",size=7)
  })
  output$vigPlot2 <- renderPlot({
    req(input$file2)
    dat = read.mat(input$file2$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    f = fitPositive(df)
    f1 = fitNegative(df)
    ggplot(df,aes(displacement,velocities))+geom_point()+geom_line(data=f,aes(displacement,pred))+geom_line(data=f1,aes(displacement,pred))+xlab("Saccade Amplitude")+ylab("Saccade Vigor (Peak Velocity)")+ggtitle("Data Set 2 Saccade Vigor-Amplitude Profile")+annotate(geom="text",x=-20,y=200,label="Nasal",size=7)+annotate(geom="text",x=20,y=200,label="Temporal",size=7)
  })
  output$vigPlotParam1b <- renderText({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    x = fitEqn(df,c(800,0.1))
    params = x[["x"]]
    paste("Data Set 1 Parameters: a=",round(params[1],6),", b=",round(params[2],6))
  })
  output$vigPlotParam1a <- renderText({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    x = fitEqn(df,c(800,-0.1))
    params = x[["x"]]
    paste("Data Set 1 Nasal Parameters: a=",round(params[1],6),", b=",round(params[2],6))
  })
  output$vigPlotParam2b <- renderText({
    req(input$file2)
    dat = read.mat(input$file2$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    x = fitEqn(df,c(800,0.1))
    params = x[["x"]]
    paste("Data Set 2 Parameters: a=",round(params[1],6),", b=",round(params[2],6))
  })
  output$vigPlotParam2a <- renderText({
    req(input$file2)
    dat = read.mat(input$file2$data)
    d = maxVelocities(dat[["data"]])
    plot(c(0:10),c(0:10))
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    x = fitEqn(df,c(800,-0.1))
    params = x[["x"]]
    paste("Data Set 2 Nasal Parameters: a=",round(params[1],6),", b=",round(params[2],6))
  })
  output$vigPlot3 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    req(input$file2)
    dat1 = read.mat(input$file2$data)
    f = comparison(dat,dat1)
    ggplot(f,aes(displacement,pred,color=type))+geom_line()+ylab("Saccade Vigor (Peak Velocity)")+xlab("Saccade Amplitude")+ggtitle("Comparison of Saccade Vigor-Amplitude Profiles")+annotate(geom="text",x=-20,y=200,label="Nasal",size=7)+annotate(geom="text",x=20,y=200,label="Temporal",size=7)
  })
  output$vigComparison <- renderText({
    req(input$file1)
    dat = read.mat(input$file1$data)
    req(input$file2)
    dat1 = read.mat(input$file2$data)
    df1 = fitCurve(dat)
    df1 = df1$pred
    df2 = fitCurve(dat1)
    df2 = df2$pred
    a = t.test(df1,df2)
    b = paste(" From t-test comparison, p value = ",round(a[["p.value"]],6))
    if(a[["p.value"]]<0.05)
      paste("There is no significant difference between the data sets. ",b)
    else
      paste("There is a significant difference between the data sets. ",b)
  })
})
