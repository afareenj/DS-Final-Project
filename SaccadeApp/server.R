#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=200*1024^2) #Sets max file size to 200 MB
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
  s = dat[["PC_start_x"]][[1]] #block.PC_start_x
  en = dat[["PC_end_x"]][[1]] #block.PC_end_x
  stime = dat[["PC_trial_start"]][[1]] #block.PC_trial_start
  while(n<length(velocity) && m<(length(s)+1)) {
    if(n<length(velocity) && !is.na(velocity[n]) && (velocity[n])>120 && (velocity[n])<1000) {#changed 
      beginning = n;
      start = s[m];
      end1 = en[m];
      time = stime[m];
      while(n<length(velocity)-4 && !is.na(velocity[n]) && (velocity[n])>100 && (velocity[n])<1000) {#changed 
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
  for (b in 1:dim(m)[1]){
    if(m[b,3]<0){
      m[b,4]=-m[b,4]
    }
  }
  m
}


shinyServer(function(input, output) {
  output$vigPlot1 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d1 = maxVelocities(dat[["DATA_ALL"]][["block1"]])
    d2 = maxVelocities(dat[["DATA_ALL"]][["block2"]])
    d3 = maxVelocities(dat[["DATA_ALL"]][["block3"]])
    d4 = maxVelocities(dat[["DATA_ALL"]][["block4"]])
    d = rbind(d1,d2,d3,d4)
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    df1 = df[df$displacement>0,]
    xdata = df1$displacement
    fun = function(x,xdata) x[1]*x[2]*xdata/(1+x[2]*xdata)
    x0 = c(1000,0.1)
    x = lsqcurvefit(fun,x0,df1$displacement,df1$velocities)
    b = x[["x"]]
    pred1 = fun(b,c(0:40))
    f1 = data.frame(c(0:40),pred1)
    df2 = df[df$displacement<0,]
    xdata = df2$displacement
    fun = function(x,xdata) x[1]*x[2]*xdata/(1+x[2]*xdata)
    x0 = c(-1000,-0.1)
    x = lsqcurvefit(fun,x0,df2$displacement,df2$velocities)
    b = x[["x"]]
    pred = fun(b,c(-40:0))
    f = data.frame(c(-40:0),pred)
    ggplot(df,aes(displacement,velocities))+geom_point()+geom_line(data=f,aes(c..40.0.,pred))+geom_line(data=f1,aes(c.0.40.,pred1))
    
  })
  output$vigPlot2 <- renderPlot({
    req(input$file2)
    dat = read.mat(input$file2$data)
    d1 = maxVelocities(dat[["DATA_ALL"]][["block1"]])
    d2 = maxVelocities(dat[["DATA_ALL"]][["block2"]])
    d3 = maxVelocities(dat[["DATA_ALL"]][["block3"]])
    d4 = maxVelocities(dat[["DATA_ALL"]][["block4"]])
    d = rbind(d1,d2,d3,d4)
    displacement = d[,3]
    velocities = d[,4]
    df = data.frame(displacement,velocities)
    df = na.omit(df)
    df1 = df[df$displacement>0,]
    xdata = df1$displacement
    fun = function(x,xdata) x[1]*x[2]*xdata/(1+x[2]*xdata)
    x0 = c(1000,0.1)
    x = lsqcurvefit(fun,x0,df1$displacement,df1$velocities)
    b = x[["x"]]
    pred1 = fun(b,c(0:40))
    f1 = data.frame(c(0:40),pred1)
    df2 = df[df$displacement<0,]
    xdata = df2$displacement
    fun = function(x,xdata) x[1]*x[2]*xdata/(1+x[2]*xdata)
    x0 = c(-1000,-0.1)
    x = lsqcurvefit(fun,x0,df2$displacement,df2$velocities)
    b = x[["x"]]
    pred = fun(b,c(-40:0))
    f = data.frame(c(-40:0),pred)
    ggplot(df,aes(displacement,velocities))+geom_point()+geom_line(data=f,aes(c..40.0.,pred))+geom_line(data=f1,aes(c.0.40.,pred1))
  })
  output$saccPlot1 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["DATA_ALL"]][["block1"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 1")
  })
  
  
})
