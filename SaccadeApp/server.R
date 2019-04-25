#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=150*1024^2) #Sets max file size to 100 MB
library(shiny)
library(rmatio)
library(signal)
# Define server logic required to draw a histogram
maxVelocities <- function(dat){
  "#for a = 1:length(fieldnames(data)) 
    #name = ['block' int2str(a)];
  #block = data.(name);
  unfilteredxpos = block.right_horizontal_eye; b = sgolayfilt(unfilteredxpos,3,11);
  d = sgolayfilt(block.right_vertical_eye,3,11);
  b = dat[[data]][[block1]][[right_horizontal_eye]][[1]]
d = dat[[data]][[block1]][[right_vertical_eye]][[1]]
 b = sgolayfilt(na.omit(b),3,11)   
d = sgolayfilt(na.omit(d),3,11)
  c = block.eyelink_time - block.eyelink_time(1);
  e = (c(2:end) - c(1,1));
  t = (d(2:end) - d(1:end-1)) ./ (c(2:end) - c(1:end-1));
  u = (b(2:end) - b(1:end-1)) ./ (c(2:end) - c(1:end-1));
  velocity = sqrt(t.^2 + u.^2);
  
  %%
  %Find Max Sacchade Velocities;
  maxsacc = [];
  temp = velocity;
  n=200;
  m=1;
  prev = 0;
  error = [];
  while(n<length(velocity) && m<(length(block.PC_start_x)+1))
  if(n<length(velocity) && (velocity(n))>120 && (velocity(n))<1000)
  beginning = n;
  start = block.PC_start_x(m);
  end1 = block.PC_end_x(m);
  time = block.PC_trial_start(m);
  while(n<length(velocity)-4 && (velocity(n))>100 && (velocity(n))<1000) %if bad point in middle of sacchade--breaks sacchade in two pieces
  if(n<length(velocity)-4 && ~(abs((velocity(n+1))-(velocity(n)))<200))
  temp(n+1) = NaN;
  if(velocity(n+2)>1000 || velocity(n+3)>1000)
  if(velocity(n+2)>1000)
  temp(n+2) = NaN;
  if(velocity(n+3)>1000)
  temp(n+3) = NaN;
  n = n+4;
  else
  n = n+3;
  end
  elseif(velocity(n+3)>1000)
  temp(n+3) = NaN;
  n = n+4;
  end 
  else
  n = n+2;
  end
  else
  n = n+1;
  end
  
  end
  if a==2
  error = [error;m beginning n start end1 b(beginning) b(n) prev max(velocity(beginning:n))];
  end
  velocity = temp;
  if(time-2<e(beginning) && time+2>e(beginning))
  if(~isnan(velocity(beginning)) && start-5<b(beginning) && start+5>b(beginning) && end1-4<b(n) && end1+4>b(n)) 
  if(max(velocity(beginning:n)) > 1000)
  maxsacc = [maxsacc;m NaN NaN start end1 NaN NaN NaN];
  m = m+1;
  prev = 0;
  elseif(abs(b(n) - b(beginning))>2)
  maxsacc = [maxsacc;m beginning n start end1 b(beginning) b(n) max(velocity(beginning:n))];
  m = m+1;
  prev = 0;
  end
  
  end
  else
  if(time-2>e(beginning) || (time-2<e(beginning) && n<2000))
  n = n+500;
  else
  maxsacc = [maxsacc;m NaN NaN start end1 NaN NaN NaN];
  m = m+1;
  n = n-2000;
  prev = prev+1;
  end
  end
  end
  n=n+1;
  end
  if(length(maxsacc)<block.PC_start_x)
  z = length(block.PC_start_x);
  for y = 1:block.PC_start_x-z
  start = block.PC_start_x(z+y);
  end1 = block.PC_end_x(z+y);
  maxsacc = [maxsacc;z+y NaN NaN start end1 NaN NaN NaN];
  end
  end
  save(['AnalyzedData' int2str(a) '.mat'],'maxsacc','b','e','velocity','counter','allsubjects');
  %saccades(a) = {maxsacc};
  clearvars -except data counter allsubjects saccades;
  end"
  
}


shinyServer(function(input, output) {
  output$saccPlot1 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block1"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 1")
  })
  output$saccPlot2 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block2"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 2")
  })
  output$saccPlot3 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block3"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 3")
  })
  output$saccPlot4 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block4"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 4")
  })
  output$saccPlot5 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block5"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 5")
  })
  output$saccPlot6 <- renderPlot({
    req(input$file1)
    dat = read.mat(input$file1$data)
    d = dat[["data"]][["block6"]][["right_horizontal_eye"]][[1]]
    d = sgolayfilt(na.omit(d),3,11)
    plot(d[input$start:input$end],ylab="Position",main="Block 6")
  })
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
