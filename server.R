library(corrplot)
library(lmtest)
library(MASS)
options(shiny.maxRequestSize = 9*1024^2)

#sampleData<<-NULL
#sampleData<<-"HPRICE1.csv"
#sampleData<<-"C:/Program Files/RStudio/HPRICE1.csv"
#sampleData<<-"https://docs.google.com/uc?export=download&id=0B8dN5Cf4Mu-5Wk95b1dGakEyemM"

shinyServer(function(input, output,session) {

  dataInput <- reactive({
    if (is.null(input$file1$datapath)){
      inFile <<- input$file1;            
      inFile$datapath<<-"HPRICE1.csv"
    }else{
      inFile <<- input$file1;            
    }
    inputData<<-read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  })
  output$contents <- renderDataTable({
    inputData=dataInput()
    var<- colnames(inputData)
    updateSelectInput(session, "dVar",choices =var,selected=var[1])
    updateSelectInput(session, "indVar",choices =var,selected=var[2])
    updateSelectInput(session, "pVar",choices =var,selected=var[2])
    
    contents=inputData
  },options = list(pageLength = 10))
  
  output$summary <- renderTable({ 
    inputData=dataInput()    
    inputData<<-read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    summary(inputData, digits=2)
  })
  
  output$distPlot <- renderPlot({
    inputData=dataInput()    
    var<- colnames(inputData)    
    xlab=input$pVar
    ylab=input$dVar
    if (!is.null(xlab)){
      x <- inputData[, xlab]
      y <- inputData[, ylab]      
      plot(x,y,xlab=xlab, ylab=ylab)  
      reg1 <- lm(y~x)
      abline(reg1,col='blue')    
    }
  })
  
  output$corrPlot <- renderPlot({
    inputData=dataInput()    
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){    
      x <- inputData[xlab]
      y <- inputData[ylab]   
      corrplot(cor(cbind(y,x))) 
    }
  })
  
  output$formula <- renderUI({
    inputData=dataInput()    
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){    
      x <- inputData[xlab]
      y <- inputData[ylab] 
      fmla <- as.formula(paste(ylab," ~ ", paste(xlab,collapse= "+")))
      lm=lm(fmla,data=inputData)      
      rSquared=summary(lm)$r.squared
      arSquared=summary(lm)$adj.r.squared
      coef=round(coefficients(lm),digits=2)
      sign=ifelse(coef<0,"-","+")
      HTML(c(ylab,"=",paste(sign,abs(coef),c("",xlab)),"+u", '<br/><br/>', 
             "R-Squared=",round(rSquared,digits=2),"     R-Squared=",round(arSquared,digits=2),'<br/>'))    
    }
  })
  
  output$model <- renderTable({  
    inputData=dataInput()    
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){          
      x <- inputData[xlab]
      y <- inputData[ylab] 
      fmla <- as.formula(paste(ylab," ~ ", paste(xlab,collapse= "+")))
      lm=lm(fmla,data=inputData)
      summary(lm)
    }
  })
  
  output$multicollinearity <- renderUI({
    inputData=dataInput()    
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){    
      x <- inputData[xlab]
      y <- inputData[ylab] 
      fmla <- as.formula(paste(ylab," ~ ", paste(xlab,collapse= "+")))
      lm=lm(fmla,data=inputData)
      coef=round(coefficients(lm),digits=2)
      multicol=xlab[(match(NA,coef))-1]
      if (!is.na(multicol)){
        s1="Remove the variable"
      }else{s1=""}    
      HTML(c("<a href=\'http://en.wikipedia.org/wiki/Multicollinearity\' target=\"_blank\">1- Multicollinearity: </a>",multicol,'<br/>',
             '<span style="color:red">',s1,'</span>','<br/>'
      ))
    }
  })
  output$autocorrelation <- renderUI({
    inputData=dataInput()
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){    
      x <- inputData[xlab]
      y <- inputData[ylab] 
      fmla <- as.formula(paste(ylab," ~ ", paste(xlab,collapse= "+")))
      lm=lm(fmla,data=inputData)
      coef=round(coefficients(lm),digits=2)
      bp=bptest(lm)$p.value
      if (bp<.05){
        s2='<span style="color:red"> Use robust estimates'
        if(input$robust==TRUE){
          lm=rlm(fmla,data=inputData)
          coef=round(coefficients(lm),digits=2)
          sign=ifelse(coef<0,"-","+")        
          s2=c('Robust Model: ',c(ylab,"=",paste(sign,abs(coef),c("",xlab)),"+u"))
        }
      }else{s2=""}
      
      HTML(c("<a href=\'http://en.wikipedia.org/wiki/Autocorrelation\' target=\"_blank\">2- Autocorrelation:  </a>","Breusch–Pagan test= P-value of ",round(bp, digits=4),'<br/>',
             s2,'</span>'
      ))
    }
  })  
  
  output$functionalMiss <- renderUI({
    inputData=dataInput()
    xlab=input$indVar
    ylab=input$dVar
    if (!is.null(xlab)){          
      x <- inputData[xlab]
      y <- inputData[ylab] 
      fmla <- as.formula(paste(ylab," ~ ", paste(xlab,collapse= "+")))
      lm=lm(fmla,data=inputData)
      coef=round(coefficients(lm),digits=2)
      reset=resettest(lm)$p.value
      if (reset<.05){
        s3='<span style="color:red"> Change the functional form'

      }else{s3=""}
      
      HTML(c("<a href=\'http://en.wikipedia.org/wiki/Specification_%28regression%29\' target=\"_blank\">3- Functional misspecification: </a>","RESET test= P-value of ",round(reset, digits=4),'<br/>',
             s3,'</span>'
      ))
    }
  })  
 
  observe({
    inputData=dataInput()    
    var<- input$indVar
    updateSelectInput(session, "pVar",choices =var,selected=input$indVar[length(input$indVar)])
  })
  
  })

