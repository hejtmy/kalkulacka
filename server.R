# SEM a konfidencni interval

library(shiny)

shinyServer(function(input, output) {
  

# Interval spolehlivosti --------------------------------------------------
  
  ## distribuce
  dist <- reactive({
    if(input$CI_scale == "IQ") {
      c(100, 15, 0)
    } else if(input$CI_scale == "T") {
      c(50, 10, 0)
    } else if (input$CI_scale == "z") {
      c(0, 1, 2)
    } else if (input$CI_scale == "W") {
      c(10, 3, 0)
    } else if (input$CI_scale == "P") {
      c(0, 1, 0)
    } else {
      c(input$CI_M_manual, input$CI_SD_manual, 2)
    }
  })
  
  expT <- reactive({
    if (input$CI_scale == "IQ") {
      score <- input$CI_score_IQ
    } else if (input$CI_scale == "T") {
      score <- input$CI_score_T
    } else if (input$CI_scale == "z") {
      score <- input$CI_score_z
    } else if (input$CI_scale == "W") {
      score <- input$CI_score_W
    } else  if (input$CI_scale == "P") {
      if (input$CI_score_P > 0 & input$CI_score_P < 100) {
        score <- qnorm(input$CI_score_P/100)
      } else if (input$CI_score_P == 0) {
        score <- qnorm(.0025)
      } else {
        score <- qnorm(.9975)
      }
    } else {
      score <- input$CI_score_jine
    }
    if (isTRUE(input$CI_regrese)) {
      c(input$CI_reliability * score + (1-input$CI_reliability)*dist()[1], score)
    } else {
      c(score, score)
    }
  })

  # Potrebne vypocty  
  SEM <- reactive({
    dist()[2] * sqrt(1 - input$CI_reliability)
    
  })
  intWidth <- reactive({
    if (input$CI_width == "manual") {
      as.numeric(input$CI_widthManual)
    } else {
      as.numeric(input$CI_width)
    }
  })
  
  CI <- reactive({
    c(expT()[1] - qnorm((100 + intWidth()) / 200) * SEM(),  # pro qnorm byla potreba transformace 
      expT()[1] + qnorm((100 + intWidth()) / 200) * SEM())  # napr. 95% na 0.975
  }) 
  
  ## distribuce
  border <- reactive({
    if(input$CI_scale == "IQ") {
      c(min(CI()[1]-.2*dist()[2], 55), max(CI()[2]+.2*dist()[2], 145))
    } else if(input$CI_scale == "T") {
      c(min(CI()[1], 20)-.2*dist()[2], max(CI()[2]+.2*dist()[2], 80))
    } else if (input$CI_scale == "z") {
      c(min(CI()[1]-.2*dist()[2], -3), max(CI()[2]+.2*dist()[2], 3))
    } else if (input$CI_scale == "W") {
      c(min(CI()[1], 1)-.2*dist()[2], max(CI()[2]+.2*dist()[2], 19))
    } else if (input$CI_scale == "P") {
      c(-4, 4)
    } else {
      c(input$CI_M_manual-4*input$CI_SD_manual, input$CI_M_manual+4*input$CI_SD_manual)
    }
  })
  
  ########### OUTPUT ###############
  
  output$text_CI1 <- renderText({
    if (input$CI_scale == "P") {
      paste(sep="", "Standardní chyba měření: SEM = ", round(SEM(), 2), " (v z-skórech)\n",
            intWidth(), "% interval spolehlivosti: CI = [", round(pnorm(CI()[1])*100, dist()[3]), ", ", 
            round(pnorm(CI()[2])*100, dist()[3]), "] \n", 
            if (isTRUE(input$CI_regrese)) {
              paste("Odhad pravého percentilu: ", round(pnorm(expT()[1])*100, dist()[3]), "\n", sep="")
            },
            "Interpretace: Klient/ka dosáhl/a percentilu ", pnorm(expT()[2])*100, " s ", intWidth(), 
            "% intervalem spolehlivosti [", round(pnorm(CI()[1])*100, dist()[3]), ", ", 
            round(pnorm(CI()[2])*100, dist()[3]), "]."
      )
    } else {
      paste(sep="", "Standardní chyba měření: SEM = ", round(SEM(), dist()[3]), "\n",
            intWidth(), "% interval spolehlivosti: CI = [", round(CI()[1], dist()[3]), ", ", round(CI()[2], dist()[3]), "] \n", 
            if (isTRUE(input$CI_regrese)) {
              paste("Odhad pravého skóre: ", round(expT()[1], dist()[3]), "\n", sep="")
            },
            "<b>Interpretace:</b> Klient/ka dosáhl/a skóre ", expT()[2], " s ", intWidth(), 
            "% intervalem spolehlivosti [", round(CI()[1], dist()[3]), ", ", round(CI()[2], dist()[3]), "]."
      )
    }
    
  })
  
  output$plot_CI <- renderPlot({  # graf
    
    x <- seq(border()[1], border()[2], length=5000)
    
    y <- dnorm(x, expT()[1], SEM())
    y2 <- dnorm(x, dist()[1], dist()[2])
    
    if (input$CI_scale == "P") {
      plot(pnorm(x)*100, y, type="l", xlab="skóre", ylab="hustota pravděpodobnosti", lwd=2, col="green",
           main="Rozložení skóre")
      abline(v = pnorm(expT()[2])*100, lwd=2, col="black")
      abline(v = pnorm(expT()[1])*100, lwd=2, col="black", lty=2)
      
      i <- x >= CI()[1] & x <= CI()[2]
      polygon(pnorm(c(CI()[1], x[i], CI()[2]))*100, c(0, y[i], 0),   # vytvari barevne vyplneni distribuce
              col=rgb(0,1,0,0.3), border=NA)
      segments(pnorm(CI()[1])*100, 0, pnorm(CI()[2])*100, 0, col="orange", lwd=3)
      lines(c(0, 100), c(.1, .1), lwd=2, col="darkgray", lty = 2)
      legend("topright", legend=c("interval spolehlivosti", "populační rozložení", "pozorované skóre", "odhad pravého skóre"), 
             lwd=c(2, 2, 2, 2), lty=c(1,2,1,2), inset=.01,
             col = c("green", "darkgray", "black", "black"))
    } else {
      plot(x, y, type="l", xlab="skóre", ylab="hustota pravděpodobnosti", lwd=2, col="green",
           main="Rozložení skóre")
      abline(v = expT()[2], lwd=2, col="black")
      abline(v = expT()[1], lwd=2, col="black", lty=2)
      
      i <- x >= CI()[1] & x <= CI()[2]
      polygon(c(CI()[1], x[i], CI()[2]), c(0, y[i], 0),   # vytvari barevne vyplneni distribuce
              col=rgb(0,1,0,0.3), border=NA)
      segments(CI()[1], 0, CI()[2], 0, col="orange", lwd=3)
      lines(x, y2, lwd=2, col="darkgray", lty = 2)
      legend("topright", legend=c("interval spolehlivosti", "populační rozložení", "pozorované skóre", "odhad pravého skóre"), 
             lwd=c(2, 2, 2, 2), lty=c(1,2,1,2), inset=.01,
             col = c("green", "darkgray", "black", "black"))
    }
    
  })
  
})