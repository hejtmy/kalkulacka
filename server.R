# SEM a konfidencni interval

library(shiny)
library(ggplot2)
library(grid)




# Funkce: Převod skórů ----------------------------------------------------

`%then%` <- shiny:::`%OR%`

# Error message
chybovaHlaska <- "POZOR, byla zadána neplatná vstupní hodnota.
(V případě zadávání čísel s desetinnými místy, používejte tečku na místo desetinné čárky)\n\n"
chybovaHlaskaSten <- "POZOR, byla zadána neplatná vstupní hodnota. 
Sten může nabývat pouze hodnot od 1 do 10.
\n\n"
chybovaHlaskaStanin <- "POZOR, byla zadána neplatná vstupní hodnota. 
Stanin může nabývat pouze hodnot od 1 do 9.
\n\n"
chybovaHlaskaPerc <- "POZOR, byla zadána neplatná vstupní hodnota. 
Percentil zde může nabývat pouze hodnot od 0 do 100 (okrajové hodnoty jsou zaokrouhleny na 0,25, resp. 99,75).
\n\n"
chybovaHlaskaWech <- "POZOR, byla zadána neplatná vstupní hodnota. 
Wechsler skór  může nabývat pouze hodnot od 1 do 19.
\n\n"


# Predefined functions
iqOut <- function(zSkor) {
  100 + 15 * zSkor
}

percOut <- function(zSkor) {
  round(pnorm(zSkor) * 100, 2)
}

tOut <- function(zSkor) {
  50 + 10 * zSkor
}

wechOut <- function(zSkor) {
  10 + 3 * zSkor
}

stenOut <- function(zSkor) {
  temp1 <- 2 * zSkor + 5.5
  
  if (temp1 %% 1 == 0.5) {  # par kroku navic kvuli divnemu R zaokrouhlovani
    temp2 <- temp1 + .5
  } else {
    temp2 <- round(temp1)
  }
  
  if (temp2 < 1) {  # ohraniceni vystupniho hodnot na 1-10
    1
  } else if (temp2 > 10) {
    10 
  } else {
    temp2
  }
}

staninOut <- function(zSkor) {
  temp1 <- 2 * zSkor + 5
  
  if (temp1 %% 1 == 0.5) {
    temp2 <- temp1 + .5
  } else {
    temp2 <- round(temp1)
  }
  
  if (temp2 < 1) {
    1
  } else if (temp2 > 9) {
    9 
  } else {
    temp2
  }
}


# Kalkulačky --------------------------------------------------------------



shinyServer(function(input, output, session) {
  

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
      c(-5, 5)
    } else {
      c(input$CI_M_manual-4*input$CI_SD_manual, input$CI_M_manual+4*input$CI_SD_manual)
    }
  })
  

# * output ----------------------------------------------------------------
  
  output$text_CI1 <- renderText({
    if (input$CI_scale == "P") {
      paste(sep="", "<b>Standardní chyba měření:</b> SEM = ", round(SEM(), 2), " (v z-skórech)<br />",
            "<b>", intWidth(), "% interval spolehlivosti:</b> CI = [", round(pnorm(CI()[1])*100, dist()[3]), ", ", 
            round(pnorm(CI()[2])*100, dist()[3]), "] <br />", 
            if (isTRUE(input$CI_regrese)) {
              paste("<b>Odhad pravého percentilu:</b> ", round(pnorm(expT()[1])*100, dist()[3]), "<br />", sep="")
            },
            "<b>Interpretace:</b> Klient/ka dosáhl/a percentilu ", input$CI_score_P, " s ", intWidth(), 
            "% intervalem spolehlivosti [", round(pnorm(CI()[1])*100, dist()[3]), ", ", 
            round(pnorm(CI()[2])*100, dist()[3]), "]."
      )
    } else {
      paste(sep="", "<b>Standardní chyba měření:</b> SEM = ", round(SEM(), dist()[3]), "<br />",
          "<b>", intWidth(), "% interval spolehlivosti:</b> CI = [", round(CI()[1], dist()[3]), ", ", round(CI()[2], dist()[3]), "]<br />", 
            if (isTRUE(input$CI_regrese)) {
              paste("<b>Odhad pravého skóre:</b> ", round(expT()[1], dist()[3]), "<br />", sep="")
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
      plot(pnorm(x)*100, y, type="l", ylab="hustota pravděpodobnosti", xlab="skóre", yaxt="n", lwd=2, col="green",
           main="Rozložení skóre")
      abline(v = pnorm(expT()[2])*100, lwd=2, col="black")
      abline(v = pnorm(expT()[1])*100, lwd=2, col="black", lty=2)
      
      i <- x >= CI()[1] & x <= CI()[2]
      polygon(pnorm(c(CI()[1], x[i], CI()[2]))*100, c(0, y[i], 0),   # vytvari barevne vyplneni distribuce
              col=rgb(0,1,0,0.3), border=NA)
      segments(pnorm(CI()[1])*100, 0, pnorm(CI()[2])*100, 0, col="orange", lwd=3)
      lines(c(0, 100), c(.4, .4), lwd=2, col="darkgray", lty = 2)
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
  
  

# Převod skórů ------------------------------------------------------------



  
    sessionId <- floor(runif(1, 1, 1e+04))
    
    stenStan <- reactive({
      input$test %in% c("sten", "stanin") 
    })
    
    # Nejpreve prevede vsechny skory na z-skor, potom se s nimi pracuje hromadne
    # A take kontrola zadani skoru, pokud jsou mimo rozsah, pak vyhodi chybu
    zOut <- reactive({  
      
      if (input$test == "z") {
        validate(need(!is.na(input$zValue), chybovaHlaska))
        input$zValue
      } else if (input$test == "iq") {
        validate(need(!is.na(input$iqValue), chybovaHlaska))
        (input$iqValue - 100) / 15 
      } else if (input$test == "t") {
        validate(need(!is.na(input$tValue), chybovaHlaska))
        (input$tValue - 50) / 10 
      } else if (input$test == "wech") {
        validate(need(!is.na(input$wechValue), chybovaHlaska) %then%
                   need(1 <= input$wechValue && input$wechValue <= 19, chybovaHlaskaWech))
        (input$wechValue - 10) / 3   
      } else if (input$test == "perc") {
        validate(need(!is.na(input$percValue), chybovaHlaska) %then%
                   need(0 <= input$percValue && input$percValue <= 100, chybovaHlaskaPerc))
        if (input$percValue == 0) {
          qnorm(0.25 / 100)
        } else if (input$percValue == 100) {
          qnorm(99.75 / 100)
        } else {
          qnorm(input$percValue / 100)
        }
        
      } else if (input$test == "sten") {
        validate(need(!is.na(input$stenValue), chybovaHlaska) %then%
                   need(1 <= input$stenValue && input$stenValue <= 10, chybovaHlaskaSten))
        (input$stenValue - 5.5) / 2
      } else if (input$test == "stanin") {
        validate(need(!is.na(input$staninValue), chybovaHlaska) %then%
                   need(1 <= input$staninValue && input$staninValue <= 9, chybovaHlaskaStanin))
        (input$staninValue - 5) / 2
      } else {
        print("chyba ve funkci zOut - spatny vstup")
      }
    })
    


# * output ----------------------------------------------------------------

    
    output$trans_vystup <- renderTable({
      
      zActual <- as.numeric(zOut())
      zActualMinus <- zActual - .25
      zActualPlus <- zActual + .25
      
      if (!stenStan()) { 
        tab <- matrix(c(zActual, iqOut(zActual), percOut(zActual), tOut(zActual), 
                        wechOut(zActual), stenOut(zActual), staninOut(zActual)), 1, 7)
        
        if (input$zaokrouhlit) {
          tab <- round(tab, digits = c(2, rep(0, 6)))
        } else {
          tab <- round(tab, digits = 2)
        }
        
        tab <- matrix(as.character(tab), 1, 7)
        colnames(tab) <- c("z-skóre", "IQ skór", "percentil", "T-skóre",
                           "Wechslerovo skóre", "sten", "stanin")
      } else {
        tmp1 <- c(zActualMinus, iqOut(zActualMinus), percOut(zActualMinus), 
                  tOut(zActualMinus), wechOut(zActualMinus), 
                  stenOut(zActualMinus + .001), staninOut(zActualMinus + .001))
        tmp2 <- c(zActualPlus, iqOut(zActualPlus), percOut(zActualPlus), 
                  tOut(zActualPlus), wechOut(zActualPlus), 
                  stenOut(zActualPlus - .001), staninOut(zActualPlus - .001))
        
        if (input$zaokrouhlit) {
          tmp1 <- round(tmp1, digits = c(2, rep(0, 6)))
          tmp2 <- round(tmp2, digits = c(2, rep(0, 6)))
        } 
        
        tab <- matrix(apply(rbind(tmp1, tmp2), 2, function(x) { 
          ifelse(x[1] != x[2], paste(x[1], x[2], sep = " – "), x[1])}), 1, 7)
        
        if (zActualMinus < -2) {
          tab[1:5] <- c(paste("<", tmp2[1:5]))
        } else if (zActualPlus > 2) {
          tab[1:5] <- c(paste(tmp1[1:5], "<"))
        }
        
        colnames(tab) <- c("z-skóre", "IQ skóre", "percentil", "T-skóre", 
                           "Wechslerovo skóre", "sten", "stanin")
      } 
      rownames(tab) <- "skóre"
      
      tab
    }, include.rownames = T, align = "c"
    )
    
    output$trans_warn <- renderText({
      if(input$test == "perc" & input$percValue == 0) {
        "Pro výpočet skórů byla použita hodnota percentilu 0,25."
      } else if (input$test == "perc" & input$percValue == 100) {
        "Pro výpočet skórů byla použita hodnota percentilu 99,75."
      } else {""}
    })
    
    output$trans_graf <- renderPlot({
      
      zActual <- zOut()
      
      x <- seq(ifelse(zActual <= -5, zActual, -5),
               ifelse(zActual >= 5, zActual, 5), by=.01)    
      y <- dnorm(x)
      
      dataPlot <- data.frame(X = x, Y = y)
      
      p <- ggplot(data = dataPlot, aes(x = X, y = Y))
      
      if (stenStan()) {  # Vypln v pripade stanu ci statninu
        if (abs(zActual) < 2) {
          p <- p + geom_ribbon(data=subset(dataPlot, ((zActual - .25) < X) & (X < (zActual + .25))),
                               aes(ymax=Y), ymin=0, fill="#5cb19c", alpha=1)
        } else if (zActual >= 2) {
          p <- p + geom_ribbon(data=subset(dataPlot, X > ifelse(input$test == "sten", 2, 1.75)),
                               aes(ymax=Y), ymin=0, fill="#5cb19c", alpha=1)
        } else if (zActual <= -2) {
          p <- p + geom_ribbon(data=subset(dataPlot, X < ifelse(input$test == "sten", -2, -1.75)),
                               aes(ymax=Y), ymin=0, fill="#5cb19c", alpha=1)
        }
      }
      
      p <- p +
        geom_line(colour = "#f69324", size = 1.3) +
        geom_hline(yintercept=0, color="grey41", size=.3, alpha = .5) + 
        xlab("z-skóre") + ylab(NULL) + 
        theme_bw(base_size = 18) + 
        theme(axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank(),
              axis.title.x=element_text(vjust=-.5),
              plot.margin=unit(c(0, 0, .5, -.2), "cm"))
      
      if (!stenStan())
        p <- p + geom_vline(xintercept=zActual, color="#5cb19c", size=1.3, linetype = "dashed")
      
      if (abs(zActual) <= 5) 
        p <- p + scale_x_continuous(breaks = -5:5)
      
      
      
      if (input$procentoMasy & stenStan()) {  # Vykreslit procento masy pod krivkou do grafu
        zActualMinus <- zActual - .25
        zActualPlus <- zActual + .25
        
        if (zActualMinus < -2) {
          auc <- paste(round(percOut(zActualPlus), 2), "%", sep = "")
        } else if (zActualPlus > 2) {
          auc <- paste(round(100 - percOut(zActualMinus), 2), "%", sep = "")
        } else { 
          auc <- paste(round(percOut(zActualPlus) - percOut(zActualMinus), 2), "%", sep = "")
        }
        
        p <- p + annotate("text", label = auc, x = ifelse(zActual > 0, zActual + 1, zActual - 1),
                          y = dnorm(zActual) + .01, size = 7, colour = "grey41")
      }
      
      
      p
    })
  
  
  
  
})