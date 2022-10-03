# SEM a konfidencni interval

library(shiny)
library(ggplot2)
library(grid)

# Funkce: Převod skórů ----------------------------------------------------

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
      paste(sep="", "<b>Standardní chyba měření:</b> SE = ", round(SEM(), 2), " (v z-skórech)<br />",
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
  
  output$text_CI2 <- renderTable({
    q <- qnorm((100 + intWidth()) / 200)
    SEdif <- sqrt(2)*SEM()
    SEpred <- dist()[2] * sqrt(1 - input$CI_reliability**2)
    
    CIdif <- round(expT()[2] + c(-1,1)*q*SEdif, dist()[3])
    CIpred <- round(expT()[1] + c(-1,1)*q*SEpred, dist()[3])
    
    result <- data.frame(
      X = round(expT()[2:1], dist()[3]),
      SE = c(SEdif, SEpred),
      CI = c(
        paste0("[", paste0(round(CIdif, 2), collapse = "; "), "]"),
        paste0("[", paste0(round(CIpred, 2), collapse = "; "), "]")
      ),
      row.names = c("rozdíl dvou skórů", "predikce (retest)")
    )
    result
  }, rownames = T)
  
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
    

# Kombinace skórů ---------------------------------------------------------
    
    SS_z <- reactive(-qnorm(input$SS_p/2))
    
    SS_use <- reactive({scores <- cbind(input$SS1,
                                        input$SS2,
                                        input$SS3,
                                        input$SS4,
                                        input$SS5,
                                        input$SS6,
                                        input$SS7,
                                        input$SS8,
                                        input$SS9,
                                        input$SS10) 
    !is.na(scores)})
    
    SS_scores <- reactive({scores <- cbind(input$SS1,
                                           input$SS2,
                                           input$SS3,
                                           input$SS4,
                                           input$SS5,
                                           input$SS6,
                                           input$SS7,
                                           input$SS8,
                                           input$SS9,
                                           input$SS10)
    scores <- scores[SS_use()]})
    SS_rels <- reactive({rels <- cbind(input$RR1,
                                       input$RR2,
                                       input$RR3,
                                       input$RR4,
                                       input$RR5,
                                       input$RR6,
                                       input$RR7,
                                       input$RR8,
                                       input$RR9,
                                       input$RR10)
    rels <- rels[SS_use()]
    rels[is.na(rels)] <- rels[1]
    rels})

    ## převede vše na z-skóre
    SS_zscore <- reactive({
      if (input$SS_scale == "IQ") {
        scores <- (SS_scores()-100)/15
      } else if (input$SS_scale == "T") {
        scores <- (SS_scores()-50)/10
      } else if (input$SS_scale == "z") {
        scores <- SS_scores()
      } else if (input$SS_scale == "W") {
        scores <- (SS_scores()-10)/3
      } else if (input$SS_scale == "jednotka") {
        scores <- (SS_scores() - input$SS_M_manual)/input$SS_SD_manual
      } else {
        scores <- SS_scores()
        scores[scores == 0] <- 0.25
        scores[scores == 100] <- 99.75
        scores <- qnorm(scores/100)
      }
      scores
    })
    
    SS_SE <- reactive({
      sqrt(1-SS_rels())
      
    })
    
    SS_CItable <- reactive({
      out <- data.frame(#test = paste0("[", c(1:length(SS_zscore())), "]"),
                        z=SS_zscore(), r=SS_rels(), SE=SS_SE(), SEt = SS_SE()*sqrt(SS_rels()),
                        CI_low = SS_zscore() - SS_z()*SS_SE(), 
                        CI_up = SS_zscore() + SS_z()*SS_SE())
      rownames(out) <- paste0("[", c(1:length(SS_zscore())), "]")
      if (input$SS_scale == "IQ") {
        out$X <- iqOut(out$z)
        out$X_CI_low <- iqOut(out$CI_low)
        out$X_CI_up <- iqOut(out$CI_up)
      } else if (input$SS_scale == "T") {
        out$X <- tOut(out$z)
        out$X_CI_low <- tOut(out$CI_low)
        out$X_CI_up <- tOut(out$CI_up)
      } else  if (input$SS_scale == "z") {
        out$X <- (out$z)
        out$X_CI_low <- (out$CI_low)
        out$X_CI_up <- (out$CI_up)
      } else if (input$SS_scale == "W") {
        out$X <- wechOut(out$z)
        out$X_CI_low <- wechOut(out$CI_low)
        out$X_CI_up <- wechOut(out$CI_up)
      } else if (input$SS_scale == "jednotka") {
        out$X <- out$z * input$SS_SD_manual + input$SS_M_manual
        out$X_CI_low <- out$CI_low * input$SS_SD_manual + input$SS_M_manual
        out$X_CI_up <- out$CI_up * input$SS_SD_manual + input$SS_M_manual
      } else {
        out$X <- pnorm(out$z)
        out$X_CI_low <- pnorm(out$CI_low)
        out$X_CI_up <- pnorm(out$CI_up)
      }
      
      out$z_CI = paste0("[", round(out$CI_low, 2), "; ", round(out$CI_up, 2), "]")
      out$CI = paste0("[", round(out$X_CI_low, 2), "; ", round(out$X_CI_up, 2), "]")
      out
    })
    

    SS_result <- reactive({
      if (input$SS_apriori == "populace") {
        estimates <- SS_CItable()$r[1]*SS_CItable()$z[1]
        SE <- SS_CItable()$SE[1]
        # SE_true <- SS_CItable()$SE[1] * sqrt(SS_CItable()$r[1])
        SE_true <- SE
        apriori_odhad <- 0
      } else if (input$SS_apriori == "no") {
        estimates <- SS_CItable()$z[1]
        SE <- SS_CItable()$SE[1]
        SE_true <- SE
        apriori_odhad <- NA
      } else {
        if (input$SS_scale == "IQ") {
          apriori_odhad <- (input$SS_odhad-100)/15
        } else if (input$SS_scale == "T") {
          apriori_odhad <- (input$SS_odhad-50)/10
        } else if (input$SS_scale == "z") {
          apriori_odhad <- input$SS_odhad
        } else if (input$SS_scale == "W") {
          apriori_odhad <- (input$SS_odhad-10)/3
        } else if (input$SS_scale == "jednotka") {
          apriori_odhad <- (input$SS_odhad - input$SS_M_manual)/input$SS_SD_manual
        } else {
          apriori_odhad <- input$SS_odhad
          apriori_odhad[apriori_odhad == 0] <- 0.25
          apriori_odhad[apriori_odhad == 100] <- 99.75
          apriori_odhad <- qnorm(apriori_odhad/100)
        }
        estimates <- SS_CItable()$r[1]*SS_CItable()$z[1] + (1-SS_CItable()$r[1])*apriori_odhad
        SE <- SS_CItable()$SE[1]
        # SE_true <- sqrt(SS_CItable()$r[1]) * SE
        SE_true <- SE
        
        
        
      }
      
      
      r_obs <- (1+sqrt(1-4*(SE_true*sqrt(SS_CItable()$r[1]))**2))/2
      
      result <- data.frame(apriori = apriori_odhad, est=estimates, SE=SE, SE_true = SE_true, r = r_obs)
      
      i <- 2
      while(i <= nrow(SS_CItable())) {
        apriori_x <- result$est[i-1]
        est_x <- (SS_CItable()$SE[i]**2)/(SS_CItable()$SE[i]**2 + result$SE_true[i-1]**2) * apriori_x + 
          (result$SE_true[i-1]**2)/(SS_CItable()$SE[i]**2 + result$SE_true[i-1]**2) * SS_CItable()$z[i]
        SE_x <- SS_CItable()$SE[i]
        SE_true_x <- 1/sqrt(1/SE_x**2 + 1/result$SE_true[i-1]**2)
        
        r_x <- (1+sqrt(1-4*SE_true_x**2))/2
        
        result <- rbind(result, c(apriori_x, est_x, SE_x, SE_true_x, r_x))
        i <- i+1
      }
      
      result$CI_low <- result$est - SS_z()*result$SE_true
      result$CI_up <- result$est + SS_z()*result$SE_true
      
      if (input$SS_scale == "IQ") {
        result$X <- iqOut(result$est)
        result$X_CI_low <- iqOut(result$CI_low)
        result$X_CI_up <- iqOut(result$CI_up)
      } else if (input$SS_scale == "T") {
        result$X <- tOut(result$est)
        result$X_CI_low <- tOut(result$CI_low)
        result$X_CI_up <- tOut(result$CI_up)
      } else  if (input$SS_scale == "z") {
        result$X <- (result$est)
        result$X_CI_low <- (result$CI_low)
        result$X_CI_up <- (result$CI_up)
      } else if (input$SS_scale == "W") {
        result$X <- wechOut(result$est)
        result$X_CI_low <- wechOut(result$CI_low)
        result$X_CI_up <- wechOut(result$CI_up)
      } else if (input$SS_scale == "jednotka") {
        result$X <- result$est * input$SS_SD_manual + input$SS_M_manual
        result$X_CI_low <- result$CI_low * input$SS_SD_manual + input$SS_M_manual
        result$X_CI_up <- result$CI_up * input$SS_SD_manual + input$SS_M_manual
      } else {
        result$X <- pnorm(result$est)
        result$X_CI_low <- pnorm(result$CI_low)
        result$X_CI_up <- pnorm(result$CI_up)
      }
      result$CI = paste0("[", round(result$X_CI_low, 2), "; ", round(result$X_CI_up, 2), "]")
      rownames(result) <- rownames(SS_CItable())
      result
      


    })
    
    SS_ylims <- reactive({
      if (input$SS_scale == "P") {
        c(0, 1)
      } else {
        c(floor(min(SS_CItable()$X_CI_low)), ceiling(max(SS_CItable()$X_CI_up)))
      }
    })
    
    SS_x2 <- reactive({
      M <- weighted.mean(SS_CItable()$z, 1/SS_CItable()$SE**2)
      odhad = SS_result()$X[nrow(SS_result())]
      CI <- SS_result()[nrow(SS_result()), 11]
      x2 <- sum(((SS_CItable()$z - M)/SS_CItable()$SE)**2)
      df <- nrow(SS_CItable()) - 1
      p <- pchisq(x2, df, lower.tail = F)
      data.frame(odhad, CI, x2=x2, df=df, p=p)
    })
    
    
    

# * output ----------------------------------------------------------------


    output$SS_warning1 <- renderText({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) <= .5)) {
        "Upozornění: Alespoň jedna ze zadaných reliabilit je nižší než 0,5. 
        V takovém případě výpočty nejsou identifikované a kalkulačka nemůže poskytnout věrohodný výstup."
      }
    })
    
    output$SS_warning2 <- renderText({
      if (length(SS_rels()[!is.na(SS_rels())] > 0) & (max(c(SS_rels(), 0), na.rm=T) > 1)) {
        "Reliabilita se musí nacházet v rozmezí 0-1."
      }
    })
    output$SS_warning3 <- renderText({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        if((SS_x2()$p < input$SS_p) & (nrow(SS_CItable()) > 1)) {
          "Skóry se statisticky významně liší. Nedoporučujeme interpretovat kombinovaný skór!"
        }
      }
    })
    
    output$SS_ok <- reactive({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        1
      } else {
        0
      }
    })
    
    
    output$SS_table <- renderTable({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        SS_CItable()[, c(7, 2:3, 11)]
      }
    }, rownames = T)
    
    output$SS_CIplot <- renderPlot({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        plot(SS_CItable()$X, type="b", xlab = "test", ylab = "skóre", lwd = 3, pch = 16, col = "darkorange", 
             ylim = SS_ylims(), xaxt="n", main = "Intervaly spolehlivosti měření")
        axis(side = 1, at = c(1:nrow(SS_result())), labels = rownames(SS_result()))
        abline(h = weighted.mean(SS_CItable()$X, 1/SS_CItable()$SE**2), col= "gray", lty=2, lwd=2)
        arrows(x0 = c(1:nrow(SS_CItable())), y0 = SS_CItable()$X_CI_low, y1 = SS_CItable()$X_CI_up, 
               angle = 90, code = 3, lwd=2, length = .1)
        points(SS_CItable()$X, lwd = 3, pch = 21, bg = "darkorange", col = "white", cex = 3)
      }
    })
    
    output$SS_goplot <- renderPlot({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        plot(SS_result()$X, type="b", xlab = "test", ylab = "skóre", lwd = 3, pch = 16, col = "darkorange", 
             ylim = c(min(SS_result()$X_CI_low), max(SS_result()$X_CI_up)), xaxt="n", 
             main = "Vývoj intervalu měření")
        axis(side = 1, at = c(1:nrow(SS_result())), labels = rownames(SS_result()))
        polygon(x = c(c(1:nrow(SS_CItable())), c(nrow(SS_CItable()):1)), 
                y = c(SS_result()$X_CI_low, SS_result()$X_CI_up[nrow(SS_CItable()):1]), 
                col="gray90", border = NA)
        arrows(x0 = c(1:nrow(SS_CItable())), y0=SS_result()$X_CI_low, y1=SS_result()$X_CI_up, 
               angle = 90, code = 3, lwd=2, length = .1)
        abline(h = SS_result()$X[length(SS_result()$X)], col= "gray", lty=2, lwd=2)
        lines(c(1:nrow(SS_CItable())), SS_result()$X, type="b", lwd = 3, pch = 21, bg = "darkblue", col = "darkorange", cex = 3)
        points(c(1:nrow(SS_CItable())), SS_result()$X, lwd = 3, pch = 21, bg = "darkblue", col = "white", cex = 3)
        
      }      
    })
    
    output$SS_result <- renderTable({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        SS_result()[, c(8, 3:5, 11)]
      }
    }, rownames = T)
    
    output$SS_x2 <- renderTable({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        SS_x2()
      }
    })
    
    output$SS_reliabilita <- renderPlot({
      if (length(SS_SE()[!is.na(SS_SE())] > 0) & (min(c(SS_rels(), 8), na.rm=T) > .5) & (max(c(SS_rels(), 0), na.rm=T) <= 1)) {
        plot(SS_result()$r, type="b", main = "Vývoj reliability během testování", 
             ylim = c(0, 1), xaxt="n" ,ylab="reliabilita", xlab="test", lwd=3)
        axis(side = 1, at = c(1:nrow(SS_result())), labels = rownames(SS_result()))
      }
    })
    
    
    output$SS_point <- renderText({
      SS_result()[nrow(SS_result()), 8]
    })
    output$SS_CItext <- renderText({
      SS_result()[nrow(SS_result()), 11]
    })
    output$SS_Ntest <- renderText({
      nrow(SS_result())
    })
    output$SS_pout <- renderText({
      input$SS_p
    })
    
    

# Rozdíl skórů ------------------------------------------------------------

    RS_z <- reactive(-qnorm(input$RS_p/2))
    
    
    ## distribuce
    RS_dist <- reactive({
      if(input$RS_scale == "IQ") {
        c(100, 15, 0)
      } else if(input$RS_scale == "T") {
        c(50, 10, 0)
      } else if (input$RS_scale == "z") {
        c(0, 1, 2)
      } else if (input$RS_scale == "W") {
        c(10, 3, 0)
      } else if (input$RS_scale == "P") {
        c(0, 1, 0)
      } else {
        c(input$RS_M_manual, input$RS_SD_manual, 2)
      }
    })
    
    RS_X <- reactive({
      c(input$RS_X1, input$RS_X2)
    })
    RS_r <- reactive({
      RS_r <- c(input$RS_r1, input$RS_r2)
      if(is.na(input$RS_r2)) {
        RS_r[2] <- RS_r[1]
      }
      RS_r
    })
    
    ## 1. pozorovaný skór
    RS_obs1 <- reactive({
      if(input$RS_scale == "P") {
        if (is.na(input$RS_X1)) {
          x <- NA
        } else if (input$RS_X1 > 0 & input$RS_X1 < 100) {
          x <- qnorm(input$RS_X1/100)
        } else if (input$RS_X1 == 0) {
          x <- qnorm(.0025)
        } else {
          x <- qnorm(.9975)
        }
      } else {
        x <- input$RS_X1
      }
      tr <- x*RS_r()[1] + RS_dist()[1]*(1-RS_r()[1])
      se <- RS_dist()[2]*sqrt(1-RS_r()[1])
      set <- se*sqrt(RS_r()[1])
      c(x, tr, se, set)
    })
    
    ## 2. pozorovaný skór
    RS_obs2 <- reactive({
      if(input$RS_scale == "P") {
        if (is.na(input$RS_X2)) {
          x <- NA
        } else if (input$RS_X2 > 0 & input$RS_X2 < 100) {
          x <- qnorm(input$RS_X2/100)
        } else if (input$RS_X2 == 0) {
          x <- qnorm(.0025)
        } else {
          x <- qnorm(.9975)
        }
      } else {
        x <- input$RS_X2
      }
      tr <- x*RS_r()[2] + RS_dist()[1]*(1-RS_r()[2])
      se <- RS_dist()[2]*sqrt(1-RS_r()[2])
      set <- se*sqrt(RS_r()[2])
      c(x, tr, se, set)
    })
    
    
    ## statisticky významný rozdíl
    RS_Sdif <- reactive({
      se_dif <- sqrt(RS_obs1()[3]**2 + RS_obs2()[3]**2)
      
      if (isFALSE(input$RS_regM)) {
        dif <- RS_obs2()[1] - RS_obs1()[1]
        exp <- RS_obs1()[1]
        CI <- paste0("[", paste0(round(exp + c(-1,1)*se_dif*RS_z(), 2), collapse = "; "), "]")  
      } else {
        dif <- sqrt(RS_r()[2]) * (RS_obs2()[1]-RS_dist()[1]) - sqrt(RS_r()[1]) * (RS_obs1()[1]-RS_dist()[1])
        exp <- NA
        CI <- NA
      }
      z_dif <- dif/se_dif
      p_dif <- pnorm(abs(z_dif), lower.tail = F)*2
      
      if(input$RS_scale == "P") {
        dif <- round(pnorm(dif)*100)
        exp <- round(pnorm(dif)*100)
        CI <- paste0("[", paste0(round(pnorm(exp + c(-1,1)*se_dif*RS_z())*100), collapse = "; "), "]")  
      }
      
      if(is.na(p_dif)) {
        word_dif <- "Pro výpočet zadejte oba skóry a reliabilitu alespoň prvního měření."
      } else if(p_dif < input$RS_p) {
        word_dif <- "Obě měření se statisticky významně liší."
      } else {
        word_dif <- ""
      }
      RS_Sdif <- data.frame(exp, CI, dif, se_dif, z_dif, p_dif, word_dif)

      names(RS_Sdif) <- c("E(T2)", "CI", "rozdíl", "SE", "z", "p", "")
      rownames(RS_Sdif) <- "statistická významnost"
      RS_Sdif
      
    })
    
    ## klinicky významný rozdíl
    RS_Kdif <- reactive({
        exp <- input$RS_cor * RS_obs1()[1] + (1-input$RS_cor) * RS_dist()[1]
        dif <- RS_obs2()[1] - exp
        se_dif <- RS_dist()[2] * sqrt(1 - input$RS_cor**2)
        CI <- paste0("[", paste0(round(exp + c(-1,1)*se_dif*RS_z(), 2), collapse = "; "), "]")
        z_dif <- dif/se_dif
        p_dif <- pnorm(abs(z_dif), lower.tail = F)*2
        if (is.na(p_dif)) {
          word_dif <- "Pro výpočet zadejte skóry obou testů a jejich korelaci."
        } else if(p_dif < input$RS_p) {
          word_dif <- "Obě měření se klinicky významně liší."
        } else {
          word_dif <- ""
        }
        if(input$RS_scale == "P") {
          exp <- round(pnorm(exp)*100)
          dif <- round(pnorm(dif)*100)
          CI <- paste0("[", paste0(round(pnorm(exp + c(-1,1)*se_dif*RS_z())*100), collapse = "; "), "]")  
        }
        RS_Kdif <- data.frame(exp, CI, dif, se_dif, z_dif, p_dif, word_dif)
        names(RS_Kdif) <- c("E(T2)", "CI", "rozdíl", "SE", "z", "p", "")
        rownames(RS_Kdif) <- "klinická významnost"
        RS_Kdif
    })
    
    ## Predikce
    RS_pred <- reactive({
      exp <- RS_obs1()[2]
      dif <- RS_obs2()[1] - exp
      se_dif <- RS_dist()[2] * sqrt(1 - RS_r()[1]**2)
      CI <- paste0("[", paste0(round(exp + c(-1,1)*se_dif*RS_z(), 2), collapse = "; "), "]")
      z_dif <- dif/se_dif
      p_dif <- pnorm(abs(z_dif), lower.tail = F)*2
      if(is.na(p_dif)) {
        word_dif <- "Pro výpočet zadejte oba skóry a reliabilitu alespoň prvního měření."
      } else if(p_dif < input$RS_p) {
        word_dif <- "Retest je statisticky významně odlišný od pretestu."
      } else {
        word_dif <- ""
      }
      if(input$RS_scale == "P") {
        exp <- round(pnorm(exp)*100)
        dif <- round(pnorm(dif)*100)
        CI <- paste0("[", paste0(round(pnorm(exp + c(-1,1)*se_dif*RS_z())*100), collapse = "; "), "]")  
      }
      RS_pred <- data.frame(exp, CI, dif, se_dif, z_dif, p_dif, word_dif)
      names(RS_pred) <- c("E(T2)", "CI", "rozdíl", "SE", "z", "p", "")
      rownames(RS_pred) <- "test-retest"
      RS_pred
    })
    
    RS_scores <- reactive({
      RS_scores <- as.data.frame(rbind(RS_obs1(), RS_obs2()))
      names(RS_scores) <- c("X", "E(T)", "SE", "SE_t")
      RS_scores$CI_low <- RS_scores$X - RS_z() * RS_scores$SE
      RS_scores$CI_up  <- RS_scores$X + RS_z() * RS_scores$SE
      
      RS_scores$CI_low_reg <- RS_scores$'E(T)' - RS_z() * RS_scores$SE
      RS_scores$CI_up_reg  <- RS_scores$'E(T)' + RS_z() * RS_scores$SE
      
      if (input$RS_scale == "P") {
        RS_scores$X <- round(pnorm(RS_scores$X)*100)
        RS_scores$'E(T)' <- round(pnorm(RS_scores$'E(T)')*100)
        RS_scores$CI_low <- round(pnorm(RS_scores$CI_low)*100)
        RS_scores$CI_up  <- round(pnorm(RS_scores$CI_up)*100)
        RS_scores$CI_low_reg <- round(pnorm(RS_scores$CI_low)*100)
        RS_scores$CI_up_reg  <- round(pnorm(RS_scores$CI_up)*100)
      }
      
      RS_scores$CI = paste0("[", round(RS_scores$CI_low, 2), "; ", round(RS_scores$CI_up, 2), "]")
      RS_scores$CI_reg = paste0("[", round(RS_scores$CI_low_reg, 2), "; ", round(RS_scores$CI_up_reg, 2), "]")
      rownames(RS_scores) <- c("1. test", "2. test")
      RS_scores
    })

# * output ----------------------------------------------------------------
    
    
    output$RS_warn1 <- renderText({
      if (input$RS_cor > sqrt(RS_r()[1]*RS_r()[2]) & !is.na(input$RS_cor) & !is.na(RS_obs1()[3])) {
        "Pozor: Korelace testů je vyšší, než by odpovídalo jejich reliabilitám. Výsledky nejsou důvěryhodné!"
      } else {
        NULL
      }
    })
    
    output$RS_warn2 <- renderText({
      if (((input$RS_r1 < 0 | input$RS_r1 > 1) & !is.na(input$RS_r1)) | 
          ((input$RS_r2 < 0 | input$RS_r2 > 1) & !is.na(input$RS_r2)) | 
          ((input$RS_cor < -1 | input$RS_cor > 1) & !is.na(input$RS_cor)))  {
        "Reliability se musí nacházet v rozmezí mezi 0 a 1. Korelace testů se musí nacházet v rozmezí -1 až 1."
      } else {
        NULL
      }
    })
    
    output$RS_warn3 <- renderText({
      if ((input$RS_X1 < 0 | input$RS_X1 > 100) & input$RS_scale == "P" & !is.na(input$RS_X1)) {
        "Percentil musí ležet v rozmezí 0 až 100."
      } else {
      }
    })
    
    output$RS_result <- renderTable({
      rbind(RS_Sdif(), RS_Kdif(), RS_pred())
    }, rownames = T)
    
    output$RS_cis <- renderTable({
      RS_scores()[c(1:3, 9:10)]
    }, rownames = T)
    
    output$RS_plot <- renderPlot({
      if(input$RS_scale == "P") {
        sirka <- c(0, 100)
      } else {
        sirka <- c(min(RS_scores()[, 5]), max(RS_scores()[, 6]))
      }
      cols = c("lightblue", "lightgreen")
      if (!is.na(RS_scores()[2,1]) & !is.na(RS_r()[2])) {
        plot(c(RS_scores()[1,1], RS_scores()[2,1]), c(1,0), 
             xlim=sirka, ylim = c(-.2, 1.2),
             pch=16, col = cols, xlab = "skóre", ylab = "test", yaxt="n", cex=5)
        arrows(x0 = RS_scores()[1, 5], x1 = RS_scores()[1, 6], y0 = 1, angle = 90, length = .15, 
               col = cols[1], code=3, lwd=3)
        arrows(x0 = RS_scores()[2, 5], x1 = RS_scores()[2, 6], y0 = 0, angle = 90, length = .15, 
               col = cols[2], code=3, lwd=3)
        axis(2, c(1,0), c("1", "2"), tick=F, las=2, cex.axis=2)
      } else {
        NULL
      }
    }, height = 250)

})