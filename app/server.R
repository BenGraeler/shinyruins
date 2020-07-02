#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xts)
library(reshape2)
library(ggplot2)

monate.lang <- c("Januar", "Februar", "März",
                 "April", "Mai", "Juni",
                 "Juli", "August", "September",
                 "Oktober", "November", "Dezember")

month.long <- c("January", "February", "March",
                "April", "May", "June",
                "July", "August", "September",
                "October","November","December")

monate.kurz <- c("Jan", "Feb", "Mar",
                 "Apr", "Mai", "Jun",
                 "Jul", "Aug", "Sep",
                 "Okt", "Nov", "Dez")

month.short <- c("Jan", "Feb", "Mar",
                 "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep",
                 "Oct","Nov", "Dec")

# Define server logic required to draw a histogram
shinyServer( function(input, output) {
    
    readData <- reactive({
        csvfiles <- paste(input$gcm, paste0("v1-r", 1:10), input$rcm, paste0("rcp", input$rcp), "csv", sep="." )
        
        datafiles <- list.files("data/")
        
        whichfiles <- csvfiles %in% datafiles
        
        listdf <- NULL
        
        for (file in csvfiles[whichfiles]) {
            rdf <- read.csv(paste0("data/",file), sep=";")
            rts <- xts(rdf[,-1], order.by = as.POSIXct(rdf[,1], origin="1970-01-01"))
            listdf <- append(listdf, list(rts))
        }
        
        # TODO: add warning message when there is no data for the given selection
        do.call(merge, lapply(listdf, function(x) x[, input$var]))
    })
    
    readData2 <- reactive({
        csvfiles <- paste(input$gcm2, paste0("v1-r", 1:10), input$rcm2, paste0("rcp", input$rcp2), "csv", sep="." )
        
        datafiles <- list.files("data/")
        
        whichfiles <- csvfiles %in% datafiles
        
        listdf <- NULL
        
        for (file in csvfiles[whichfiles]) {
            rdf <- read.csv(paste0("data/",file), sep=";")
            rts <- xts(rdf[,-1], order.by = as.POSIXct(rdf[,1], origin="1970-01-01"))
            listdf <- append(listdf, list(rts))
        }
        
        # TODO: add warning message when there is no data for the given selection
        do.call(merge, lapply(listdf, function(x) x[, input$var2]))
    })
    
    # controls second axis and plotting of second variable (aligned or none-aligned axes)
    comparable <- reactive({
        input$var == input$var2 # and other, e.g. any temperature
    })
    
    output$tsPlot <- renderPlot({
        # 1st variable selection
        # filter decade
        decFilter <- paste0(input$dec, "-01-01/", substr(input$dec, 1,3), "9-12-31")
        rtsSub <- readData()[decFilter]
        
        # drop years, merge monthly
        yrs <- as.numeric(substr(as.character(index(rtsSub)), 1, 4))
        mth <- months(index(rtsSub))
        mth <- factor(mth, 
                      levels = switch("Mai" %in% mth + 1,
                                      month.long,
                                      monate.lang),
                      ordered = T)
        
        d <- melt(cbind(mth, as.data.frame(rtsSub)), id.vars="mth")
        d$variable <- paste0(yrs, d$variable)
        colnames(d) <- c("month", "yearVar", "variable")
        
        # 2nd variable selection
        # filter decade
        decFilter2 <- paste0(input$dec2, "-01-01/", substr(input$dec2, 1,3), "9-12-31")
        rtsSub2 <- readData2()[decFilter2]
        
        # drop years, merge monthly
        yrs2 <- as.numeric(substr(as.character(index(rtsSub2)), 1, 4))
        mth2 <- months(index(rtsSub2))
        mth2 <- factor(mth2, 
                       levels = switch("Mai" %in% mth2 + 1,
                                       month.long,
                                       monate.lang),
                       ordered = T)
        
        d2 <- melt(cbind(mth2, as.data.frame(rtsSub2)), id.vars="mth2")
        d2$variable <- paste0(yrs2, d2$variable)
        colnames(d2) <- c("month", "yearVar", "variable")
        
        if (comparable()) {
            remapvar2 <- function(x) x
            sa <- scale_y_continuous(sec.axis = sec_axis(~ . + 0, name=input$var2))
        } else {
            r1 <- range(d$variable)
            r2 <- range(d2$variable)
            scl <- (r2[2]-r2[1])/(r1[2]-r1[1])
            sft <- r2[1]-r1[1]    
            
            sa <- scale_y_continuous(sec.axis = sec_axis(~ . * scl + sft, name=input$var2))
            
            d2$variable <- (d2$variable - sft) / scl
        }
        
        ggplot(NULL, aes(x=month, y = variable, group=yearVar)) +
            geom_line(data = d, col="red", alpha=0.4) +
            geom_line(data = d2, col="blue", alpha=0.4) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none") +
            sa + labs(y=input$var, title = "Jahresgänge im Vergleich") 
    })
    
    output$densityPlot <- renderPlot({
        
        selMth <- input$months
        if (input$reverseMonth)
            selMth <- pmin(pmax(rev(selMth)+c(1,-1),1),12)
        
        
        
        # 1st variable selection
        # filter decade
        decFilter <- paste0(input$dec, "-01-01/", substr(input$dec, 1,3), "9-12-31")
        rtsSub <- readData()[decFilter]
        
        # drop years, merge monthly
        yrs <- as.numeric(substr(as.character(index(rtsSub)), 1, 4))
        mth <- months(index(rtsSub))
        mth <- factor(mth, 
                      levels = switch("Mai" %in% mth + 1,
                                      month.long,
                                      monate.lang),
                      ordered = T)
        
        selRows <- as.numeric(mth) >= selMth[1] & as.numeric(mth) <= selMth[2]
        if (input$reverseMonth)
            selRows <- as.numeric(mth) >= selMth[1] | as.numeric(mth) <= selMth[2]
        
        d <- data.frame(var=unlist(as.data.frame(rtsSub)[selRows,]))
        
        # 2nd variable selection
        # filter decade
        decFilter2 <- paste0(input$dec2, "-01-01/", substr(input$dec2, 1,3), "9-12-31")
        rtsSub2 <- readData2()[decFilter2]
        
        # drop years, merge monthly
        yrs2 <- as.numeric(substr(as.character(index(rtsSub2)), 1, 4))
        mth2 <- months(index(rtsSub2))
        mth2 <- factor(mth2, 
                       levels = switch("Mai" %in% mth2 + 1,
                                       month.long,
                                       monate.lang),
                       ordered = T)
        
        selRows2 <- as.numeric(mth2) >= selMth[1] & as.numeric(mth2) <= selMth[2]
        if (input$reverseMonth)
            selRows2 <- as.numeric(mth2) >= selMth[1] | as.numeric(mth2) <= selMth[2]
        
        d2 <- data.frame(var=unlist(as.data.frame(rtsSub2)[selRows2,]))
        
        r <- range(rbind(rtsSub, rtsSub2))
        
        ggplot(NULL, aes(var)) +
            geom_density(data = d, fill = "red", colour="red", alpha = 0.1) +
            geom_density(data = d2, fill = "blue", colour="blue", alpha = 0.1) + 
            geom_vline(aes(xintercept=median(d$var)), color="red", linetype="dashed", size=1, alpha=0.3) +
            geom_vline(aes(xintercept=median(d2$var)), color="blue", linetype="dashed", size=1, alpha=0.3) +
            xlim(r[1]- diff(r)*0.2, r[2]+diff(r)*0.2) + 
            labs(x = paste(input$var, "/", input$var2), 
                 y = "Wahrscheinlichkeit",
                 title = ifelse(selMth[1] != selMth[2],
                                paste("Verteilung von", monate.lang[selMth[1]],
                                      "bis", monate.lang[selMth[2]]),
                                paste("Verteilung im", monate.lang[selMth[1]])))
        
    })
})
