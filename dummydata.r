# a fake dataset with plot data for an experiment with 12 plots and a fenced and unfenced treatment, each replicated twice per plot. 

n_plots <- 12

d_plot <- list()

d_plot$plot <- rep(1:n_plots, each = 2)
d_plot$year <- rep(c("2013", "2014"), times = n_plots)
d_plot$grazing <-  round(exp(runif(n_plots*2,0,6.5)), 0)
d_plot$mowing <-  3-floor(log(d_plot$grazing)) + abs(as.integer(rnorm(n_plots*2,2,1)))
  d_plot$mowing[d_plot$mowing < 0] <- 0
d_plot$fertilization <-  round(exp(runif(n_plots*2,0,5)), 0)
d_plot$temp <- round(rep(rnorm(2,14,1), each = n_plots) + rnorm(n_plots*2, 0, 0.5)  , 1)
d_plot$precipitation <- round(rep(rnorm(2,660,145), each = n_plots) + rnorm(n_plots*2, 0, 50), 3)


# a fake dataset with observational data of species abundance and evenness

d_raw <- list()
d_raw$ID <- 1:(n_plots*4)
d_raw$plot <- rep(1:n_plots, each = 4)
d_raw$fenced <- rep(c(TRUE, TRUE, FALSE, FALSE), times = n_plots)

intercept = 0.5; a = + 0.2; alpha = 1.0; b = +0.6 ; beta = 0.1; c = -0.003 ; gamma = 1.2 ; d = +0.008; e = +0.001 

d_plot_2013 <- subset(as.data.frame(d_plot), year == "2013")[d_raw$plot,]

d_raw$shannon <- with(d_plot_2013, intercept + a * mowing^alpha + as.integer(d_raw$fenced) * b * grazing^beta + c * fertilization^gamma + d * temp + e * precipitation ) + rnorm(n_plots, 0, 0.2)  
d_raw$shannon <- round(d_raw$shannon, 3)

d_raw$shannon[d_raw$shannon < 0] <- 0

#plot(d_raw$shannon ~ d_plot_2013$fertilization)

d_raw$notes <- character(length = n_plots*4)
d_raw$notes[c(14,45)] <- "fence was trampled"
d_raw$notes[c(42)] <- "catch samples were lost"
d_raw$fenced <- c("no", "yes")[d_raw$fenced+1]
d_raw$fenced[c(23)] <- "No"


write.csv(as.data.frame(d_raw), file = "data/rawdata.csv", row.names = FALSE)


# a fake dataset with observational data of species abundance and evenness

d_raw <- list()
d_raw$ID <- 1:(n_plots*4)
d_raw$plot <- rep(1:n_plots, each = 4)
d_raw$fenced <- rep(c(TRUE, TRUE, FALSE, FALSE), times = n_plots)


d_plot_2014 <- subset(as.data.frame(d_plot), year == "2014")[d_raw$plot,]

intercept = 60; a = + 2; alpha = 1.0; b = +11 ; beta = 0.1; c = -0.003 ; gamma = 1.2 ; d = +0.008; e = +0.001 

d_raw$n_species <- with(d_plot_2014, intercept + a * mowing^alpha + as.integer(d_raw$fenced) * b * grazing^beta + c * fertilization^gamma + d * temp + e * precipitation ) + rnorm(n_plots, 0, 10) 
d_raw$n_species <- round(d_raw$n_species, 0)

intercept = 0.5; a = + 0.2; alpha = 1.0; b = +0.6 ; beta = 0.1; c = -0.003 ; gamma = 1.2 ; d = +0.008; e = +0.001 

d_raw$shannon <- with(d_plot_2014, intercept + a * mowing^alpha + d_raw$fenced * b * grazing^beta + c * fertilization^gamma + d * temp + e * precipitation ) + rnorm(n_plots, 0, 0.2)  
d_raw$shannon[d_raw$shannon < 0] <- 0
d_raw$shannon <- round(d_raw$shannon, 3)

d_raw$notes <- character(length = n_plots*4)
d_raw$notes[c(25,26)] <- "rabbit calamity"
d_raw$fenced <- c("no", "yes")[d_raw$fenced+1]

write.csv(as.data.frame(d_raw), file = "data/rawdata2014.csv", row.names = FALSE)



d_plot$precipitation[c(11, 12)] <- NA

write.csv2(as.data.frame(d_plot), file = "data/plots.csv", row.names = FALSE)
