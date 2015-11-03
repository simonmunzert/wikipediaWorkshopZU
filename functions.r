#----------------------------------------------------------------
# Using Wikipedia Page View Statistics
# to Measure Issue Salience
# Simon Munzert
# 2015
#----------------------------------------------------------------

# as.character / as.numeric conversion
char <- function(x) as.character(x)
num <- function(x) as.numeric(x)


# rescaling vector to max=100
normalize100 <- function(x) (x/(max(x)))*100
normalize1000 <- function(x) (x/(max(x)))*1000



# function to generate weekly normalized wiki time series (DEPRECATED)
xtsWeeklyNorm <- function(x) {
  wiki_ger_xts <- xts(wiki_ger_df_sub[,x], as.Date(wiki_ger_df$date))
  wiki_ger_xts_weekly <- apply.weekly(wiki_ger_xts,sum)
  names(wiki_ger_xts_weekly) <- "var"
  wiki_ger_xts_weekly$varnorm <-  wiki_ger_xts_weekly$var/max(wiki_ger_xts_weekly$var)*100
  return(wiki_ger_xts_weekly)
}



# adpation of plot.stl to allow for xlab value
plot.stl <- function(..., xlab = "time") {
  mtext <- function(text, ...)                  graphics::mtext(if (text == "time") xlab else text, ...)
  plot.stl <- stats:::plot.stl
  environment(plot.stl) <- environment()
  plot.stl(...)
}



# triangleplot2a is a slight modification of the triangleplot() function of the package 'arm'
triangleplot2a <- function (x, y = NULL, cutpts = NULL, details = TRUE, n.col.legend = 5, 
                            cex.col = 0.7, cex.var = 0.9, digits = 1, labels.cutpts= NULL, color = FALSE, srt = 0) 
{
  if (!is.matrix(x)) 
    stop("x must be a matrix!")
  if (dim(x)[1] != dim(x)[2]) 
    stop("x must be a square matrix!")
  x.na <- x
  x.na[is.na(x.na)] <- -999
  z.plot <- x
  if (is.null(y)) {
    z.names <- dimnames(x)[[2]]
  }
  else {
    z.names <- y
  }
  for (i in 1:dim(z.plot)[1]) for (j in i:dim(z.plot)[2]) z.plot[i, 
                                                                 j] <- NA
  layout(matrix(c(2, 1), 1, 2, byrow = FALSE), c(10.5, .6))
  if (is.null(cutpts)) {
    if (details) {
      neg.check <- abs(sum(z.plot[z.plot < 0], na.rm = T))
      if (neg.check > 0) {
        z.breaks <- sort(c(0, seq(min(z.plot, na.rm = T), 
                                  max(z.plot, na.rm = T), length = n.col.legend)))
      }
      else {
        z.breaks <- seq(min(z.plot, na.rm = T), max(z.plot, 
                                                    na.rm = T), length = n.col.legend + 1)
      }
      for (i in 1:4) {
        n1 <- length(unique(round(z.breaks, digits = digits)))
        n2 <- length(z.breaks)
        ifelse((n1 != n2), digits <- digits + 1, digits <- digits)
      }
      if (digits > 3) {
        stop("Too many digits! Try to adjust n.col.legend to get better presentation!")
      }
    }
    else {
      postive.z <- na.exclude(unique(round(z.plot[z.plot > 
                                                    0], digits = digits)))
      neg.check <- abs(sum(z.plot[z.plot < 0], na.rm = T))
      ifelse(neg.check > 0, negative.z <- na.exclude(unique(round(z.plot[z.plot < 
                                                                           0], digits = digits))), negative.z <- 0)
      max.z <- max(z.plot, na.rm = T)
      min.z <- min(z.plot, na.rm = T)
      z.breaks <- sort(unique(c(postive.z, negative.z)))
      n.breaks <- length(z.breaks)
      l.legend <- ceiling(n.col.legend/2)
      if (n.breaks > 8) {
        if (neg.check > 0) {
          postive.z <- seq(0, max(postive.z), length = l.legend + 
                             1)
          negative.z <- seq(min(negative.z), 0, length = l.legend)
          z.breaks <- sort(unique(c(postive.z, negative.z)))
          n.breaks <- length(z.breaks)
          z.breaks[1] <- min.z
          z.breaks[n.breaks] <- max.z
          n.col.legend <- length(z.breaks) - 1
        }
        else {
          postive.z <- seq(0, max(postive.z), length = n.col.legend + 
                             1)
          z.breaks <- sort(unique(c(postive.z, negative.z)))
          n.breaks <- length(z.breaks)
          z.breaks[1] <- min.z
          z.breaks[n.breaks] <- max.z
          n.col.legend <- length(z.breaks) - 1
        }
      }
      else {
        if (neg.check > 0) {
          z.breaks <- sort(c(0, seq(min(z.plot, na.rm = T), 
                                    max(z.plot, na.rm = T), length = n.col.legend)))
        }
        else {
          z.breaks <- seq(min(z.plot, na.rm = T), max(z.plot, 
                                                      na.rm = T), length = n.col.legend + 1)
        }
      }
    }
  }
  if (!is.null(cutpts)) {
    z.breaks = cutpts
    n.breaks <- length(z.breaks)
    n.col.legend <- length(z.breaks) - 1
  }
  if (color) {
    z.colors <- brewer.pal(n.breaks-1, "RdYlGn")
    
  }
  else {
    z.colors <- gray(n.col.legend:1/n.col.legend)
  }
  par(mar = c(0.5, 0.1, 2, 0.1), pty = "m")
  plot(c(0, 1), c(min(z.breaks), max(z.breaks)), type = "n", 
       bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  for (i in 2:(length(z.breaks))) {
    rect(xleft = 0.5, ybottom = z.breaks[i - 1], xright = 1, 
         ytop = z.breaks[i], col = z.colors[i - 1])
    text(x = 0.45, y = z.breaks[i - 1], labels = format(round(z.breaks[i - 
                                                                         1], digits)), cex = cex.col, adj = 1, xpd = TRUE)
  }
  rect(xleft = 0.5, ybottom = z.breaks[length(z.breaks)], xright = 1, 
       ytop = z.breaks[length(z.breaks)], col = z.colors[length(z.colors)])
  text(x = 0.45, y = z.breaks[length(z.breaks)], labels = format(round(z.breaks[length(z.breaks)], 
                                                                       digits)), cex = cex.col, adj = 1, xpd = TRUE)
  par(mar = c(0.1, 0.1, 2, 0.1), pty = "m")
  image(x = 1:dim(z.plot)[1], y = 1:dim(z.plot)[2], z = z.plot, 
        xaxt = "n", yaxt = "n", bty = "n", col = z.colors, breaks = z.breaks, 
        xlim = c(-2, dim(z.plot)[1] + 0.5), ylim = c(-1, dim(z.plot)[2] + 
                                                       0.5), xlab = "", ylab = "")
  text(x = 1:dim(z.plot)[1], y = 1:dim(z.plot)[2], labels = z.names, 
       cex = cex.var, adj = 1, xpd = TRUE, srt = srt)
  for (i in 1:dim(z.plot)[1]) {
    for (j in i:dim(z.plot)[2]) {
      if (x.na[i, j] == -999 & i != j) 
        points(x = j, y = i, pch = "x", cex = 0.9)
    }
  }
  for (i in 1:dim(z.plot)[1]) {
    for (j in i:dim(z.plot)[2]) {
      if (x.na[i, j] != -999 & i != j & x.na[i, j] > 0) 
        points(x = j, y = i, pch = "+", cex = 0.9)
    }
  }
  for (i in 1:dim(z.plot)[1]) {
    for (j in i:dim(z.plot)[2]) {
      if (x.na[i, j] != -999 & i != j & x.na[i, j] < 0) 
        points(x = j, y = i, pch = "-", cex = 0.9)
    }
  }
}


# first letter toupper
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
