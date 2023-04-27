#Suivi du tuto pour faire la transformée de fourrier :
  #http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

 <- xsseq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)


wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)


wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)


repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)


plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}

# An eg
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100))


acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s)
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components

f <- function(t,w) {
  dc.component +
    sum( component.strength * sin(component.freqs*w*t + component.delay))
}

plot.fourier(f,f.0,ts)






convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize

  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)

  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(1:5))

# returns the x.n time series for a given time sequence (ts) and
# a vector with the amount of frequencies k in the signal (X.k)
get.trajectory <- function(X.k,ts,acq.freq) {

  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)

  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }

  x.n * acq.freq
}



plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]

  plot(plot.data, t="h", lwd=2, main="",
       xlab="Frequency (Hz)", ylab="Strength",
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))


}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}


X.k <- fft(c(4,0,0,0))                   # get amount of each frequency k

time     <- 4                            # measuring time interval (seconds)
acq.freq <- 100                          # data acquisition frequency (Hz)
ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s)

x.n <- get.trajectory(X.k,ts,acq.freq)   # create time wave

plot(ts,x.n,type="l",ylim=c(-2,4),lwd=2)
abline(v=0:time,h=-2:4,lty=3); abline(h=0)

plot.harmonic(X.k,1,ts,acq.freq,"red")
plot.harmonic(X.k,2,ts,acq.freq,"green")
plot.harmonic(X.k,3,ts,acq.freq,"blue")






acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s)
f.0 <- 1/time

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,.5,.75) # strength of signal components

f   <- function(t,w) {
  dc.component +
    sum( component.strength * sin(component.freqs*w*t + component.delay))
}

plot.fourier(f,f.0,ts=ts)

plot(ts)


w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)
plot(trajectory)


X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,20))



fft(trajectory)

d <- runif(100)

d_ft <- fft(d)

p <- plot.frequency.spectrum(d_ft, xlimits=c(0,20))



### Package pour calculer les harmoniques d'un signal (fréquences sous jacentes)

install.packages("GeneCycle")

f.data <- GeneCycle::periodogram(d)
f.data

harmonics <- 1:50

plot(f.data$freq[harmonics],
     f.data$spec[harmonics],
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")
plot(d, type = "l")

plot(f.data$spec, type = "l")

# Normalement je peux utiliser directement la valeur donnée pour le spectre (f.data$spec) et prendre ça pour l'EMD

# test :

d <- runif(100)
e <- runif(100)

plot(d, type = "l")
plot(e, type = "l")

f.data1 <- GeneCycle::periodogram(d)
f.data2 <- GeneCycle::periodogram(e)


harmonics <- 1:50

plot(f.data1$freq,
     f.data1$spec,
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

plot(f.data2$freq,
     f.data2$spec,
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

# test avec des vraies cellules :
library(calipR)

VNO <- prepareData("inst/extdata/VNO_small", 5, 0.25, compare_groups = TRUE)

test_VNO_clean_1Hz <- clean_data(VNO, 0.1, 2, mean_width = 20, DPA_width = 10, CN_DPA_width = 80, mean_width_diff = 10)
test_VNO_back_1Hz <- backEstimate(test_VNO_clean_1Hz, method = "gam")

simple_cell_plot(test_VNO_back_1Hz, cell = "A2aal", var = "Mean_Grey", line = "gam")

A1 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2aae"]$gam_detrended
A2 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2aal"]$gam_detrended

# similar ones :
A3 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2aao"]$gam_detrended
A4 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2aap"]$gam_detrended

A5 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2abc"]$gam_detrended
A6 <- test_VNO_back_1Hz[test_VNO_back_1Hz$Cell_id == "A2abf"]$gam_detrended


plot(A1, type = "l")
plot(A2, type = "l")

f.data1 <- GeneCycle::periodogram(A1)
f.data2 <- GeneCycle::periodogram(A2)
f.data3 <- GeneCycle::periodogram(A3)
f.data4 <- GeneCycle::periodogram(A4)
f.data5 <- GeneCycle::periodogram(A5)
f.data6 <- GeneCycle::periodogram(A6)



plot(f.data1$freq,
     f.data1$spec,
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

plot(f.data2$freq,
     f.data2$spec,
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

plot_freq <- function(data) {


  plot(data$freq,
       data$spec,
       xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

}

plot_freq(f.data1)
plot_freq(f.data2)
plot_freq(f.data3)
plot_freq(f.data4)
plot_freq(f.data5)
plot_freq(f.data6)



# Maintenant calcul de l'EMD :


m1 <- matrix(f.data1$spec)
m1 <- cbind(m1, matrix(f.data1$freq))

m2 <- matrix(f.data2$spec)
m2 <- cbind(m2, matrix(f.data2$freq))

emd(m1, m2)

dim(m1)
dim(m2)


matrix_gen <- function(f.data) {

  m1 <- matrix(f.data$spec)
  m1 <- cbind(m1, matrix(f.data$freq))

  return(m1)
}


m1 <- matrix_gen(f.data1)
m2 <- matrix_gen(f.data2)
m3 <- matrix_gen(f.data3)
m4 <- matrix_gen(f.data4)
m5 <- matrix_gen(f.data5)
m6 <- matrix_gen(f.data6)

emd(m1,m2)
emd(m1,m6)
emd(m3,m4)

test_VNO_norm_1Hz <- norm_df(test_VNO_back_1Hz, var = "gam", width = 10)
test_deconvolve <- deconvolve(test_VNO_norm_1Hz, lambda = 1000, gam = 0.90, constraint =T, threshold = 3)
test_VNO_best_1Hz <- keep_best_peaks(test_deconvolve)
test_borders_1Hz <- find_borders(test_VNO_best_1Hz, 150)
test_classify_1Hz <- classify_peaks(test_borders_1Hz, time_thresh = 1, frame_rate = 0.25)
test_resp_1 <- Analyze_Responses(test_classify_1Hz, test_VNO_back_1Hz, compare_groups = FALSE)
end <- Sys.time()
