# filtering signals in freq domain

library(signal)

acq.freq <- 200  # data acquisition frequency (Hz)    # for given freq, Nyquist freq = 100 Hz
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)   # vector of sampling time-points (s) 

signal <- 3*sin(3*w*ts) 

noise <- 0.9 * sin(12*w*ts)

noise2 <- 1.1 * sin(7*w*ts)

noisedSig <- signal + noise
noisedSig <- signal + noise + noise2

head(signal)

plot(ts, signal, type = "l")
plot(ts, noise, type = "l")
plot(ts, noisedSig, type="l")

#ftt()

library(GeneCycle)

harmonics <- 1:20

f_data_noise <- GeneCycle::periodogram(noise)
plot(f_data_noise$freq[harmonics]*length(noise), 
     f_data_noise$spec[harmonics]/sum(f_data_noise$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")


f_data_signal <- GeneCycle::periodogram(signal)
plot(f_data_signal$freq[harmonics]*length(signal), 
     f_data_signal$spec[harmonics]/sum(f_data_signal$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")


f_data_noisedSig <- GeneCycle::periodogram(noisedSig)
plot(f_data_noisedSig$freq[harmonics]*length(noisedSig), 
     f_data_noisedSig$spec[harmonics]/sum(f_data_noisedSig$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")


?butter     # typ filtra
?filtfilt    # proces filtracji

#https://en.wikipedia.org/wiki/Butterworth_filter

# W = 1/10 - mean 1/10th of Nyquist freq
# Nyquist freq = 0.5 of sampling freq; sampling freq = 1/ sample step

bf <- butter(n = 2, W = 1/10, type = "low" ) 
b1 <- filtfilt(bf, noisedSig)

freqz(bf)

bf2 <- butter(n = 2, W = 1/5, type = "low" ) 
b2 <- filtfilt(bf2, noisedSig)

freqz(bf2)


bf3 <- butter(n = 2, W = 1, type = "low" ) 
b3 <- filtfilt(bf3, noisedSig)
freqz(bf3)


bf4 <- butter(n = 2, W = 1/15, type = "low" ) 
b4 <- filtfilt(bf4, noisedSig)
freqz(bf4)


plot(x = ts, y = noisedSig, cex =  .5)

#plot(ts, b1, type = "l")
lines(ts, b1, type = "l", col = "green")
lines(ts, b2, type = "l", col = "red")
lines(ts, b3, type = "l", col = "yellow")
lines(ts, b4, type = "l", col = "blue")


bf5 <- butter(n = 2, W = 1/20, type = "low" ) 
b5 <- filtfilt(bf5, noisedSig)
freqz(bf5)

lines(ts, b5, type = "l", col = "pink")

# W = 1/20 juz zaczyna splaszczac sygnaly


head(b4)

b_temp<-b2

f_data_temp <- GeneCycle::periodogram(b_temp)
plot(f_data_temp$freq[harmonics]*length(b_temp), 
     f_data_temp$spec[harmonics]/sum(f_data_temp$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h", main = "1/5")



#################


bfH <- butter(n = 2, W = 1/10, type = "high" ) 
bh1 <- filtfilt(bfH, noisedSig)

plot(x = ts, y = noisedSig, cex =  .5)
lines(ts, bh1, type = "l", col = "green")

b_temp<-bh1

f_data_temp <- GeneCycle::periodogram(b_temp)
plot(f_data_temp$freq[harmonics]*length(b_temp), 
     f_data_temp$spec[harmonics]/sum(f_data_temp$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h", main = "h 1/10")

