# filtering signals in freq domain

library(signal)

acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)

signal <- 3*sin(3*w*ts) 

noise <- 0.9 * sin(12*w*ts)

noise2 <- 1.1 * sin(7*w*ts)

noisedSig <- signal + noise
noisedSig <- signal + noise + noise2


plot(signal, type = "l")
plot(noise, type = "l")
plot(noisedSig, type="l")

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

bf <- butter(n = 2, W = 1/10, type = "low" ) 
b1 <- filtfilt(bf, noisedSig)

freqz(bf)

bf2 <- butter(n = 2, W = 1/5, type = "low" ) 
b2 <- filtfilt(bf2, noisedSig)

freqz(bf2)

plot(ts, b1, type = "l")
lines(ts, b2, type = "l", col = "red")


