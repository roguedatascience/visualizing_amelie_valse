library(tuneR)
library(tidyverse)
library(signal) # signal processing functions
#library(oce) # image plotting functions and nice color maps

####################
# Instagram post: Juno filter
####################

#https://hansenjohnson.org/post/spectrograms-in-r/

#library(tuneR) # nice functions for reading and manipulating .wav files

# read in audio file
data =
    readMP3('la_valse_d-amelie.mp3')

# perc <- 1
# 
# for(i in seq(0.1, 1, 0.1)){
#     
#     perc <- c(perc, floor(length(data@left) * i))
#     
# }

# extract signal
#snd = data@left[perc[7]:perc[8]]
snd_l <- data@left
snd_r <- data@right

# determine duration
dur <- length(snd_l) / data@samp.rate

# determine sample rate
fs <- data@samp.rate


##############

# number of points to use for the fft
nfft <- 1024

# window size (in points)
window <- 256

# overlap (in points)
overlap <- 128



###############


# create spectrogram
spec_l <-
    specgram(x = snd_l,
             n = nfft,
             Fs = fs,
             window = window,
             overlap = overlap)

spec_r <-
    specgram(x = snd_r,
             n = nfft,
             Fs = fs,
             window = window,
             overlap = overlap)

# discard phase information
p_l <- abs(spec_l$S)
p_r <- abs(spec_r$S)

# normalize
p_l <- p_l / max(p_l)
p_r <- p_r / max(p_r)

# convert to dB
p_l <- 10 * log10(p_l)
p_r <- 10 * log10(p_r)

# config time axis
t <- spec_l$t

# plot spectrogram
# img <-
#     imagep(x = t,
#            y = spec$f,
#            z = t(P),
#            col = oce.colorsViridis,
#            ylab = 'Frequency [Hz]',
#            xlab = 'Time [s]',
#            drawPalette = T,
#            decimate = F)


mat_l_df <-
    p_l %>%
    as_data_frame() %>%
    rownames_to_column('freq') %>%
    mutate(freq = as.numeric(freq)) %>%
    gather('var', 'val', -freq) %>%
    mutate(var =
               var %>%
               str_replace('V', '') %>%
               as.numeric()) %>%
    dplyr::filter(!is.infinite(val))

mat_r_df <-
    p_r %>%
    as_data_frame() %>%
    rownames_to_column('freq') %>%
    mutate(freq = as.numeric(freq)) %>%
    gather('var', 'val', -freq) %>%
    mutate(var =
               var %>%
               str_replace('V', '') %>%
               as.numeric()) %>%
    dplyr::filter(!is.infinite(val))

mat_df_trim <-
    bind_rows(
        mat_l_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.4),
        mat_r_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.4) %>%
            mutate(freq = -freq)
    )

png('scatter_final.png', width = 8000, height = 1600)

mat_df_trim %>%
    ggplot(aes(x = var, y = freq, col = val)) +
    geom_point(size = 3, alpha = .1, shape = 16) +
    scale_color_continuous(guide = FALSE, low="blue", high="red") +
    #scale_fill_continuous(guide = FALSE, (type = "viridis")) +
    theme_void()

dev.off()





############################################################
#
# Purple, Blue, Black - Full
#
############################################################

#https://pinetools.com/split-image

set.seed(13)

mat_df_trim_2 <-
    bind_rows(
        mat_l_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.05),
        mat_r_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.05) %>%
            mutate(freq = -freq)
    ) %>%
    mutate(val =
               ifelse(val < -55, -55, val))

# png('scatter_final_2.png', width = 6000, height = 2000)
# 
# mat_df_trim_2 %>%
#     ggplot(aes(x = var, y = freq, col = val)) +
#     geom_point(size = 5, alpha = .9, shape = 16) +
#     scale_color_continuous(guide = FALSE, low = '#99298C', high = '#E72F31') + #purple - red
#     #scale_color_continuous(guide = FALSE, low = '#293688', high = '#EC2A39') + #blue - red
#     #scale_color_continuous(guide = FALSE, low = '#8C05C4', high = '#E72F31') +
#     #scale_color_continuous(guide = FALSE, low = 'blue', high = '#E72F31') +
#     #scale_color_continuous(guide = FALSE, low = '#201B1C', high = '#E72F31') +
#     #scale_color_continuous(guide = FALSE, low = '#4FA345', high = '#E72F31') +
#     #scale_fill_continuous(guide = FALSE, (type = "viridis")) +
#     theme_void() +
#     scale_y_continuous(
#         expand = expand_scale(mult = 0.05)
#     ) +
#     scale_x_continuous(
#         expand = expand_scale(mult = 0.015)
#     )
# 
# dev.off()

# 
# png('scatter_final_purple_red.png', width = 6000, height = 2000)
# 
# mat_df_trim_2 %>%
#     ggplot(aes(x = var, y = freq, col = val)) +
#     geom_point(size = 5, alpha = .9, shape = 16) +
#     scale_color_continuous(guide = FALSE, low = '#99298C', high = '#E72F31') + #purple - red
#     theme_void() +
#     scale_y_continuous( expand = expand_scale(mult = 0.05) ) +
#     scale_x_continuous( expand = expand_scale(mult = 0.015) )
# 
# dev.off()
# 
# 
# png('scatter_final_blue_red.png', width = 6000, height = 2000)
# 
# mat_df_trim_2 %>%
#     ggplot(aes(x = var, y = freq, col = val)) +
#     geom_point(size = 5, alpha = .9, shape = 16) +
#     scale_color_continuous(guide = FALSE, low = '#293688', high = '#E72F31') + #blue - red
#     theme_void() +
#     scale_y_continuous( expand = expand_scale(mult = 0.05) ) +
#     scale_x_continuous( expand = expand_scale(mult = 0.015) )
# 
# dev.off()


png('scatter_final_full.png', width = 6000, height = 2000)

mat_df_trim_2 %>%
    ggplot(aes(x = var, y = freq, col = val)) +
    geom_point(size = 5, alpha = .9, shape = 16) +
    scale_color_continuous(guide = FALSE, low = '#201B1C', high = '#E72F31') + #black - red
    theme_void() +
    scale_y_continuous( expand = expand_scale(mult = 0.05) ) +
    scale_x_continuous( expand = expand_scale(mult = 0.015) )

dev.off()




############################################################
#
# heavy samping
#
############################################################

#https://pinetools.com/split-image

set.seed(13)

mat_df_trim_temp <-
    bind_rows(
        mat_l_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.05),
        mat_r_df %>%
            #dplyr::filter(val > -25) %>%
            dplyr::filter(freq < 350) %>%
            sample_frac(.05) %>%
            mutate(freq = -freq)
    )

mat_df_trim_join <-
    bind_rows(
        mat_df_trim_temp %>%
            dplyr::filter(val >= -10) %>%
            sample_frac(.5),
        mat_df_trim_temp %>%
            dplyr::filter(val >= -20 & val < -10) %>%
            sample_frac(.175),
        mat_df_trim_temp %>%
            dplyr::filter(val >= -30 & val < -20) %>%
            sample_frac(.125),
        mat_df_trim_temp %>%
            dplyr::filter(val >= -40 & val < -30) %>%
            sample_frac(.1),
        mat_df_trim_temp %>%
            dplyr::filter(val >= -50 & val < -40) %>%
            sample_frac(.075),
        mat_df_trim_temp %>%
            dplyr::filter(val < -55) %>%
            sample_frac(.025)
    ) %>%
    mutate(val =
               ifelse(val < -60, -60, val)) %>%
    mutate(val2 = (abs(val) + 1)^1.6) %>%
    arrange(val)

png('scatter_final_scatter.png', width = 6000, height = 2000)

mat_df_trim_join %>%
    ggplot(aes(x = var, y = freq, col = val)) +
    geom_point(aes(size = val2, alpha = -abs(val)^1.6), shape = 16) +
    #geom_point(size = 20, alpha = .1, shape = 16) +
    scale_color_continuous(guide = FALSE, low = '#201B1C', high = '#E72F31') + #black - red
    scale_size_continuous(guide = FALSE, range = c(.9, 20)) +
    scale_alpha(guide = FALSE,range = c(.1, .6)) +
    theme_void() +
    scale_y_continuous( expand = expand_scale(mult = 0.05) ) +
    scale_x_continuous( expand = expand_scale(mult = 0.015) )

dev.off()



