# test_df <- data.frame(date = seq.Date(from = ymd("1999-01-01"),
#                                       to = ymd("2000-12-31"),
#                                       by = 1)) %>%
#   mutate(tmin = 12 +
#            8 * sin(-pi/2 + 2*pi*(yday(date) + 20)/365.25) +
#            rnorm(n = n(),
#                  mean = 0,
#                  sd = 1.5 * (1 - 0.6*sin(-pi/2 + 2*pi*yday(date)/365.25)))
#          ,
#          tmax = pmax(tmin + 2,
#                      22 +
#                        6 * sin(-pi/2 + 2*pi*(yday(date) + 20)/365.25) +
#                        rnorm(n = n(),
#                              mean = 0,
#                              sd = 1 * (1 - 0.6*sin(-pi/2 + 2*pi*yday(date)/365.25))))) %>%
#   na.omit()
#
# temp_out <- seasonal_cycle(df = test_df,
#                            .cols = c("tmax", "tmin"),
#                            quantile = c(0.05,0.5,0.95),
#                            n_harmonics = 4)
#
# ggplot(temp_out %>%
#          pivot_longer(-date, values_to = "measurement", names_to = "var"),
#        aes(x = date,
#            y = measurement,
#            group = var,
#            linetype = var,
#            color = var)) +
#   geom_path() +
#   scale_color_manual(values = c("red", "blue", "red", "black")) +
#   scale_shape_manual(values = c(1, 1, 1, 19)) +
#   scale_linetype_manual(values = c(3, 4, 3, 1))
