posc_comp <- function(posc_1, posc_2, color1 = 'red', color2 = 'blue'){

options(warn=-1)

posc_mdl = c(rep('1',nrow(posc_1$predict_matrix)), rep('2',nrow(posc_2$predict_matrix)) )
posc_preds = rbind( posc_1$predict_matrix,posc_2$predict_matrix )
df = cbind(posc_mdl, posc_preds)


z1 = atanh(posc_1$parameters$r)
z2 = atanh(posc_2$parameters$r)
n1 = posc_1$parameters$n
n2 = posc_2$parameters$n

z = round((z1 - z2) / sqrt( (1 / (n1 - 3)) + (1 / (n2 - 3)) ) , 3)
p = round(2*(1-pnorm(abs(z))),4)


main_plot = ggplot(data=df, aes(x=delta_x,y=py,group=posc_mdl, color = posc_mdl,fill = posc_mdl)) +
  geom_line(linewidth=1.5) +
  coord_cartesian(ylim=c(.5,1),xlim = c(0,4)) +
  theme_light() +
  geom_ribbon(data=NULL,aes(x = delta_x,ymin = pyLCI,ymax = pyUCI),alpha=.15,color=NA) +
  scale_color_manual(values = c(color1,color2)) +
  scale_fill_manual(values = c(color1,color2)) +
  theme(axis.text.x = element_text(size=11),
      axis.text.y = element_text(size=11),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      aspect.ratio = .8) +
  ylab('Probability of Higher Outcome') +
  xlab('Difference in Predictor (SD)') +
  ggtitle('Probability of Outcome Superiority Curve (POSC)')

 stats = data.frame(r = c(posc_1$parameters$r,posc_2$parameters$r),
                    n = c(posc_1$parameters$n,posc_2$parameters$n),
                    z = c('',z),
                    p = c('',p))

 print(stats)
 print(main_plot)

  invisible(list(stats = stats,
                 posc_plot_object = main_plot))

  }
