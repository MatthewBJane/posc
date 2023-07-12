
posc_uni <- function(r,
                     n,
                     sd_x = 1,
                     predictor_name = 'X',
                     outcome_name = 'Y',
                     color = 'black',
                     ci.lvl = .95){

  options(warn=-1)


  if( ci.lvl >= 1 || ci.lvl <= 0 ){
    stop("ci.lvl must be within the interval (0-1). Example: ci.lvl = .95")
  }



  if(sign(r) == 1 || sign(r) == 0){
    ystring = 'Probability of higher'
  } else if(sign(r) == -1){
    ystring = 'Probability of lower'
  }

  r = abs(r)
  delta_x = seq(0,4,length.out=100)
  py = pnorm(r*delta_x/sqrt(2*(1-r^2)))
  sigma  = sd_x*sqrt(2)
  xpdf = 1 / sqrt(pi) * exp(-delta_x^2 / 4)

  z = qnorm(ci.lvl+(1-ci.lvl)/2)
  rU = tanh(atanh(r) + z/sqrt(n-3))
  rL = tanh(atanh(r) - z/sqrt(n-3))

  pU = pnorm(rU*delta_x/sqrt(2*(1-rU^2)))
  pL = pnorm(rL*delta_x/sqrt(2*(1-rL^2)))

  main_plot = ggplot(data = NULL, aes(x = sigma*delta_x, y = py)) +
    geom_line(linewidth=1.5,color = color) +
    coord_cartesian(ylim=c(.5,1),xlim = c(0,sigma*4)) +
    geom_ribbon(data=NULL,aes(x = sigma*delta_x,ymin = pL,ymax = pU),alpha=.15,fill = color) +
    theme_light() +
    theme(axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12)) +
    ylab(paste(ystring,outcome_name,sep = ' ')) +
    xlab(paste('Difference in',predictor_name, ifelse(sigma==1,'(SD)',''),sep = ' '))+
    ggtitle('Probability of Outcome Superiority Curve (POSC)')


    print(main_plot)

invisible(list(
  parameters = data.frame(r = r, n = n),
  predict_matrix = data.frame(delta_x, py, pyUCI = pU, pyLCI = pL),
  posc_plot_object = main_plot)
  )



}
