posc_lm <- function(model, predictor_name = 'Predicted Y',
                      outcome_name = 'Y',
                      color = 'black',
                      ci.lvl = .95){

  options(warn=-1)

 x = model$fitted.values
 y = model$model$y
 sigma = sd(x) * sqrt(2)
 n = nrow(mdl$model)
 r = abs(cor(x,y))

 delta_x = seq(0,4,length.out=100)
 py = pnorm(r*delta_x/sqrt(2*(1-r^2)))

 z = qnorm(ci.lvl+(1-ci.lvl)/2)
 rU = tanh(atanh(r) + z/sqrt(n-3))
 rL = tanh(atanh(r) - z/sqrt(n-3))

 pU = pnorm(rU*delta_x/sqrt(2*(1-rU^2)))
 pL = pnorm(rL*delta_x/sqrt(2*(1-rL^2)))

 if(cor(x,y)>=0){
   ystring = 'Probability of higher Y'
 } else if(cor(x,y) < 0){
   ystring = 'Probability of lower Y'
 }

 main_plot = ggplot(data = NULL, aes(x = sigma*delta_x, y = py)) +
   geom_line(linewidth=1.5,color = color) +
   coord_cartesian(ylim=c(.5,1),xlim = c(0,sigma*4)) +
   geom_ribbon(data=NULL,aes(x = sigma*delta_x,ymin = pL,ymax = pU),alpha=.15,fill = color) +
   theme_light() +
   theme(axis.text.x = element_text(size=11),
         axis.text.y = element_text(size=11),
         axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12),
         panel.background = element_rect()) +
   ylab(paste(ystring,outcome_name,sep = ' ')) +
   xlab(paste('Difference in',predictor_name,sep = ' '))+
   ggtitle('Probability of Outcome Superiority Curve (POSC)')


   print(main_plot)

    invisible(list(
      parameters = data.frame(r = r, n = n),
      predict_matrix = data.frame(delta_x, py, pyUCI = pU, pyLCI = pL),
      posc_plot_object = main_plot
    ))

}
