# ggplot styling
theme_ochre <-  function(){
  list(scale_x_continuous(limits=c(0,500000),
                          labels = function(x) format(x/1000, scientific = FALSE)),
       theme_bw(),
       labs(x='Age ka BP',
            y='Density'))
}
saveRDS(theme_ochre, 'work/theme_ochre.rds')
