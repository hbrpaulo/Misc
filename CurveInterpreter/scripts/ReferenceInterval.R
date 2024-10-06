resi_cs <- resi_cs %>% 
  mutate(out_sup = ifelse(res<ref_sup, 'inside', 'outside'),
         out_inf = ifelse(res>ref_inf, 'inside', 'outside'),
         out = ifelse(out_sup=='outside'|out_inf=='outside', 'outside', 'inside'))

beginning <- unname(quantile(resi_cs$x, divisor))
end <- unname(quantile(resi_cs$x, 1-divisor)) 

# definir factor levels
resi_cs$part <- factor(ifelse(resi_cs$x<beginning, 'beginning', 
                              ifelse(resi_cs$x>end, 'end', 'middle')), 
                       levels = c('beginning','middle','end'))

attr(resi_cs, 'beginning') <- beginning
attr(resi_cs, 'end') <- end

# separacao de inicio|meio|fim usando quantis ----
resi_cs_middle <- resi_cs %>%
  filter(between(x, attr(resi_cs, 'beginning'),
                 attr(resi_cs, 'end')))

# atual ----
output$ggplotff <- renderPlot({
  source('scripts/ReferenceInterval/IntervalMainPlot.R')
})

output$trends <- renderPrint({
  source('scripts/ReferenceInterval/IntervalTrends.R')
})

output$outside <- renderPrint({
  source('scripts/ReferenceInterval/IntervalOutside.R')
})
