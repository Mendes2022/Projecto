# Generated from function body. Editing this file has no effect.
my_plot_spectra <- function (dataset, column.class, func = NULL, samples = NULL, 
          variable.bounds = NULL, xlab = NULL, ylab = NULL, lty = 1, 
          legend.place = "topright", cex = 0.8, reverse.x = FALSE, 
          ...) 
{
  if (is.null(xlab)) 
    xlab = get_x_label(dataset)
  if (is.null(ylab)) 
    ylab = get_value_label(dataset)
  if (!is.null(dataset$labels$x) && !is.expression(dataset$labels$x) && 
      dataset$labels$x == "mz/rt") {
    if (is.null(variable.bounds)) {
      variables = 1:length(get_x_values_as_text(dataset))
      vars = as.numeric(gsub("/.*", "", get_x_values_as_text(dataset)))
    }
    else {
      x.vars = as.numeric(gsub("/.*", "", get_x_values_as_text(dataset)))
      variables = which(x.vars > variable.bounds[1] & x.vars < 
                          variable.bounds[2])
      vars = x.vars[variables]
    }
  }
  else {
    if (is.null(variable.bounds)) {
      variables = rownames(dataset$data)
      vars = variables
    }
    else {
      x.vars = get_x_values_as_num(dataset)
      variables = rownames(dataset$data)[x.vars > variable.bounds[1] & 
                                           x.vars < variable.bounds[2]]
      vars = variables
    }
  }
  if (reverse.x) 
    xlim = c(max(as.numeric(vars)), min(as.numeric(vars)))
  else xlim = range(as.numeric(vars))
  if (is.null(samples)) {
    samples = colnames(dataset$data)
  }
  if (is.null(func)) {
    matplot(vars, dataset$data[variables, samples], type = "l", 
            lty = lty, col = 1:length(samples), xlab = xlab, 
            ylab = ylab, xlim = xlim, ...)
    if (legend.place != "none") 
      legend(legend.place, colnames(dataset$data[variables, samples]), cex = cex, 
             fill = sort(as.integer(factor(1:length(samples)))))
  }
  else {
    aggregate.result = aggregate(t(dataset$data[variables, 
                                                samples]), by = list(rep(1:length(samples), each = length(variables))), func)
    matplot(vars, t(aggregate.result[-1]), type = "l", lty = 1, 
            col = 1:length(samples), xlab = xlab, 
            ylab = ylab, xlim = xlim, ...)
    if (legend.place != "none") 
      legend(legend.place, colnames(dataset$data[variables, samples]), cex = cex, 
             fill = sort(as.integer(factor(1:length(samples)))))
  }
}
