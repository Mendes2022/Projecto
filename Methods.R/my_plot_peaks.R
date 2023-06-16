my_plot_peaks <- function (dataset, column.class, samples = NULL, variable.bounds = NULL, 
  xlab = NULL, ylab = NULL, legend.place = "topright", cex = 0.8, 
  reverse.x = FALSE, p.size = 0.5, ...) 
{
  if (is.null(xlab)) 
    xlab = get_x_label(dataset)
  if (is.null(ylab)) 
    ylab = get_value_label(dataset)
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
  if (reverse.x) 
    xlim = c(max(as.numeric(vars)), min(as.numeric(vars)))
  else xlim = range(as.numeric(vars))
  if (is.null(samples)) {
    samples = colnames(dataset$data)
  }
  matplot(vars, dataset$data[variables, samples], col = 1:length(samples), 
    xlab = xlab, ylab = ylab, xlim = xlim, type = "p", pch = 16, 
    cex = p.size, ...)
  if (legend.place != "none") 
    legend(legend.place, colnames(dataset$data), cex = cex, fill = sort(as.integer(factor(1:length(samples)))))
}
