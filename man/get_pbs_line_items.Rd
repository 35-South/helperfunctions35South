% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pbs_data.R
\name{get_pbs_line_items}
\alias{get_pbs_line_items}
\title{Download, read, clean PBS line items}
\usage{
get_pbs_line_items()
}
\value{
(tibble) A timeseries of Budget Data going back to 2014-15
}
\description{
The is function will fetch data from data.gov on PBS expense items. It will
then clean and join all the data and return data in long form. The data will
be a timeseries going back to 2014-15 Budget.
}
\examples{
\dontrun{

pbs_data <- get_pbs_line_items()

}
}
