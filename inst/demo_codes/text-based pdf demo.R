library(pdftools)

pdf_txt_fl <- 'inst/raw_data/tzw 401K statement.pdf'

pdf_txt_data <- pdftools::pdf_data(pdf_txt_fl)
