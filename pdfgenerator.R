# Install required packages if not already installed
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("tinytex")) install.packages("tinytex"); tinytex::install_tinytex()

# Render the PDF
rmarkdown::render(
  "Lab10_Report.Rmd",
  output_file = "Lab10_Report.pdf",
  output_options = list(pdf_engine = "xelatex")
)

cat("✅ PDF generated successfully as 'Lab10_Report_Detailed.pdf'\n")
