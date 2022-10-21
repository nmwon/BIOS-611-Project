PHONY: clean

clean:
	rm derived_data/*
	rm logs/*
	rm figures/*


figures/bar_age_graph.png: FinalProject.R Data/MD/NIBRS_offender.csv
	Rscript FinalProject.R
	
figures/bar_sex_graph.png: FinalProject.R Data/MD/NIBRS_offender.csv
	Rscript FinalProject.R

Report.pdf: FinalProject.rmd FinalProject.R Data/MD/NIBRS_offender.csv
	R -r "rmarkdown::render(\"FinalProject.Rmd\", output_format=\"pdf_document\")"
        