PHONY: clean

clean:
	rm derived_data/*
	rm logs/*
	rm figures/*

derived_data/deduplicated_%.csv logs/deduplication_%.csv:\
  source_data/%.csv deduplicate.R
	Rscript deduplicate.R $*

derived_data/gender_data.csv: derived_data/deduplicated_character-data.csv make_gender_dataset.R
	Rscript make_gender_dataset.R

derived_data/power_gender_data.csv derived_data/power_gender_ranks.csv: derived_data/deduplicated_powers.csv derived_data/gender_data.csv make_power_gender_dataset.R
	Rscript make_power_gender_dataset.R

figures/power_gender_rank.png: figure_power_gender_rank.R derived_data/power_gender_ranks.csv
	Rscript figure_power_gender_rank.R
