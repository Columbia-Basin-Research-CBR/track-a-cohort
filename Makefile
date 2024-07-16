# Makefile to render Quarto documents using _quarto.yml configuration

# Default target
all: update_data render

# Target to update data
update_data:
            Rscript update_data.R

# Target to render documents
render:
            quarto render