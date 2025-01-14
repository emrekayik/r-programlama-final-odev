# PowerPoint şablonu oluşturma script'i
library(officer)

# Create a new presentation
ppt <- read_pptx()

# Add title slide
ppt <- add_slide(ppt, layout = "Title Slide")

# Add section slide
ppt <- add_slide(ppt, layout = "Section Header")

# Add content slide
ppt <- add_slide(ppt, layout = "Title and Content")

# Add two content slide
ppt <- add_slide(ppt, layout = "Two Content")

# Save template
print(ppt, target = "template.pptx") 

