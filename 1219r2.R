#1번: 엑셀 데이터 빼내기기

library(data.table); library(tidyverse); library(tableone)
# Load file
url <- "https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv"
g1e <- fread(url,header=T)

# Table 1 - htwt
htwt <- g1e[EXMD_BZ_YYYY==2009] %>%
  summarize(meanHGHT = mean(HGHT), meanWGHT = mean(WGHT))

# Table 2 - smk
smk <- g1e[EXMD_BZ_YYYY==2009] %>%
  CreateTableOne(vars=c("BP_SYS", "BP_DIA"), strata="Q_SMK_YN") %>%
  print(quote=F, noSpaces=T) %>% as.data.frame()


library(openxlsx)
write.xlsx(htwt, file = "htwt.xlsx")
write.xlsx(smk, file = "smk.xlsx", rowNames=T, overwrite=T)

tablelist <- list(htwt, smk)
write.xlsx(tablelist, file = "tables.xlsx", rowNames=T)

# create workbook
wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "htwt")
writeData(wb = wb, sheet = "htwt", x = htwt)

addWorksheet(wb = wb, sheetName = "smk")
writeData(wb = wb, sheet = "smk", x = smk, rowNames=T)

# view without saving
openXL(wb)
# save
saveWorkbook(wb, "tables.xlsx", overwrite = TRUE) 


#2:그림 ppt로
library(data.table); library(tidyverse); library(ggplot2)

# Plot 1 - ht_plot
ht_plot <- g1e[EXMD_BZ_YYYY==2009] %>% ggplot(aes(x=HGHT)) + geom_histogram()

# Plot 2 - smk_plot
smk_plot <- g1e[,':='(Year=as.factor(EXMD_BZ_YYYY), smk=as.factor(Q_SMK_YN))] %>%
  ggplot(aes(x=Year, fill=smk)) + geom_bar(position='fill')

library(rvg); library(officer)

plot1 <- read_pptx()
plot1 <- add_slide(plot1)
plot1 <- ph_with(x = plot1, ht_plot, location=ph_location_type(type="body"))
print(plot1, target = "plot1.pptx")

plots <- read_pptx() %>% add_slide() %>%
  ph_with(ht_plot, location=ph_location_type(type="body"))
plots <- plots %>% add_slide() %>%
  ph_with(dml(ggobj = smk_plot), location=ph_location_type(type="body"))
plots <- plots %>% add_slide() %>%
  ph_with(smk, location=ph_location_type(type="body"))

print(plots, target = "plots.pptx")

library(devEMF)
emf("smk_plot.emf", width = 7, height = 7, emfPlus = F)
print(smk_plot)
dev.off()