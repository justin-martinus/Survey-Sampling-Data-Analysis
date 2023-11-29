# Load the required library
packages <- c("ca", "ggplot2", "psych", "dplyr")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

# Create a contingency table
# Replace 'df' with your actual data frame and provide the appropriate column names
df <- read.csv("C:/Users/ASUS/OneDrive/Documents/CSV Hold/(Processed2)Kuesioner BOP Mahasiswa Departemen Matematika UI.csv")


contingency_table1 <- table(df$X1..Berapa.penghasilan.keluarga.Anda.per.bulan., 
                           df$X14..Secara.keseluruhan..seberapa.puas.Anda.dengan.biaya.pendidikan.yang.diberikan.pihak.kampus.tanpa.bantuan.biaya.pendidikan.)
contingency_table1 <- contingency_table1[, c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied")]
contingency_table1 <- contingency_table1[c("8.000.000 ke bawah", "8.000.001-15.000.000", "15.000.001-30.000.000", "30.000.001-50.000.000", "di atas 50.000.000"),]


contingency_table3 <- table(df$X11..Apakah.yang.fasilitas.dan.akses.yang.Anda.dapatkan.sesuai.dengan.jumlah.biaya.yang.Anda.keluarkan., 
                            df$X14..Secara.keseluruhan..seberapa.puas.Anda.dengan.biaya.pendidikan.yang.diberikan.pihak.kampus.tanpa.bantuan.biaya.pendidikan.)
contingency_table3 <- contingency_table3[, c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied")]
contingency_table3 <- contingency_table3[c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied"),]


contingency_table4 <- table(df$X12..Apakah.Anda.pernah.merasa.biaya.pendidikan.yang.diberikan.pihak.kampus.tidak.adil.atau.melebihi.kemampuan.keluarga.Anda., 
                            df$X14..Secara.keseluruhan..seberapa.puas.Anda.dengan.biaya.pendidikan.yang.diberikan.pihak.kampus.tanpa.bantuan.biaya.pendidikan.)
contingency_table4 <- contingency_table4[, c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied")]
contingency_table4 <- contingency_table4[c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied"),]


contingency_table5 <- table(df$X1..Berapa.penghasilan.keluarga.Anda.per.bulan., 
                            df$X12..Apakah.Anda.pernah.merasa.biaya.pendidikan.yang.diberikan.pihak.kampus.tidak.adil.atau.melebihi.kemampuan.keluarga.Anda.)
contingency_table5 <- contingency_table5[, c("very unsatisfied", "unsatisfied", "neutral", "satisfied", "very satisfied")]
contingency_table5 <- contingency_table5[c("8.000.000 ke bawah", "8.000.001-15.000.000", "15.000.001-30.000.000", "30.000.001-50.000.000", "di atas 50.000.000"),]

# Perform correspondence analysis for each contingency table

contingency_table1
contingency_table3
contingency_table4
contingency_table5

result1 <- ca::ca(contingency_table1, graph=FALSE)
result3 <- ca::ca(contingency_table3, graph=FALSE)
result4 <- ca::ca(contingency_table4, graph=FALSE)
result5 <- ca::ca(contingency_table5, graph=FALSE)



# Print the row and column scores for each analysis
print(result1$rowcoord)
print(result1$colcoord)
print(result3$rowcoord)
print(result3$colcoord)
print(result4$rowcoord)
print(result4$colcoord)
print(result5$rowcoord)
print(result5$colcoord)





# Convert row coordinates to a data frame
row_coords1 <- as.data.frame(result1$rowcoord)
row_coords3 <- as.data.frame(result3$rowcoord)
row_coords4 <- as.data.frame(result4$rowcoord)
row_coords5 <- as.data.frame(result5$rowcoord)
col_coords1 <- as.data.frame(result1$colcoord)
col_coords3 <- as.data.frame(result3$colcoord)
col_coords4 <- as.data.frame(result4$colcoord)
col_coords5 <- as.data.frame(result5$colcoord)





# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = row_coords1, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(row_coords1)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Penghasilan-Kepuasan BOP", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = col_coords1, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(col_coords1)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Kepuasan BOP-Penghasilan", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()




# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = row_coords3, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(row_coords3)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Fasilitas-Kepuasan BOP", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = col_coords3, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(col_coords3)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Kepuasan BOP-Fasilitas", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = row_coords4, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(row_coords4)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Keadilan BOP-Kepuasan BOP", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = col_coords4, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(col_coords4)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Kepuasan BOP-Keadilan BOP", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()



# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = row_coords5, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(row_coords5)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Penghasilan-Keadilan BOP", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()



# Plot the biplot-like visualization with different colors for dots and arrows
ggplot(data = col_coords5, aes(x = Dim1, y = Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = rownames(col_coords5)), size = 4) +
  geom_segment(aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")), size = 0.5, color = "blue") +
  geom_point(size = 3, color = "red") +  # Add dots with different color
  labs(title = "Correspondence Analysis Keadilan BOP-Penghasilan", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()



# Define the Likert scale categories and their corresponding numeric values
likert_mapping <- c("very unsatisfied" = 1, "unsatisfied" = 2, "neutral" = 3, "satisfied" = 4, "very satisfied" = 5)

# Iterate over each column
for (col in colnames(df)) {
  if (any(df[[col]] %in% names(likert_mapping))) {
    df[[col]] <- df[[col]] %>% recode(!!!likert_mapping, .default = NA_real_)
  }
}

likert_vars <- df[, c("X2..Bagaimana.pendapat.Anda.dengan.transparansi.pihak.kampus.terkait.alasan.biaya.pendidikan.ditetapkan.sejumlah.yang.diterapkan.ditetapkan.untuk.Anda.",
                      "X3..Bagaimana.pendapat.anda.dengan.ketersediaan.bantuan.biaya.pendidikan.yang.disediakan.kampus.dalam.berkemungkinan.mengurangi.beban.biaya.pendidikan.",
                      "X4..Bagaimana.pendapat.Anda.dengan.pertimbangan.pihak.kampus.dalam.menentukan.biaya.pendidikan.",
                      "X10..Apakah.biaya.pendidikan.Anda.sesuai.dengan.harapan.Anda.",
                      "X11..Apakah.yang.fasilitas.dan.akses.yang.Anda.dapatkan.sesuai.dengan.jumlah.biaya.yang.Anda.keluarkan.", 
                      "X12..Apakah.Anda.pernah.merasa.biaya.pendidikan.yang.diberikan.pihak.kampus.tidak.adil.atau.melebihi.kemampuan.keluarga.Anda.",
                      "X14..Secara.keseluruhan..seberapa.puas.Anda.dengan.biaya.pendidikan.yang.diberikan.pihak.kampus.tanpa.bantuan.biaya.pendidikan.")]








# Calculate Spearman correlation matrix
cor_matrix <- cor(likert_vars, method = "spearman")



# Split the correlation matrix into 3x3 submatrices
submatrices <- split(cor_matrix, rep(1:(ncol(cor_matrix)/7), each = 7))




# Print the submatrices without column names
for (submatrix in submatrices) {
  # Remove column names
  colnames(submatrix) <- NULL
  
  # Print the submatrix in a 3x3 format
  print(matrix(unlist(submatrix), nrow = 7), quote = FALSE)
  cat("\n")
}; print("    Q2         Q3          Q4         Q10       Q11        Q12         Q14")

# Calculate the reliability coefficients and check for reversed items
reliability <- alpha(likert_vars, check.keys = TRUE)

# Print the reliability coefficients
print(reliability$total, quote = FALSE)





df_subset <- df[c("X4..Bagaimana.pendapat.Anda.dengan.pertimbangan.pihak.kampus.dalam.menentukan.biaya.pendidikan.",
                  "X7..Seberapa.puas.Anda.dengan.bantuan.biaya.pendidikan.yang.Anda.ikuti.", 
                  "X8..Apakah.Anda.merasa.sudah.terdapat.cukup.banyak.pilihan.atau.jumlah.bantuan.biaya.pendidikan.yang.disediakan.dari.pihak.kampus.", 
                  "X9..Menurut.Anda..seberapa.susah.mendapatkan.bantuan.biaya.pendidikan.yang.disediakan.dari.pihak.kampus.",
                  "X10..Apakah.biaya.pendidikan.Anda.sesuai.dengan.harapan.Anda.",
                  "X11..Apakah.yang.fasilitas.dan.akses.yang.Anda.dapatkan.sesuai.dengan.jumlah.biaya.yang.Anda.keluarkan.", 
                  "X14..Secara.keseluruhan..seberapa.puas.Anda.dengan.biaya.pendidikan.yang.diberikan.pihak.kampus.tanpa.bantuan.biaya.pendidikan.")]
                  
    
df_subset_complete <- na.omit(df_subset)


# Calculate Spearman correlation matrix
cor_matrix <- cor(df_subset_complete, method = "spearman")



# Split the correlation matrix into 3x3 submatrices
submatrices <- split(cor_matrix, rep(1:(ncol(cor_matrix)/7), each = 7))




# Print the submatrices without column names
for (submatrix in submatrices) {
  # Remove column names
  colnames(submatrix) <- NULL
  
  # Print the submatrix in a 3x3 format
  print(matrix(unlist(submatrix), nrow = 7), quote = FALSE)
  cat("\n")
}; print("   Q4         Q7         Q8        Q9       Q10       Q11        Q14")

# Calculate the reliability coefficients and check for reversed items
reliability <- alpha(df_subset_complete, check.keys = TRUE)

# Print the reliability coefficients
print(reliability$total, quote = FALSE)
