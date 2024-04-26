# 轉換日期格式----
bond$期間 <- as.Date(as.character(bond$期間), format = "%Y")
bond$期間 <- as.Date(paste(bond$期間, "-01-01", sep = ""), format = "%Y-%m-%d")




