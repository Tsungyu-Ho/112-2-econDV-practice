# 載入reticulate套件
library(reticulate)

# 找到Python的安裝路徑
python_path <- py_discover_config()

# 打印出找到的Python路徑
print(python_path)

# 儲存到results list物件中
results <- list(python_path = python_path)


# 指定要使用的 Python 路徑
python_path <- "C:/D-disk/Tsung-yu/ma_1/econDV/112-2-econDV-practice/my_env/Scripts/python.exe"

# 創建一個新的虛擬環境在指定路徑，名為 "r-reticulate"
virtualenv_create(envname = "econDV", python = python_path)

# 使用剛剛建立的虛擬環境
use_virtualenv("econDV", required = TRUE)

# 將創建的虛擬環境資訊儲存到 results 物件中
results_1 <- list(virtualenv = "econDV")