# ===================================================================
# BAGIAN 0: MEMUAT PAKET-PAKET YANG DIBUTUHKAN
# ===================================================================
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT) # Pastikan library DT dimuat
library(forecast)
library(TTR)
library(readr)
library(scales) # Untuk formatting angka seperti persentase
library(dplyr) # Untuk manipulasi data seperti sorting

# ===================================================================
# BAGIAN 1: USER INTERFACE (UI)
# ===================================================================
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Aplikasi Peramalan Deret Waktu", titleWidth = 350),
                    
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Home", tabName = "introduction", icon = icon("info-circle")),
                        menuItem("Pengantar Materi", tabName = "Theorem", icon = icon("book-open")),
                        menuItem("Data & Statistik", tabName = "data_and_desk", icon = icon("table")),
                        menuItem("Metode Smoothing", icon = icon("chart-line"), startExpanded = FALSE,
                                 menuSubItem("Single Moving Average (SMA)", tabName = "SMA", icon = icon("angle-right")),
                                 menuSubItem("Double Moving Average (DMA)", tabName = "DMA", icon = icon("angle-right")),
                                 menuSubItem("Simple Exponential Smoothing (SES)", tabName = "SES", icon = icon("angle-right")),
                                 menuSubItem("Holt's Exponential Smoothing (HES)", tabName = "HES", icon = icon("angle-right")),
                                 menuSubItem("Holt-Winters' Smoothing (HwES)", tabName = "HwES", icon = icon("angle-right"))
                        )
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
                                .content-wrapper, .main-footer {
                                    background-image: url('https://github.com/Myteorite/Belajar/blob/main/gambar/Desain%20tanpa%20judul.jpg?raw=true');
                                    background-size: cover;
                                    background-repeat: no-repeat;
                                    background-position: center;
                                    background-attachment: fixed;
                                }
                            "))
                      ),
                      withMathJax(),
                      tabItems(
                        # --- Tab Home ---
                        tabItem(tabName = "introduction",
                                fluidRow(
                                  box(
                                    title = "Selamat Datang di Aplikasi Peramalan", width = 12, solidHeader = TRUE, status = "primary",
                                    HTML("
                                              <h4 style='font-weight:bold;'>Tentang Aplikasi</h4>
                                              <p>Aplikasi ini dirancang oleh mahasiswa yang sedang menempuh semester 4 di program studi statistika, Fakultas Matematika dan Ilmu Pengetahuan Alam, Universitas Negeri Jakarta. Dilatar belakangi untuk mempermudah dalam melakukan peramalan data deret waktu, Anda dapat mengunggah data Anda sendiri, mengurutkannya berdasarkan tanggal, menganalisisnya, dan membandingkan kinerja berbagai model.</p>
                                              <h4 style='font-weight:bold;'>Petunjuk Penggunaan</h4>
                                              <ol>
                                                <li>Buka menu <strong>Data & Statistik</strong>.</li>
                                                <li>Unggah file data .csv Anda.</li>
                                                <li>Pilih <strong>Pemisah Data</strong>, <strong>Kolom Tanggal</strong>, dan <strong>Format Tanggal</strong> yang sesuai dengan file Anda.</li>
                                                <li>Data Anda akan otomatis terurut berdasarkan tanggal dan siap untuk dianalisis.</li>
                                                <li>Pilih salah satu <strong>Metode Peramalan</strong> dari menu sidebar.</li>
                                                <li>Atur parameter dan tekan tombol <strong>'Jalankan Analisis'</strong>.</li>
                                                <li>Hasil akan disajikan dalam bentuk <strong>Plot</strong>, tabel <strong>Akurasi Model</strong>, dan tabel detail <strong>Perbandingan Data</strong> serta <strong>Hasil Ramalan</strong> yang menyertakan tanggal.</li>
                                              </ol>"
                                    )
                                  ),
                                  box(
                                    title = "TENTANG KAMI", 
                                    width = 12, 
                                    solidHeader = TRUE, 
                                    status = "primary",
                                    HTML("<h4 style='font-weight:bold;'>Dirancang oleh:</h4>
                                          <table>
                                            <tr>
                                              <td>1. Pandu Tegar Faradian</td>
                                              <td>&nbsp;- 1314623005</td>
                                            </tr>
                                            <tr>
                                              <td>2. Shabrina Ghaisani Putri</td>
                                              <td>&nbsp;- 1314623026</td>
                                            </tr>
                                            <tr>
                                              <td>3. Nabilah Rahma Fauziah</td>
                                              <td>&nbsp;- 1314623034</td>
                                            </tr>
                                            <tr>
                                              <td>4. Rahmad Azmy Rizkiyansyah D.</td>
                                              <td>&nbsp;- 1314623054</td>
                                            </tr>
                                            <tr>
                                              <td>5. Kelly Audryna Salsabilla</td>
                                              <td>&nbsp;- 1314623057</td>
                                            </tr>
                                            <tr>
                                              <td>6. Yoshua Fernando Purba</td>
                                              <td>&nbsp;- 1314623061</td>
                                            </tr>
                                          </table>"
                                    )
                                  )
                                )
                        ),
                        
                        # --- Pengantar Materi ---
                        tabItem(tabName = "Theorem",
                                fluidRow(
                                  box(
                                    title = "Landasan Teori Peramalan Deret Waktu", 
                                    width = 12, 
                                    solidHeader = TRUE, 
                                    status = "primary",
                                    
                                    HTML("
                                    <h3 style='text-align:center; font-weight:bold;'>Peramalan Data Deret Waktu</h3>
                                    <p>
                                      <strong>Data deret waktu (time series)</strong> adalah sekumpulan observasi yang diambil secara berkala dalam periode waktu yang sama. Analisis deret waktu berbeda dari metode statistika lainnya karena memperhitungkan ketergantungan antar observasi dari waktu ke waktu. 
                                      <strong>Peramalan (forecasting)</strong> merupakan proses untuk memprediksi nilai di masa depan berdasarkan pola data historis, seperti <em>tren</em> dan <em>musiman</em>.
                                    </p>
                                    <hr>
                                  
                                    <h4 style='text-align:center; font-weight:bold;'>Metode-Metode Peramalan</h4>
                                    
                                    <h5><strong>1. Single Moving Average (SMA)</strong></h5>
                                    <p>Metode dasar yang menghitung rata-rata dari sejumlah observasi terakhir. Paling cocok untuk data stasioner yang tidak memiliki tren atau pola musiman yang jelas.</p>
                                    <p style='text-align:center;'>$$ \\hat{y}_{t} = \\frac{1}{n} \\sum_{i=1}^{n} y_{t-i} $$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(\\hat{y}_{t}\\) = Nilai peramalan, 
                                      \\(y_{t-i}\\) = Nilai aktual pada periode sebelumnya, 
                                      \\(n\\) = Jumlah periode rata-rata.
                                    </p>
                                    
                                    <h5><strong>2. Double Moving Average (DMA)</strong></h5>
                                    <p>Merupakan pengembangan dari SMA yang digunakan untuk data yang menunjukkan adanya <strong>pola tren linier</strong>. Metode ini melakukan proses perataan (averaging) sebanyak dua kali untuk menghilangkan lag yang terjadi pada metode SMA.</p>
                                    <p style='text-align:center;'>Level: $$a_t = 2S'_t - S''_t$$</p>
                                    <p style='text-align:center;'>Trend: $$b_t = \\frac{2}{n-1} (S'_t - S''_t)$$</p>
                                    <p style='text-align:center;'>Forecast: $$F_{t+m} = a_t + b_t m$$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(F_{t+m}\\) = Nilai peramalan untuk \\(m\\) periode ke depan,
                                      \\(a_t\\) = Komponen level, 
                                      \\(b_t\\) = Komponen tren, 
                                      \\(S'_t\\) = SMA pertama, 
                                      \\(S''_t\\) = SMA kedua.
                                    </p>
                                    
                                    <h5><strong>3. Simple Exponential Smoothing (SES)</strong></h5>
                                    <p>Metode ini memberikan bobot yang lebih besar pada observasi yang lebih baru secara eksponensial. Seperti SMA, metode ini ideal untuk data tanpa tren atau musiman.</p>
                                    <p style='text-align:center;'>$$ \\hat{y}_{t+1} = \\alpha y_{t} + (1 - \\alpha)\\hat{y}_{t} $$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(\\hat{y}_{t+1}\\) = Nilai peramalan untuk periode berikutnya, 
                                      \\(y_{t}\\) = Nilai aktual pada periode t,
                                      \\(\\alpha\\) = Parameter pemulusan level (0 ≤ α ≤ 1).
                                    </p>
                                    
                                    <h5><strong>4. Holt's Exponential Smoothing</strong></h5>
                                    <p>Merupakan pengembangan dari SES yang dirancang khusus untuk data yang menunjukkan adanya <strong>pola tren</strong>, tetapi tanpa komponen musiman.</p>
                                    <p style='text-align:center;'>Level: $$\\ell_{t} = \\alpha y_{t} + (1 - \\alpha)(\\ell_{t-1} + b_{t-1})$$</p>
                                    <p style='text-align:center;'>Trend: $$b_{t} = \\beta(\\ell_{t} - \\ell_{t-1}) + (1 - \\beta)b_{t-1}$$</p>
                                    <p style='text-align:center;'>Forecast: $$\\hat{y}_{t+h} = \\ell_{t} + hb_{t}$$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(\\ell_{t}\\) = Estimasi level, 
                                      \\(b_{t}\\) = Estimasi tren,
                                      \\(\\beta\\) = Parameter pemulusan tren (0 ≤ β ≤ 1),
                                      \\(h\\) = Jumlah periode peramalan ke depan.
                                    </p>
                                    
                                    <h5><strong>5. Holt-Winters' Exponential Smoothing</strong></h5>
                                    <p>Merupakan ekstensi lebih lanjut yang mampu menangani data dengan <strong>pola tren dan musiman</strong> secara bersamaan, sehingga menjadi salah satu metode yang paling komprehensif.</p>
                                    <p style='text-align:center;'>Level: $$\\ell_{t} = \\alpha(y_{t} - s_{t-m}) + (1 - \\alpha)(\\ell_{t-1} + b_{t-1})$$</p>
                                    <p style='text-align:center;'>Trend: $$b_{t} = \\beta(\\ell_{t} - \\ell_{t-1}) + (1 - \\beta)b_{t-1}$$</p>
                                    <p style='text-align:center;'>Seasonality: $$s_{t} = \\gamma(y_{t} - \\ell_{t}) + (1 - \\gamma)s_{t-m}$$</p>
                                    <p style='text-align:center;'>Forecast: $$\\hat{y}_{t+h} = \\ell_{t} + hb_{t} + s_{t-m+h}$$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(s_{t}\\) = Komponen musiman, 
                                      \\(\\gamma\\) = Parameter pemulusan musiman (0 ≤ γ ≤ 1),
                                      \\(m\\) = Panjang satu periode musiman.
                                    </p>
                                    <hr>
                                  
                                    <h4 style='text-align:center; font-weight:bold;'>Ukuran Evaluasi Model (Indikator Error)</h4>
                                    <p>Setelah membuat model, performanya dievaluasi menggunakan metrik error yang mengukur selisih antara nilai aktual (\\(y_t\\)) dengan nilai peramalan (\\(\\hat{y}_t\\)). Semakin kecil nilai error, semakin akurat model tersebut.</p>
                                    
                                    <h5><strong>1. Mean Squared Error (MSE)</strong></h5>
                                    <p style='text-align:center;'>$$MSE = \\frac{1}{n} \\sum_{t=1}^{n} (y_t - \\hat{y}_t)^2$$</p>
                                    
                                    <h5><strong>2. Root Mean Squared Error (RMSE)</strong></h5>
                                    <p style='text-align:center;'>$$RMSE = \\sqrt{\\frac{1}{n} \\sum_{t=1}^{n} (y_t - \\hat{y}_t)^2}$$</p>
                                  
                                    <h5><strong>3. Mean Absolute Percentage Error (MAPE)</strong></h5>
                                    <p style='text-align:center;'>$$MAPE = \\frac{1}{n} \\sum_{t=1}^{n} \\left| \\frac{y_t - \\hat{y}_t}{y_t} \\right| \\times 100\\%$$</p>
                                    <p style='font-size: smaller; font-style: italic; margin-left: 20px;'>
                                      <strong>Keterangan:</strong> 
                                      \\(y_t\\) = Nilai aktual pada periode t, 
                                      \\(\\hat{y}_t\\) = Nilai peramalan pada periode t,
                                      \\(n\\) = Jumlah total data.
                                    </p>
                                    <hr>
                                  
                                    <h4 style='text-align:left; font-weight:bold;'>Daftar Pustaka</h4>
                                    <p style='padding-left: 20px;'>
                                      Hyndman, R. J., & Athanasopoulos, G. (2021). <i>Forecasting: Principles and practice</i> (3rd ed.). OTexts. https://otexts.com/fpp3/
                                    </p>
                                    <p style='padding-left: 20px;'>
                                      Makridakis, S., Wheelwright, S. C., & Hyndman, R. J. (1998). <i>Forecasting: Methods and applications</i> (3rd ed.). John Wiley & Sons.
                                    </p>
                                  ")
                                  )
                                )
                        ),
                        
                        # --- Tab Data & Statistik ---
                        tabItem(tabName = "data_and_desk",
                                fluidRow(
                                  box(title = "Input Data", width = 12, solidHeader = TRUE, status = "primary",
                                      fileInput("file", "Langkah 1: Unggah file .csv", accept = ".csv"),
                                      fluidRow(
                                        column(4, radioButtons("separator", "Pemisah Data:", choices = c("Koma (,)" = ",", "Titik Koma (;)" = ";"), selected = ";", inline = TRUE)),
                                        column(4, uiOutput("date_column_ui")), # Opsi memilih kolom tanggal
                                        column(4, selectInput("date_format", "Format Tanggal:", 
                                                              choices = c("dd/mm/yyyy" = "%d/%m/%Y", 
                                                                          "mm/dd/yyyy" = "%m/%d/%Y",
                                                                          "yyyy-mm-dd" = "%Y-%m-%d",
                                                                          "yyyy/mm/dd" = "%Y/%m/%d")))
                                      ),
                                      selectInput("number_format", "Format Angka dalam File:",
                                                  choices = c("1.000,00 (Titik ribuan, Koma desimal)" = "id",
                                                              "1,000.00 (Koma ribuan, Titik desimal)" = "us"))
                                  )
                                ),
                                fluidRow(
                                  tabBox(
                                    title = "Eksplorasi Data", id = "tabset_data", width = 12,
                                    tabPanel("Tabel Data", icon = icon("table"), DTOutput("table")),
                                    tabPanel("Statistik Deskriptif", icon = icon("calculator"),
                                             uiOutput("kolom_checkbox"),
                                             verbatimTextOutput("statistik_output")
                                    ),
                                    tabPanel("Plot & Dekomposisi", icon = icon("chart-area"),
                                             uiOutput("plot_variable_ui"),
                                             uiOutput("plot_frequency_ui"),
                                             hr(),
                                             h4("Plot Data Deret Waktu Asli", align = "center"),
                                             plotOutput("original_plot_output", height = "300px"),
                                             hr(),
                                             h4("Plot Dekomposisi Data", align = "center"),
                                             plotOutput("decomposition_plot_output", height = "500px"),
                                             hr(),
                                             h4("Interpretasi Dekomposisi"),
                                             verbatimTextOutput("decomposition_interpretation_output")
                                    )
                                  )
                                )
                        ),
                        
                        # --- Tab SMA ---
                        tabItem(tabName = "SMA",

                                
                        ),
                        
                        # --- Tab DMA ---
                        tabItem(tabName = "DMA",

                          
                        ),
                        
                        # --- Tab SES, HES, HwES ---
                        tabItem(tabName = "SES",
                               
                                
                        ),
                        tabItem(tabName = "HES",

                                  
                        ),
                        tabItem(tabName = "HwES",

                                
                        )
                      ),
                      div(
                        style = "padding: 20px; text-align: center; color: black; font-weight: bold;",
                        HTML("Copyright &copy; Kelompok 2 Komputasi Statistik Statistika 2023 Universitas Negeri Jakarta")
                      )
                    )
)

# ===================================================================
# BAGIAN 2: SERVER LOGIC (DENGAN PERBAIKAN FINAL)
# ===================================================================
server <- function(input, output, session) {
  
  # --- 1. LOGIKA GLOBAL & PEMROSESAN DATA ---
  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  date_column_name <- reactiveVal(NULL)
  
  # Fungsi untuk menerjemahkan akhiran (k, M, %)
  parse_special_suffix <- function(x) {
    sapply(x, function(val) {
      if (is.na(val) || val == "") return(NA)
      val_lower <- tolower(val)
      if (grepl("%$", val_lower)) return(as.numeric(gsub("%", "", val, fixed = TRUE)) / 100)
      if (grepl("k$", val_lower)) return(as.numeric(gsub("k", "", val_lower, fixed = TRUE)) * 1000)
      if (grepl("m$", val_lower)) return(as.numeric(gsub("m", "", val_lower, fixed = TRUE)) * 1000000)
      return(suppressWarnings(as.numeric(val)))
    }, USE.NAMES = FALSE)
  }
  
  # UI untuk memilih kolom tanggal
  output$date_column_ui <- renderUI({
    df <- raw_data()
    req(df)
    selectInput("date_column", "Pilih Kolom Tanggal:", choices = names(df))
  })
  
  # Membaca data mentah saat file diunggah
  observeEvent(input$file, {
    req(input$file, input$separator)
    tryCatch({
      df <- read.csv(input$file$datapath, sep = input$separator, stringsAsFactors = FALSE, colClasses = "character")
      raw_data(df)
      showNotification("File berhasil diunggah. Silakan pilih kolom tanggal.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error membaca file:", e$message), type = "error", duration = 10)
      raw_data(NULL)
    })
  })
  
  # Memproses dan mengurutkan data setelah kolom tanggal dan format dipilih
  observe({
    df <- raw_data()
    req(df, input$date_column, input$date_format, input$number_format)
    
    tryCatch({
      temp_df <- df
      
      # Konversi kolom tanggal
      temp_df[[input$date_column]] <- as.Date(temp_df[[input$date_column]], format = input$date_format)
      date_column_name(input$date_column)
      
      validate(need(!all(is.na(temp_df[[input$date_column]])), "Format tanggal salah atau kolom tidak berisi tanggal. Silakan periksa kembali."))
      
      # Proses kolom numerik lainnya
      for (col_name in names(temp_df)) {
        if (col_name == input$date_column) next
        
        col_vector <- temp_df[[col_name]]
        if (input$number_format == "id") {
          cleaned_vector <- gsub("\\.", "", col_vector)
          cleaned_vector <- gsub(",", ".", cleaned_vector)
        } else {
          cleaned_vector <- gsub(",", "", col_vector)
        }
        final_numeric_vector <- parse_special_suffix(cleaned_vector)
        
        if (sum(!is.na(final_numeric_vector)) > (length(final_numeric_vector) * 0.5)) {
          temp_df[[col_name]] <- final_numeric_vector
        }
      }
      
      # Urutkan berdasarkan tanggal dan simpan
      temp_df <- temp_df %>%
        filter(!is.na(.data[[input$date_column]])) %>%
        arrange(.data[[input$date_column]])
      
      processed_data(temp_df)
      showNotification("Data berhasil diproses dan diurutkan berdasarkan tanggal.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error memproses data:", e$message), type = "error", duration = 10)
      processed_data(NULL)
    })
  })
  
  
  # Memperbarui pilihan variabel di semua menu peramalan
  observe({
    df <- processed_data()
    if (!is.null(df)) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      if (length(numeric_cols) > 0) {
        updateSelectInput(session, "sma_variable", choices = numeric_cols)
        updateSelectInput(session, "dma_variable", choices = numeric_cols)
        updateSelectInput(session, "ses_variable", choices = numeric_cols)
        updateSelectInput(session, "hes_variable", choices = numeric_cols)
        updateSelectInput(session, "hwes_variable", choices = numeric_cols)
        updateSelectInput(session, "plot_variable", choices = numeric_cols)
      }
    }
  })
  
  # Fungsi untuk mengambil data bersih sebagai objek 'ts'
  get_ts_and_dates <- function(variable_name, frequency = 1) {
    df <- processed_data()
    req(df, variable_name)
    
    clean_df <- df %>%
      select(all_of(c(date_column_name(), variable_name))) %>%
      filter(!is.na(.data[[variable_name]]))
    
    validate(
      need(is.numeric(clean_df[[variable_name]]), paste("ERROR: Variabel '", variable_name, "' bukan numerik.")),
      need(nrow(clean_df) > 3, "ERROR: Data tidak cukup (minimal 4 observasi non-NA).")
    )
    
    ts_object <- ts(clean_df[[variable_name]], frequency = frequency)
    
    return(list(ts_object = ts_object, dates = clean_df[[date_column_name()]]))
  }
  
  
  output$table <- renderDT({
    req(processed_data(), input$date_column, input$date_format)
    display_df <- processed_data()
    # Format kolom tanggal di tabel utama
    display_df[[input$date_column]] <- format(display_df[[input$date_column]], input$date_format)
    datatable(display_df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$kolom_checkbox <- renderUI({
    req(processed_data())
    df <- processed_data()
    choices_cols <- names(df)[sapply(df, function(c) is.numeric(c) || inherits(c, "Date"))]
    validate(need(length(choices_cols) > 0, "Tidak ada kolom numerik atau tanggal untuk diringkas."))
    checkboxGroupInput("pilih_kolom", "Pilih Kolom:", choices = choices_cols, selected = choices_cols)
  })
  
  output$statistik_output <- renderPrint({
    req(processed_data(), input$pilih_kolom)
    print(summary(processed_data()[, input$pilih_kolom, drop = FALSE]))
  })
  
  # --- BAGIAN PLOT & DEKOMPOSISI ---
  output$plot_variable_ui <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    selectInput("plot_variable", "Pilih Variabel untuk di Plot:", choices = numeric_cols)
  })
  
  output$plot_frequency_ui <- renderUI({
    req(processed_data(), input$plot_variable)
    clean_data <- na.omit(processed_data()[[input$plot_variable]])
    max_freq <- floor(length(clean_data) / 2)
    if (max_freq < 2) return(helpText("Data tidak cukup panjang untuk dekomposisi."))
    tagList(
      sliderInput("plot_freq", "Frekuensi Data (Panjang Musim):", min = 2, max = max_freq, value = min(12, max_freq), step = 1),
      selectInput("decomp_type", "Tipe Dekomposisi:", choices = c("additive", "multiplicative"))
    )
  })
  
  ts_to_plot_data <- reactive({
    req(input$plot_freq)
    data_list <- get_ts_and_dates(input$plot_variable, frequency = input$plot_freq)
    validate(need(length(data_list$ts_object) >= 2 * input$plot_freq, paste("Data tidak cukup. Untuk frekuensi", input$plot_freq, "dibutuhkan minimal", 2 * input$plot_freq, "observasi.")))
    return(data_list)
  })
  
  output$original_plot_output <- renderPlot({
    data_list <- ts_to_plot_data()
    req(data_list)
    df_plot <- data.frame(Tanggal = data_list$dates, Nilai = as.numeric(data_list$ts_object))
    ggplot(df_plot, aes(x = Tanggal, y = Nilai)) +
      geom_line(color = "royalblue") +
      labs(title = paste("Plot Deret Waktu untuk", input$plot_variable), x = "Tanggal", y = "Nilai") +
      theme_minimal()
  })
  
  decomposed_results <- reactive({
    data_list <- ts_to_plot_data()
    req(data_list)
    decompose(data_list$ts_object, type = input$decomp_type)
  })
  
  output$decomposition_plot_output <- renderPlot({
    req(decomposed_results())
    autoplot(decomposed_results()) +
      labs(title = paste("Dekomposisi", input$decomp_type, "untuk", input$plot_variable)) +
      theme_minimal()
  })
  
  output$decomposition_interpretation_output <- renderText({
    req(decomposed_results())
    d <- decomposed_results()
    var_random <- var(d$random, na.rm=TRUE)
    if(is.na(var_random) || var_random == 0) return("Tidak dapat menghitung kekuatan komponen karena varians acak nol atau NA.")
    Ft <- max(0, 1 - var_random / var(d$trend + d$random, na.rm=TRUE))
    Fs <- max(0, 1 - var_random / var(d$seasonal + d$random, na.rm=TRUE))
    
    paste(
      "--- Apa Arti Grafik Ini? ---\n\n",
      "Grafik ini membedah data Anda menjadi 3 bagian utama agar lebih mudah dimengerti:\n\n",
      "1. Tren (Trend):\n",
      "   - Ini adalah arah umum data Anda dalam jangka panjang. Apakah cenderung naik, turun, atau datar? Garis ini menunjukkannya.\n",
      "   - Kekuatan Tren: ", paste0(round(Ft * 100, 1), "%"), " -> Seberapa kuat pengaruh tren ini dalam data Anda. Semakin tinggi persentasenya, semakin jelas arah pergerakan data.\n\n",
      "2. Musiman (Seasonal):\n",
      "   - Ini adalah pola yang berulang secara teratur. Misalnya, penjualan selalu naik di akhir tahun atau traffic selalu ramai di akhir pekan. Grafik ini menangkap siklus tersebut.\n",
      "   - Kekuatan Musiman: ", paste0(round(Fs * 100, 1), "%"), " -> Seberapa konsisten pola musiman ini. Persentase tinggi berarti polanya sangat bisa diprediksi.\n\n",
      "3. Acak (Random):\n",
      "   - Ini adalah 'sisa' atau 'noise' setelah tren dan pola musiman dihilangkan. Idealnya, bagian ini terlihat acak total tanpa pola yang jelas. Jika ada pola di sini, mungkin ada faktor lain yang memengaruhi data Anda yang belum teridentifikasi."
    )
  })
  
  # --- FUNGSI HELPER UNTUK TABEL DAN INTERPRETASI ---
  generate_details_text <- function(results, input_params = NULL) {
    forecast_obj <- results$forecast
    accuracy_df <- results$accuracy
    
    # --- Basic Info ---
    details <- paste(
      "=====================================",
      "        DETAIL ANALISIS MODEL        ",
      "=====================================\n\n",
      "Banyak Data Digunakan:", length(forecast_obj$x), "observasi\n",
      "Metode Peramalan:", forecast_obj$method, "\n"
    )
    
    # --- Parameters Interpretation ---
    params_text <- "\n--- Pengaturan Model ---\n"
    if (!is.null(forecast_obj$model)) { # For SES, HES, HwES
      params <- round(forecast_obj$model$par, 4)
      if ("alpha" %in% names(params)) params_text <- paste(params_text, "Alpha (Level):", params["alpha"], "\n   (Menentukan seberapa besar pengaruh data terbaru. Semakin mendekati 1, model makin 'percaya' data baru. Mendekati 0, model lebih 'percaya' histori.)\n")
      if ("beta" %in% names(params)) params_text <- paste(params_text, "Beta (Tren):", params["beta"], "\n   (Menentukan seberapa cepat model mendeteksi perubahan arah tren [naik/turun]. Semakin tinggi, model makin fleksibel mengikuti perubahan tren.)\n")
      if ("gamma" %in% names(params)) params_text <- paste(params_text, "Gamma (Musiman):", params["gamma"], "\n   (Menentukan seberapa cepat model beradaptasi jika ada pergeseran pola musiman.)\n")
    } else if (!is.null(input_params)) { # For SMA, DMA
      if ("n_order" %in% names(input_params)) params_text <- paste(params_text, "Orde (n):", input_params$n_order, "\n   (Model melihat data dari", input_params$n_order, "periode terakhir untuk menebak nilai selanjutnya. Angka besar membuat ramalan lebih stabil, angka kecil membuat lebih responsif.)\n")
    } else {
      params_text <- "Parameter ditentukan secara otomatis oleh sistem.\n"
    }
    
    # --- Accuracy Interpretation ---
    accuracy_title <- "\n=====================================\n"
    accuracy_title <- paste(accuracy_title, "      HASIL EVALUASI MODEL         \n")
    accuracy_title <- paste(accuracy_title, "=====================================\n")
    
    accuracy_body <- capture.output(print(accuracy_df))
    accuracy_text <- paste(accuracy_body, collapse = "\n")
    
    accuracy_interp_text <- "\n--- Penjelasan Metrik Evaluasi ---\n"
    if(!is.na(accuracy_df[,"ME"])) accuracy_interp_text <- paste(accuracy_interp_text, "* ME (Mean Error):", round(accuracy_df[,"ME"], 4), "\n   Apakah model cenderung meramal terlalu tinggi (negatif) atau terlalu rendah (positif)? Idealnya mendekati nol.\n")
    if(!is.na(accuracy_df[,"RMSE"])) accuracy_interp_text <- paste(accuracy_interp_text, "* RMSE (Root Mean Squared Error):", round(accuracy_df[,"RMSE"], 4), "\n   Rata-rata seberapa jauh ramalan meleset dari kenyataan (dalam satuan asli). Semakin kecil angkanya, semakin hebat modelnya.\n")
    if(!is.na(accuracy_df[,"MAE"])) accuracy_interp_text <- paste(accuracy_interp_text, "* MAE (Mean Absolute Error):", round(accuracy_df[,"MAE"], 4), "\n   Sama seperti RMSE, tapi lebih kebal terhadap nilai ekstrem (outlier).\n")
    if(!is.na(accuracy_df[,"MAPE"])) {
      mape_value <- accuracy_df[1, "MAPE"]
      mape_interp <- if (is.na(mape_value)) {
        "* MAPE (Mean Absolute Percentage Error): Tidak tersedia."
      } else {
        paste0("* MAPE (Mean Absolute Percentage Error): ", round(mape_value, 2), "%\n",
               "   Rata-rata, ramalan meleset berapa persen dari nilai asli? Ini adalah ukuran akurasi yang paling populer.\n",
               "   - Di bawah 10%: Luar Biasa\n",
               "   - 10% - 20%: Baik\n",
               "   - 20% - 50%: Cukup\n",
               "   - Di atas 50%: Kurang Baik")
      }
      accuracy_interp_text <- paste(accuracy_interp_text, mape_interp)
    }
    
    # Combine all parts
    final_text <- paste(details, params_text, accuracy_title, accuracy_text, accuracy_interp_text)
    return(final_text)
  }
  
  create_comparison_table <- function(result, dates) {
    req(input$date_format)
    
    combined_ts <- ts.union(
      Data_Aktual = result$x,
      Nilai_Fitted = result$fitted,
      Residuals = result$residuals
    )
    
    df <- as.data.frame(combined_ts)
    
    if(nrow(df) == length(dates)) {
      df <- cbind(Tanggal = format(dates, input$date_format), df)
    } else {
      warning("Panjang tanggal dan data tidak cocok untuk tabel perbandingan.")
    }
    
    datatable(df, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, rownames = FALSE), caption = "Tabel Perbandingan Nilai Aktual vs. Fitted/Hasil Model") %>%
      formatRound(c('Data_Aktual', 'Nilai_Fitted', 'Residuals'), 4)
  }
  
  detect_interval <- function(dates) {
    if (length(dates) < 2) return("day")
    diffs <- as.numeric(diff(dates))
    median_diff <- median(diffs, na.rm=TRUE)
    
    if (median_diff > 350) return("year")
    if (median_diff > 85) return("quarter")
    if (median_diff > 25) return("month")
    if (median_diff > 5) return("week")
    return("day")
  }
  
  create_forecast_table <- function(result, all_dates, h) {
    req(input$date_format)
    
    last_date <- tail(all_dates, 1)
    interval <- detect_interval(all_dates)
    
    forecast_dates <- seq.Date(from = last_date, by = interval, length.out = h + 1)[-1]
    
    df <- data.frame(
      Tanggal_Ramalan = format(forecast_dates, input$date_format),
      Nilai_Ramalan = as.numeric(result$mean)
    )
    datatable(df, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE), rownames = FALSE, caption = "Tabel Hasil Ramalan untuk Periode Mendatang") %>%
      formatRound('Nilai_Ramalan', 4)
  }
  
  # --- BAGIAN PERAMALAN ---
  # SMA

  
  # DMA

  
  # SES

  
  # HES

  
  # HwES

  
}

# ===================================================================
# BAGIAN 3: MENJALANKAN APLIKASI
# ===================================================================
shinyApp(ui = ui, server = server)
