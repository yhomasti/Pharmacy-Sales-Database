## title : Create Analytics Database
## subtitle : CS 5200 / Practicum II
## author : Thomas Yi
## date : Fall 2024

# Loading necessary packages
install_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Installing required packages
nec_packages <- c("DBI", "RSQLite", "sqldf")
for (pkg in nec_packages) {
  install_package(pkg)
}

# Function to drop all existing tables
drop_existing <- function(db) {
  dbExecute(db, "DROP TABLE IF EXISTS customers")
  dbExecute(db, "DROP TABLE IF EXISTS products")
  dbExecute(db, "DROP TABLE IF EXISTS country_lookup")
  dbExecute(db, "DROP TABLE IF EXISTS reps")
  dbExecute(db, "DROP TABLE IF EXISTS sales")
}

# Function to create tables
create_schema <- function(db) {
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS products (
      product_id INTEGER PRIMARY KEY,
      product_name TEXT NOT NULL,
      unit_cost REAL NOT NULL
    );
  ")
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS reps (
      rep_id INTEGER PRIMARY KEY,
      first_name TEXT,
      last_name TEXT,
      region TEXT,
      phone TEXT,
      commission_rate REAL,
      hire_date TEXT
    );
  ")
  
  #creating a lookup table for country
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS country_lookup (
            country_id INTEGER PRIMARY KEY,
            country_name TEXT UNIQUE NOT NULL
            );")
  
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS customers (
      customer_id INTEGER PRIMARY KEY,
      customer_name TEXT NOT NULL,
      country_id INT,
      FOREIGN KEY (country_id) REFERENCES country_lookup(country_id)
    );
  ")
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS sales (
      sale_id INTEGER NOT NULL,
      txn_date TEXT,
      product_id INTEGER NOT NULL,
      rep_id INTEGER NOT NULL,
      customer_id INTEGER,
      quantity INTEGER NOT NULL,
      total_cost REAL NOT NULL,
      file_name TEXT NOT NULL,
      PRIMARY KEY(sale_id, file_name)
      FOREIGN KEY (product_id) REFERENCES products(product_id),
      FOREIGN KEY (rep_id) REFERENCES reps(rep_id),
      FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
    );
  ")
}

# Connecting to SQLite database and creating tables
unlink("analytics.db")
dbcon <- dbConnect(SQLite(), "analytics.db")
drop_existing(dbcon)
create_schema(dbcon)

print(getwd()) # Check your working directory
print(file.exists("analytics.db")) # Ensure the file exists


#a list of all the csv files with customer/sale information
csv_files <- list("csv-data/pharmaSalesTxn-100.csv",
                  "csv-data/pharmaSalesTxn-500.csv",
                  "csv-data/pharmaSalesTxn-900.csv",
                  "csv-data/pharmaSalesTxn-2000.csv",
                  "csv-data/pharmaSalesTxn-3000.csv",
                  "csv-data/pharmaSalesTxn-5000.csv",
                  "csv-data/pharmaSalesTxn-6000.csv",
                  "csv-data/pharmaSalesTxn-8000.csv")

#combining all those csv files into one large dataframe 
combined_data <- do.call(rbind, lapply(csv_files, read.csv, 
                                       stringsAsFactors = FALSE))



# Function to load data into the products table
load_products <- function(db) {
  sales_data <- combined_data
  sales_data$unitcost <- ifelse(is.na(sales_data$unitcost) | sales_data$unitcost 
                                == "", 0.0, sales_data$unitcost)
  
  products <- sqldf("SELECT DISTINCT prod AS product_name, 
                    unitcost AS unit_cost 
                    FROM sales_data")
  products$product_id <- seq(1, nrow(products))  
  
  dbBegin(db)
  for (i in seq_len(nrow(products))) {
    dbExecute(db, "
      INSERT INTO products (product_id, product_name, unit_cost)
      VALUES (?, ?, ?)
    ", params = list(products$product_id[i], 
                     products$product_name[i], 
                     products$unit_cost[i]))
  }
  dbCommit(db)
  print("Products table contents:")
  print(dbGetQuery(db, "SELECT * FROM products"))
}

# Function to load data into the `reps` table with properly formatted hire dates
load_reps <- function(db, file) {
  
  # Read the CSV file into a data frame
  reps_data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Parse the `repHireDate` column and convert it to `YYYY-MM-DD` format
  reps_data$repHireDate <- format(as.Date(reps_data$repHireDate, 
                                          format = "%b %d %Y"), 
                                  "%m-%d-%Y")
  
  # Insert the formatted data into the `reps` table
  dbBegin(db)
  for (i in seq_len(nrow(reps_data))) {
    dbExecute(db, "
      INSERT INTO reps (rep_id, first_name, last_name, 
      region, phone, commission_rate, hire_date)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      reps_data$repID[i],
      reps_data$repFN[i],
      reps_data$repLN[i],
      reps_data$repTR[i],
      reps_data$repPh[i],
      reps_data$repCm[i],
      #double check the formating of the hire date
      reps_data$repHireDate[i] 
    ))
  }
  dbCommit(db)
  
  # Verify insertion
  print("Reps table contents:")
  print(dbGetQuery(db, "SELECT * FROM reps"))
}


#a function to load data into the country_lookup table
load_countries <- function(db) {
  # Extract distinct countries from the combined_data dataframe
  country_df <- data.frame(
    country_name = unique(combined_data$country),
    stringsAsFactors = FALSE
  )
  
  # Assign unique country_id values
  country_df$country_id <- seq_len(nrow(country_df))
  
  # Insert data into the country_lookup table
  dbBegin(db)
  for (i in seq_len(nrow(country_df))) {
    dbExecute(db, "
      INSERT INTO country_lookup (country_id, country_name)
      VALUES (?, ?)
    ", params = list(country_df$country_id[i], country_df$country_name[i]))
  }
  dbCommit(db)
  
  print("Country lookup table populated successfully.")
  print(dbGetQuery(db, "SELECT * FROM country_lookup"))
}






load_customers <- function(db) {
  #extracting distinct customers and their countries from combined_data
  customer_df <- data.frame(
    customer_name = unique(combined_data$cust),
    country_name = combined_data$country[match(unique(combined_data$cust), 
                                               combined_data$cust)],
    stringsAsFactors = FALSE
  )
  
  #retrieve all data from country_lookup table from the database and load it 
  #into a dataframe
  country_lookup <- dbGetQuery(db, "SELECT * FROM country_lookup")
  
  #map country names to country IDs
  customer_df <- merge(customer_df, country_lookup, by.x = "country_name", by.y 
                       = "country_name")
  
  #assign unique customer IDs
  customer_df$customer_id <- seq_len(nrow(customer_df))
  
  #insert data into the customers table
  dbBegin(db)
  for (i in seq_len(nrow(customer_df))) {
    dbExecute(db, "
      INSERT INTO customers (customer_id, customer_name, country_id)
      VALUES (?, ?, ?)
    ", params = list(
      customer_df$customer_id[i],
      customer_df$customer_name[i],
      customer_df$country_id[i]
    ))
  }
  dbCommit(db)
  
  print("Customer table populated successfully.")
  print(dbGetQuery(db, "SELECT * FROM customers"))
  
}
  



#function to load data into the sales table
load_sales <- function(db, file) {
  #load CSV data into a data frame
  sales_data <- read.csv(file, stringsAsFactors = FALSE)
  
  sales_data$txn_date <- as.Date(sales_data$date, format = "%m/%d/%Y")
  
  # Verify that dates are correctly converted :')
  if (any(is.na(sales_data$txn_date))) {
    stop("check the date in teh csv file because something is wrong :(")
  }
  
  # Retrieve existing mappings from the database
  products <- dbGetQuery(db, "SELECT product_id, product_name FROM products")
  customers <- dbGetQuery(db, "SELECT customer_id, 
                          customer_name FROM customers")
  reps <- dbGetQuery(db, "SELECT rep_id FROM reps")
  
  dbBegin(db)
  for (i in seq_len(nrow(sales_data))) {
    # Map product_id, customer_id, and rep_id
    product_id <- products$product_id[products$product_name 
                                      == sales_data$prod[i]]
    customer_id <- customers$customer_id[customers$customer_name 
                                         == sales_data$cust[i]]
    rep_id <- sales_data$repID[i]
    
    # Calculate the total cost for the sale
    total_cost <- sales_data$qty[i] * sales_data$unitcost[i]
    
    # Insert the data into the `sales` table
    dbExecute(db, "
      INSERT INTO sales (sale_id, txn_date, product_id, rep_id, 
      customer_id, quantity, total_cost, file_name)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      sales_data$txnID[i],
      format(sales_data$txn_date[i], "%Y-%m-%d"), 
      product_id,
      rep_id,
      customer_id,
      sales_data$qty[i],
      total_cost,
      file
    ))
    
  }
  dbCommit(db)
  
 
}

 
#calling the functions and loading data into the tables
load_reps(dbcon, "csv-data/pharmaReps.csv")

load_products(dbcon)

load_countries(dbcon)

load_customers(dbcon)

for (file in csv_files) {
  load_sales(dbcon, file)
}



# Verify the insertion
print("Sales table contents:")
print(dbGetQuery(dbcon, "SELECT * FROM sales LIMIT 10"))




# Function to partition the sales table by year and remove the original table
partition_sales_by_year <- function(db) {
  #extracting unique years from the txn_date attribute in the sales table
  years <- dbGetQuery(db, "
    SELECT DISTINCT strftime('%Y', txn_date) AS year
    FROM sales
  ")
  
  #year and populate them
  for (year in years$year) {
    table_name <- paste0("sales_", year)
    
    # Drop the partition table if it already exists
    dbExecute(db, paste0("DROP TABLE IF EXISTS ", table_name))
    print(paste0("Dropped existing table: ", table_name))
    
    # Create a partition table for the specific year
    create_table_sql <- paste0("
      CREATE TABLE ", table_name, " AS
      SELECT *
      FROM sales
      WHERE strftime('%Y', txn_date) = '", year, "';")
    
    # Execute the SQL to create the partition table
    dbExecute(db, create_table_sql)
    print(paste0("Created partition table: ", table_name))
  }
  
  # Step 3: Drop the original `sales` table
  dbExecute(db, "DROP TABLE IF EXISTS sales")
  print("Original sales table removed after partitioning.")
  
  # Verify partitioning
  print("Partitioned tables created:")
  print(dbListTables(db))
}

partition_sales_by_year(dbcon)

#verify data in the new partitioned tables
print("data from the new sales_2020 table: ")
print(dbGetQuery(dbcon, "SELECT * FROM sales_2020 LIMIT 10"))
print("data from the new sales_2021 table: ")
print(dbGetQuery(dbcon, "SELECT * FROM sales_2021 LIMIT 10"))
print("data from the new sales_2022 table: ")
print(dbGetQuery(dbcon, "SELECT * FROM sales_2022 LIMIT 10"))
print("data from the new sales_2023 table: ")
print(dbGetQuery(dbcon, "SELECT * FROM sales_2023 LIMIT 10"))


