## title : Create Star Schema
## subtitle : CS 5200 / Practicum II
## author : Thomas Yi
## date : Fall 2024

# Connection details for Aiven MySQL Database

db_user <- 'avnadmin' 
db_password <- //HIDDEN - ASK FOR PASSWORD
db_name <- 'defaultdb'
db_host <- 'mysql-1f4ba127-cs5200-dbms-practicum1.k.aivencloud.com' 
db_port <- 21111

# Establish the connection
aivendb <- dbConnect(RMySQL::MySQL(),
                     user = db_user,
                     password = db_password,
                     dbname = db_name,
                     host = db_host,
                     port = db_port)

# Check the connection status
if (!is.null(aivendb)) {
  print("Successfully connected to the Aiven MySQL database using RMySQL.")
} else {
  print("Failed to connect to the Aiven MySQL database using RMySQL.")
}


print(dbListTables(aivendb))

create_product_fact <- function(db) {
  
  ##creating a country dimension table
  dbExecute(aivendb, "DROP TABLE IF EXISTS country_dimension")
  dbExecute(aivendb, "
            CREATE TABLE IF NOT EXISTS country_dimension (
            country_id INT PRIMARY KEY,
            country_name VARCHAR(255),
            region VARCHAR(255));
            ")
  
  ##creating the fact table now
  dbExecute(aivendb, "DROP TABLE IF EXISTS product_facts")
  
  # Create fact tables in MySQL
  dbExecute(aivendb, "
  CREATE TABLE IF NOT EXISTS product_facts (
    product_id INT,
    product_name VARCHAR(255),
    country_id INT, 
    month VARCHAR(2),
    year VARCHAR(4),
    total_revenue DECIMAL(10, 2),
    total_units_sold INT,
    PRIMARY KEY (product_id, country_id, month, year),
    FOREIGN KEY (country_id) REFERENCES country_dimension(country_id)

  );
")
  
  
  
  
}

create_product_fact(aivendb)




################################


#a function that includes a SQL select statement to extract only the data that 
#is needed. 
get_product_facts <- function(db) {
  query <- "
    SELECT
      p.product_id,
      p.product_name,
      cl.country_id,
      strftime('%m', s.txn_date) AS month,
      strftime('%Y', s.txn_date) AS year,
      SUM(s.total_cost) AS total_revenue,
      SUM(s.quantity) AS total_units_sold
    FROM (
      SELECT * FROM sales_2020
      UNION ALL
      SELECT * FROM sales_2021
      UNION ALL
      SELECT * FROM sales_2022
      UNION ALL
      SELECT * FROM sales_2023
    ) s
    JOIN customers c ON s.customer_id = c.customer_id
    JOIN country_lookup cl ON c.country_id = cl.country_id
    JOIN products p ON s.product_id = p.product_id
    GROUP BY p.product_id, cl.country_id, year, month
  "
  dbGetQuery(db, query)
}



# insert_into_pf <- function(db) {
#   pf_df <- get_product_facts(dbcon)
#   
#   for (i in seq_len(nrow(pf_df))) {
#     
#     
#     print(paste0(pf_df$product_id[i], ", '" ,
#                  pf_df$product_name[i], "', '" ,
#                  pf_df$country[i], "', '" ,
#                  pf_df$month[i], "', '" , 
#                  pf_df$year[i], "', " ,
#                  pf_df$total_revenue[i], ", " ,
#                  pf_df$total_units_sold[i]))
#    
#     
#     
#     dbExecute(db, paste0("INSERT INTO product_facts (
#     product_id,
#     product_name ,
#     country ,
#     month,
#     year,
#     total_revenue,
#     total_units_sold) VALUES (" , 
#                          pf_df$product_id[i], ", '" ,
#                          pf_df$product_name[i], "', '" ,
#                          pf_df$country[i], "', '" ,
#                          pf_df$month[i], "', '" , 
#                          pf_df$year[i], "', " ,
#                          pf_df$total_revenue[i], ", " ,
#                          pf_df$total_units_sold[i], ")"
#                          ))
#     
#     
#   }
#   
# }




insert_into_product_facts <- function(db) {
  # Fetch product facts data
  product_facts <- get_product_facts(dbcon)
  
  # Define batch size
  batch_size <- 100
  
  # Insert data in batches
  for (start_row in seq(1, nrow(product_facts), by = batch_size)) {
    end_row <- min(start_row + batch_size - 1, nrow(product_facts))
    batch <- product_facts[start_row:end_row, ]
    
    # Construct value strings for batch insertion
    value_strings <- apply(batch, 1, function(row) {
      paste0(
        "(", 
        row["product_id"], ", '",
        row["product_name"], "', ",
        row["country_id"], ", ",
        row["month"], ", ",
        row["year"], ", ",
        row["total_revenue"], ", ",
        row["total_units_sold"], ")"
      )
    })
    
    # Combine all value strings into a single INSERT query
    values_clause <- paste(value_strings, collapse = ", ")
    query <- paste0("
      INSERT INTO product_facts (
        product_id,
        product_name,
        country_id,
        month,
        year,
        total_revenue,
        total_units_sold
      ) VALUES ", values_clause)
    
    # Execute query with error handling
    tryCatch({
      dbExecute(db, query)
      message(paste("Successfully inserted rows", start_row, "to", end_row))
    }, error = function(e) {
      message(paste("Error inserting rows", start_row, "to", end_row, ":", e$message))
    })
  }
  
  print("All data inserted into product_facts table.")
}

# Load data into product_facts table
insert_into_product_facts(aivendb)

# Verify the inserted data
print(dbGetQuery(aivendb, "SELECT * FROM product_facts"))


dbListTables(aivendb)
dbGetQuery(aivendb, "DESCRIBE product_facts")





####################


#creating the rep_facts table
create_rep_fact <- function(db) {
  dbExecute(db, "DROP TABLE IF EXISTS rep_facts")
  
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS rep_facts (
      rep_id INT,
      rep_name VARCHAR(255),
      quarter VARCHAR(2),
      year VARCHAR(4),
      total_revenue DECIMAL(10, 2),
      avg_amount_per_sale DECIMAL(10, 2),
      PRIMARY KEY (rep_id, quarter, year)
    );
  ")
}

create_rep_fact(aivendb)


get_rep_facts <- function(db) {
  query <- "
    SELECT
      r.rep_id,
      CONCAT(r.first_name, ' ', r.last_name) AS rep_name,
      CASE
        WHEN CAST(strftime('%m', s.txn_date) AS INT) BETWEEN 1 AND 3 THEN 'Q1'
        WHEN CAST(strftime('%m', s.txn_date) AS INT) BETWEEN 4 AND 6 THEN 'Q2'
        WHEN CAST(strftime('%m', s.txn_date) AS INT) BETWEEN 7 AND 9 THEN 'Q3'
        WHEN CAST(strftime('%m', s.txn_date) AS INT) BETWEEN 10 AND 12 THEN 'Q4'
      END AS quarter,
      strftime('%Y', s.txn_date) AS year,
      SUM(s.total_cost) AS total_revenue,
      AVG(s.total_cost) AS avg_amount_per_sale
    FROM (
      SELECT * FROM sales_2020
      UNION ALL
      SELECT * FROM sales_2021
      UNION ALL
      SELECT * FROM sales_2022
      UNION ALL
      SELECT * FROM sales_2023
    ) s
    JOIN reps r ON s.rep_id = r.rep_id
    GROUP BY r.rep_id, quarter, year
  "
  dbGetQuery(db, query)
}



insert_into_rep_facts <- function(db) {
  # Fetch rep facts data
  rep_facts <- get_rep_facts(dbcon)
  
  # Define batch size
  batch_size <- 100
  
  # Insert data in batches
  for (start_row in seq(1, nrow(rep_facts), by = batch_size)) {
    end_row <- min(start_row + batch_size - 1, nrow(rep_facts))
    batch <- rep_facts[start_row:end_row, ]
    
    # Construct value strings for batch insertion
    value_strings <- apply(batch, 1, function(row) {
      paste0(
        "(", 
        row["rep_id"], ", '",
        row["rep_name"], "', '",
        row["quarter"], "', ",
        row["year"], ", ",
        row["total_revenue"], ", ",
        row["avg_amount_per_sale"], ")"
      )
    })
    
    # Combine all value strings into a single INSERT query
    values_clause <- paste(value_strings, collapse = ", ")
    query <- paste0("
      INSERT INTO rep_facts (
        rep_id,
        rep_name,
        quarter,
        year,
        total_revenue,
        avg_amount_per_sale
      ) VALUES ", values_clause)
    
    # Execute query with error handling
    tryCatch({
      dbExecute(db, query)
      message(paste("Successfully inserted rows", start_row, "to", end_row))
    }, error = function(e) {
      message(paste("Error inserting rows", start_row, "to", end_row, ":", e$message))
    })
  }
  
  print("All data inserted into rep_facts table.")
}

insert_into_rep_facts(aivendb)

#checking information inside this new rep_facts table
print(dbGetQuery(aivendb, "SELECT * FROM rep_facts LIMIT 10"))

print(dbGetQuery(aivendb, "SELECT * FROM product_facts"))

#disconnecting from the database
dbDisconnect(aivendb)

