

# 0) Load libraries -----
library(DBI)
library(RPostgres)  # make sure this is installed
library(rstudioapi) # for secure password input
library(arrow) # Parquet files 

# Function to run query and save results
runquery <- function(wrds_connection, query, path) {
  # Run query  
  df <- dbGetQuery(wrds_connection, query)
  
  # Save to Parquet file (compact, fast)
  write_parquet(df, path)
}


# 1) Connect to server ------

# Download financial data from WRDS 
# Connect to WRDS PostgreSQL database
wrds_connection <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = "tschlanger",
                  password = rstudioapi::askForPassword("WRDS Password"), # Secure password input
                  dbname = "wrds",
                  sslmode = "require", 
                  connect_timeout = 80  # Increase connection timeout (in seconds)
                  )


# 2) Run queries ------

## a) Compustat annual firm-level data ------
  
  #' COMPUSTAST ANNUAL FUNDAMENTAL DATA
  #' Variables
  #' gvkey: Global company key
  #' sic: Standard Industrial Classification code
  #' datadate: Date of the financial data
  #' fyear: Fiscal year
  #' revt: Revenue
  #' prcc_f: Price per share fiscal year 
  #' csho : Common shares outstanding 
  #' dvpsp_f: Div per share fiscal year
  #' Stock split adjustment ratio 
  #' Stock delisting 
  #' mkvalt: Market value
  
  # Construct query as a string: 
  query <- "
    SELECT f.gvkey, f.datadate, f.fyear, f.sale, f.cogs, 
      f.invt, f.ppegt, f.emp, f.xrd, f.at, f.mkvalt, f.ceq, f.prcc_c, f.csho, 
       c.sic
    FROM comp.funda AS f
    LEFT JOIN comp.company AS c
    ON f.gvkey = c.gvkey
    WHERE f.indfmt = 'INDL'
      AND f.datafmt = 'STD'
      AND f.popsrc = 'D'
      AND f.consol = 'C'
      AND f.fyear >= 2024
  "
  
  # Run query and disconnect from WRDS
  runquery(wrds_connection, query, here("data/wrds/comp_funda.parquet"))
  dbDisconnect(wrds_connection)
  
## b) Compustat firm age -------
  query_age <- "
    SELECT gvkey, fyear, 
    fyear - MIN(fyear) OVER (PARTITION BY gvkey) AS firm_age
    FROM comp.funda
    WHERE indfmt = 'INDL' AND datafmt = 'STD' AND popsrc = 'D' AND consol = 'C'
    "
  # Run query and disconnect from WRDS
  runquery(wrds_connection, query_age, here("data/wrds/comp_funda_age.parquet"))
  dbDisconnect(wrds_connection)

  
## c) CRSP-COMPUSTAT merged data  ------

  query_ccm <- "
    SELECT gvkey, linkprim, linktype, lpermno, lpermco, linkdt, linkenddt
    FROM crsp.ccmxpf_linktable
    "
  # Run query and disconnect from WRDS
  runquery(wrds_connection, query_ccm, here("data/wrds/comp_crsp_link.parquet"))
  dbDisconnect(wrds_connection)
  
## CRSP monthly return data ###########

  query_crsp <- "
    SELECT
        CALDT,       -- Calendar Date (Year-End)
        vwretd,      -- Value-Weighted Return (includes distributions)
        totcnt,      -- Total Market Count 
        sprtrn,      -- Return on S&P Composite Index
        usdcnt       -- Count of Securities Used
    FROM
        crsp.msia
    WHERE
        CALDT >= '2000-01-01'  -- Start year for CRSP index data
    ORDER BY
        CALDT;
    "
  runquery(wrds_connection, query_crsp, here("data/wrds/crsp_monthly_market.parquet"))
  dbDisconnect(wrds_connection)
  
  
