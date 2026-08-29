-- Business questions answered in SQL against the cleaned invoice-line table.
-- Each query is named with a "-- name:" header; R/functions.R::read_queries()
-- parses this file and run_queries() executes every query against SQLite.
--
-- Table: retail_lines(invoice_no, stock_code, description, quantity,
--                     unit_price, customer_id, country, invoice_ts, revenue)
-- invoice_ts is ISO-8601 text, so strftime() works directly.

-- name: monthly_summary
-- identified_customers, not "customers": COUNT(DISTINCT customer_id) skips
-- NULLs, and about a quarter of lines are guest checkouts with no id - an
-- unqualified column name would silently overstate what was counted.
SELECT
    strftime('%Y-%m', invoice_ts)                        AS month,
    ROUND(SUM(revenue), 2)                               AS revenue,
    COUNT(DISTINCT invoice_no)                           AS invoices,
    COUNT(DISTINCT customer_id)                          AS identified_customers,
    ROUND(SUM(revenue) / COUNT(DISTINCT invoice_no), 2)  AS revenue_per_invoice
FROM retail_lines
GROUP BY month
ORDER BY month;

-- name: monthly_growth
-- Month-over-month growth via a window function. trading_days travels with
-- the growth figure so a consumer of the standalone CSV can see that the
-- last month is partial (the data stops mid-month) instead of reading its
-- steep "decline" as real.
WITH monthly AS (
    SELECT strftime('%Y-%m', invoice_ts)     AS month,
           SUM(revenue)                      AS revenue,
           COUNT(DISTINCT DATE(invoice_ts))  AS trading_days
    FROM retail_lines
    GROUP BY month
)
SELECT
    month,
    ROUND(revenue, 2) AS revenue,
    trading_days,
    ROUND(
        100.0 * (revenue - LAG(revenue) OVER (ORDER BY month))
              / LAG(revenue) OVER (ORDER BY month),
        1
    ) AS mom_growth_pct
FROM monthly
ORDER BY month;

-- name: top_products
-- UPPER(stock_code): a handful of codes appear in both cases ('85123a' and
-- '85123A'); a case-sensitive GROUP BY would split one product into two rows
-- and understate its totals. MAX(description): a code's description varies in
-- casing or punctuation between lines; any single representative label will do.
SELECT
    UPPER(stock_code)        AS stock_code,
    MAX(description)         AS description,
    ROUND(SUM(revenue), 2)   AS revenue,
    CAST(SUM(quantity) AS INTEGER) AS units,
    COUNT(DISTINCT invoice_no)     AS invoices
FROM retail_lines
GROUP BY UPPER(stock_code)
ORDER BY revenue DESC
LIMIT 10;

-- name: country_summary
SELECT
    country,
    ROUND(SUM(revenue), 2)                          AS revenue,
    COUNT(DISTINCT customer_id)                     AS identified_customers,
    ROUND(100.0 * SUM(revenue)
          / (SELECT SUM(revenue) FROM retail_lines), 1) AS revenue_share_pct
FROM retail_lines
GROUP BY country
ORDER BY revenue DESC
LIMIT 10;

-- name: customer_repeat
-- Revenue concentration in repeat buyers. Lines without a customer_id (guest
-- checkouts, about a quarter of rows) cannot be attributed and are excluded
-- here; the denominator is identified revenue only.
WITH per_customer AS (
    SELECT
        customer_id,
        COUNT(DISTINCT DATE(invoice_ts)) AS active_days,
        SUM(revenue)                     AS revenue
    FROM retail_lines
    WHERE customer_id IS NOT NULL
    GROUP BY customer_id
)
SELECT
    CASE WHEN active_days = 1 THEN 'one-off' ELSE 'repeat' END AS customer_type,
    COUNT(*)                                                   AS customers,
    ROUND(SUM(revenue), 2)                                     AS revenue,
    ROUND(100.0 * SUM(revenue)
          / (SELECT SUM(revenue) FROM per_customer), 1)        AS revenue_share_pct
FROM per_customer
GROUP BY customer_type
ORDER BY customer_type;
