-- initialize table
CREATE TABLE "insurance" (
	policyID,
	statecode,
	county,
	eq_site_limit,
	hu_site_limit,
	fl_site_limit,
	fr_site_limit,
	tiv_2011,
	tiv_2012,
	eq_site_deductible,
	hu_site_deductible,
	fl_site_deductible,
	fr_site_deductible,
	point_latitude,
	point_longitude,
	line,
	construction,
	point_granularity
);

-- populate table with data from file
.mode csv
.import FL_insurance_sample.csv insurance
-- drop header row
DELETE FROM insurance WHERE policyID = 'policyID';


-- 1. print first 10 rows
.print ' '
.print 'First 10 rows'
SELECT * FROM insurance LIMIT 10;

-- 2. list counties in the sample
.print ' '
.print 'List of counties in the sample'
SELECT DISTINCT county FROM insurance;

-- 3. compute average appreciation from 2011 to 2012
.print ' '
.print 'Average appreciation from 2011 to 2012'
SELECT AVG(tiv_2012 - tiv_2011) FROM insurance;

-- 4. create frequency table of construction variable
.print ' '
.print 'Frequency table of construction variable'
SELECT construction, COUNT(*)
	FROM insurance
	GROUP BY construction;
