/* Create an Interbase database */
/* C:\Database\IBKeyValue.IB    */
/* Then run this script         */

/* Table: ASTORE, Owner: SYSDBA */

CREATE TABLE "ASTORE" 
(
  "AKEY"	VARCHAR(128) NOT NULL,
  "AVALUE"	VARCHAR(512),
  "ADATETIME"	TIMESTAMP,
 PRIMARY KEY ("AKEY")
);