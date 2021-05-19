PROC IMPORT DATAFILE = '/folders/myfolders/HEART.UPDATED.csv'OUT = heart DBMS = csv;
title 'Importing heart transplant data'
RUN;

PROC PRINT DATA = heart;
title 'Heart transplant data';
RUN;

DATA heart;
SET heart;
IF SURTIME = . THEN DELETE;
IF STATUS = . THEN DELETE;
IF AGE = . THEN DELETE;
IF MISCORE = . THEN DELETE;
IF SEX = . THEN DELETE;
RUN;

PROC FORMAT;
VALUE SURGRPa 1 = 'surtime < 180'
            2 = '180 <= surtime < 365'
            3 = '365 <= surtime < 730'
            4 = '730 <= surtime < 1825'
            5 = 'surtime >= 1825';
VALUE AGEGRPa 1 = 'age < 25'
            2 = '25 <= age < 30'
            3 = '30 <= age < 45'
            4 = '45 <= age < 60'
            5 = 'age >= 60';
VALUE STATUSa 0 = 'ALIVE' 1 = 'DEAD';
VALUE SEXa 1 = 'FEMALE' 2 = 'MALE';
RUN;

DATA heart2;
SET heart;
IF (AGE < 25) THEN AGEGRP = 1;
IF (25 <= AGE < 30) THEN AGEGRP = 2;
IF (30 <= AGE < 45) THEN AGEGRP = 3;
IF (45 <= AGE < 60) THEN AGEGRP = 4;
IF (AGE >= 60) THEN AGEGRP = 5;
IF (SURTIME < 180) THEN SURGRP = 1;
IF (180 <= SURTIME < 365) THEN SURGRP = 2;
IF (365 <= SURTIME < 730) THEN SURGRP = 3;
IF (730 <= SURTIME < 1825) THEN SURGRP = 4;
IF (SURTIME >= 1825) THEN SURGRP = 5;

LABEL AGE = 'Age at the time of transplant'
      SURTIME = 'Survival time in days after transplant'
      MISCORE = 'Mismatch score'
      AGEGRP = "Age at the time of transplant by group"
      SURGRP = "Survival time in days after transplant by group";
FORMAT STATUS STATUSa. SEX SEXa. AGEGRP AGEGRPa. SURGRP SURGRPa.;
RUN;
      
PROC PRINT DATA = heart2; 
title 'Updated heart transplant data';
RUN;

* Assumption for Chi-Sq test: the expected count in each cell must be >= 5 ;
PROC FREQ DATA = heart2;
TABLES SURGRP * AGEGRP /CHISQ NOROW NOCOL NOPERCENT EXPECTED;
title 'Chi-square test of independence on survival time and age';
RUN;
* IF p < 0.05, reject H0, they are dependent;

PROC FREQ DATA = heart2;
TABLES SURGRP * SEX / CHISQ NOROW NOCOL NOPERCENT EXPECTED;
title 'Chi-square test of independence on survival time and sex';
RUN;

PROC FREQ DATA = heart2;
TABLES SURGRP * STATUS / CHISQ NOROW NOCOL NOPERCENT EXPECTED;
title 'Chi-square test of independence on survival time and status';
RUN;

*If Expected < 5, we must run Fisher's exact test JUST ADD EXACT AT END;
*PROC FREQ DATA = heart2;
*TABLES SURGRP * AGEGRP /CHISQ NOROW NOCOL NOPERCENT EXPECTED EXACT;
*RUN
*If p > 0.05, do not reject H0, they are independent;

DATA male;
SET heart2;
IF SEX = 2;
RUN;

PROC PRINT DATA = male;
title 'Male heart transplant data';
RUN;

PROC FREQ DATA = male;
TABLES AGEGRP / TESTP = (.10, .15, .20, .20, .35);
title 'Chi-square goodness of fit test for male patients by age group';
RUN; 




