# Codebook
The codebook was created in the last part of the R script run_analysis. Please refer to the run_analysis.md file for further details.
The codebook itself is in CSV format and it lists the variable names and description of each variable in the tidy data set is given as follows:

Variable name    | Description
-----------------|------------
subject          | ID the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity         | Activity name
featDomain       | Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument   | Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration | Feature: Acceleration signal (Body or Gravity)
featVariable     | Feature: Variable (Mean or SD)
featJerk         | Feature: Jerk signal
featMagnitude    | Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis         | Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount        | Feature: Count of data points used to compute `average`
featAverage      | Feature: Average of each variable for each activity and each subject





VariableName  |	  Description
---------------------------
subject	      |   The ID of the subject who performed the activity. Values are numeric and range from 1 to 30
activity	    |   The name of activity undertaken. The activities are: {WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING}
DomainSignal  |	  The time or frequency domain signal. The singals are: {Time, Frequency}
Acceleration  |	  The acceleration signal: {Body, Gravity}
Instrument    |	  The measuring instrument. The instruments are: {Accelerometer, Gyroscope}
Jerk	        |   An indication of whether the Jerk signal was calculated. Possible values are: {No, Yes}
Magnitude	    |   An indication of whether the magnitude of the signals was calculated. Possible values are: {No, Yes}
Measure	      |   The measures which were calculated from the original observations. Possible values are: {Mean, Standard Deviation}
Axis          |	  The axis on which the signals were calculated. Possible values are: {X, Y, Z, Not Applicable}
Average	      |   The average value for each bucket
Count         |	  The count of each bucket

