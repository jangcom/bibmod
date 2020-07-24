@echo off
set out_flag=_trial1
set turn_on=rm_periods_from_journal
bibmod.pl ./samples/sample.bib --prf=./prf/sample.prf --out_flag=%out_flag% --turn_on=%turn_on%
