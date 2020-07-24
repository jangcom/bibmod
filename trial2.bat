@echo off
set out_flag=_trial2
set turn_on=rm_periods_from_journal
set turn_off=enclose_with_braces,subscript_molecules,superscript_isotopes,greeks_to_tex_cmds,accented_to_tex_syntaxes
bibmod.pl ./samples/sample.bib --prf=./prf/sample.prf --out_flag=%out_flag% --turn_on=%turn_on% --turn_off=%turn_off%
