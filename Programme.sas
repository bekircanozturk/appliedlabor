libname raw "C:/Users/Public/Documents/bekircan gonzalo/entreprisesraw";
libname subset "C:/Users/Public/Documents/bekircan gonzalo/entreprises";
data subset.entreprisesclean;
	set
		raw.ent2010(keep=APET EFF_MOY_ET NBPNA_SEXECSR_13 NBPNA_SEXECSR_14 NBPNA_SEXECSR_16 NBPNA_SEXECSR_23 NBPNA_SEXECSR_24 NBPNA_SEXECSR_26 S_BRUT SIRET ZEMPT) run;
		raw.ent2011(keep=APET EFF_MOY_ET NBPNA_SEXECSR_13 NBPNA_SEXECSR_14 NBPNA_SEXECSR_16 NBPNA_SEXECSR_23 NBPNA_SEXECSR_24 NBPNA_SEXECSR_26 S_BRUT SIRET ZEMPT) run;
		raw.ent2012(keep=APET EFF_MOY_ET NBPNA_SEXECSR_13 NBPNA_SEXECSR_14 NBPNA_SEXECSR_16 NBPNA_SEXECSR_23 NBPNA_SEXECSR_24 NBPNA_SEXECSR_26 S_BRUT SIRET ZEMPT) run;
		raw.ent2013(keep=APET EFF_MOY_ET NBPNA_SEXECS_13 NBPNA_SEXECS_14 NBPNA_SEXECS_16 NBPNA_SEXECS_23 NBPNA_SEXECS_24 NBPNA_SEXECS_26 S_BRUT SIRET ZEMPT) run;
		raw.ent2014(keep=APET EFF_MOY_ET NBPNA_SEXECS_13 NBPNA_SEXECS_14 NBPNA_SEXECS_16 NBPNA_SEXECS_23 NBPNA_SEXECS_24 NBPNA_SEXECS_26 S_BRUT SIRET ZEMPT)
		raw.ent2015(keep=APET EFF_MOY_ET NBPNA_SEXECS_13 NBPNA_SEXECS_14 NBPNA_SEXECS_16 NBPNA_SEXECS_23 NBPNA_SEXECS_24 NBPNA_SEXECS_26 S_BRUT SIRET ZEMPT)
		;
		run;
%macro subset_by_year;
	%do year = 2008 %to 2008;
		data subset.ent&year._sub;
		set raw.ent&year (keep=APET EFF_MOY_ET NBPNA_SEXECSR_13 NBPNA_SEXECSR_14 NBPNA_SEXECSR_16 NBPNA_SEXECSR_23 NBPNA_SEXECSR_24 NBPNA_SEXECSR_26 S_BRUT SIRET ZEMPT);
		run;
		%end;
		%mend
%subset_by_year

%macro subset_by_year2;
	%do year = 2013 %to 2015;
		data subset.ent&year._sub;
		set raw.ent&year (keep=APET EFF_MOY_ET NBPNA_SEXECS_13 NBPNA_SEXECS_14 NBPNA_SEXECS_16 NBPNA_SEXECS_23 NBPNA_SEXECS_24 NBPNA_SEXECS_26 S_BRUT SIRET ZEMPT);
		run;
		%end;
		%mend
%subset_by_year2