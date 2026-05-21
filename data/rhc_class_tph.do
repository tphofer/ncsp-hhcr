use https://public.websites.umich.edu/~thofer/data/rhc_class_tph.dta,clear
noi notes
noi di "Type the command {stata data_desc} for a organized list of the variables"
noi di "Type {stata data_desc,short} for a short organized list"

* run https://public.websites.umich.edu/~thofer/data/rhc_class_tph.do

cap prog drop data_desc
prog define data_desc	
	syntax , [short]
	if "`short'"==""	{
		noi di _n "*****  Study variables"
		noi de id-ptid sadmdte-dthdte
		noi di _n "*****  Outcome variables"
		noi de t3d30-death
		noi di _n "*****  Treatment"
		noi de swang1
		noi di _n "*****  Demographics"
		noi de age-ninsclas
		noi di _n "*****  Admission diagnosis"
		noi de cat1-trauma
		noi di _n "*****  Key confounders"
		noi de dnr1-ca
		noi di _n "*****  Day 1 physiology components"
		noi de aps1 resp1-pafi1 wblc1-paco21
		noi di _n "*****  Past Medical History"
		noi de cardiohx-immunhx
	}
	else if "`short'"=="short" {
		noi di _n "*****  Study variables"
		noi ds id-ptid sadmdte-dthdte
		noi di _n "*****  Outcome variables"
		noi ds t3d30-death
		noi di _n "*****  Treatment"
		noi ds swang1
		noi di _n "*****  Demographics"
		noi ds age-ninsclas
		noi di _n "*****  Admission diagnosis"
		noi ds cat1-trauma
		noi di _n "*****  Key confounders"
		noi ds dnr1-ca
		noi di _n "*****  Day 1 physiology components"
		noi ds aps1 resp1-pafi1 wblc1-paco21
		noi di _n "*****  Past Medical History"
		noi ds cardiohx-immunhx
	}
end	

cap prog drop macro_make
prog define macro_make
	qui	{	
		vl set
		display "$vlcontinuous"
		display "$vlcategorical"
		vl create cvars = vlcontinuous - (id sadmdte lstctdte dschdte dthdte t3d30 )
		*makespline bspline aps1 surv2md1, basis(_bs) knots(3)
		*vl modify cvars = cvars - (aps1 surv2md1)
		vl modify cvars = cvars + (scoma1 sod1 /*_bs* */)
		vl create fvars = vlcategorical - (agecat sex swang1 dth30 dth60 death)
		vl sub allvars = c.cvars i.fvars c.cvars#i.fvars

		vl create demo_fvar = (male race income ninsclas)
		vl create demo_cvar = (age edu) 
		vl create dx_fvar = (cat1 cat2 resp seps card meta ///
			renal gastr hema neuro ortho trauma )
		vl create casemix_fvar = (dnr1 transhx ca cardiohx-immunhx)
		vl create casemix_cvar = (das2d3pc-scoma1)
		vl create phys_cvar = (resp1 - paco21)
	}
	noi macro list demo_fvar demo_cvar dx_fvar casemix_fvar casemix_cvar phys_cvar
end
