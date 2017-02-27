set T/T1*T70128/;

set VAR/hydro_shype,hydro_shype_cor,load,solar,wind_merra/;

parameter TS(T,VAR)/
$include ../RE_EXTREME/data/gams_input_simple/TS.csv
/;



TS(T,VAR)=TS(T,VAR)/10E5;

parameter sums(VAR);
sums(VAR)=SUM(T,TS(T,VAR));

display sums;
display TS;


positive variable x_thermal(t),
x_loss(t),
x_curtailment(t),
x_hydro(t),
x_stor_in(t),
x_stor_out(t),
x_stor_lev(t),
x_wind(t),
x_pv(t),
x_wind_inv,
x_pv_inv;

variable x_tot_thermal;

equations

e_obj,
e_bal,
e_inv_wind,
e_inv_pv,
e_limit_thermal,
e_limit_hydro,
e_hydr_bal,
e_stor_bal_hydro,
e_stor_max_hydro,
e_renewable_expansion;

e_obj..          x_tot_thermal=E=sum(t,x_thermal(t)+1000*x_loss(t));

e_bal(T)..       TS(T,"load")=E=x_thermal(t)+x_loss(t)+x_hydro(t)+x_stor_out(t)+x_wind(t)+x_pv(t)-x_curtailment(t);

e_inv_wind(T)..     x_wind(t)=E=TS(T,"wind_merra")*x_wind_inv;

e_inv_pv(T)..       x_pv(t)=E=TS(T,"solar")*x_pv_inv;

e_limit_thermal(T)..x_thermal(t)=L=0.009;

e_limit_hydro(T)..x_hydro(t)+x_stor_out(t)=L=0.016;

e_hydr_bal(T)..TS(T,"hydro_shype")=E=x_stor_in(t)+x_hydro(t);

e_stor_bal_hydro(T)..x_stor_lev(t)=E=x_stor_in(t)-x_stor_out(t);

e_stor_max_hydro(T)..x_stor_lev(t)=L=33;

e_renewable_expansion..SUM(t,x_wind(t))+SUM(t,x_pv(t))=L=SUM(T,TS(T,"load"))-SUM(T,TS(T,"hydro_shype"));

model limitsimple /all/;

solve limitsimple minimize x_tot_thermal using LP;

parameter results(*);
results("totalthermal")=sum(T,x_thermal.L(T));
results("totalloss")=sum(T,x_loss.L(T));
results("totalsolar")=SUM(T,TS(T,"solar")*x_pv_inv.L);
results("totalwind")=SUM(T,TS(T,"wind_merra")*x_wind_inv.L);
results("totalhydro")=SUM(T,x_hydro.L(T)+x_stor_out.L(T));

display results;

Execute_Unload 'results_simple.gdx';








