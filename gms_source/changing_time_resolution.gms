set t;
set runs/r1*r4/;
set loc;

parameter load(t);
parameter length(t);
parameter hydro(t);
parameter intermittent(loc,t);

$GDXIN "input_tr.gdx"
$LOAD loc,t,load,length,hydro,intermittent
$GDXIN

parameter results(runs,*);
parameter invest_thermal_cap;


positive variables
x_term(t),
x_renew(loc,t),
x_hydro(t),
x_stor_in(t),
x_stor_out(t),
x_stor_lev(t),
x_curtail(t),
x_loss(t),
x_invest_thermal_cap,
x_invest_storage,
x_invest_intermittent(loc)
;

variables
totalCost;

equations
objective

bal,

renewable,

hydro_bal,

storage_lev,

storage_max,

res_therm
;


objective..     totalCost        =E= SUM(t,x_term(t)+300*x_loss(t))+
                                     50000*x_invest_thermal_cap+
                                     250*x_invest_storage+
                                     SUM(loc,25000*x_invest_intermittent(loc));

bal(t)..        load(t)          =E= x_term(t)+
                                     x_stor_out(t)+
                                     SUM(loc,x_renew(loc,t))+
                                     x_hydro(t)+
                                     x_loss(t)-
                                     x_curtail(t)
                                     ;


renewable(loc,t)..x_renew(loc,t) =E= intermittent(loc,t) *  x_invest_intermittent(loc);

hydro_bal(t)..  hydro(t)         =E= x_hydro(t)+x_stor_in(t);

storage_lev(t)..x_stor_lev(t)    =E= x_stor_lev(t-1)+
                                     0.9*x_stor_in(t)-
                                     x_stor_out(t);

storage_max(t)..x_stor_lev(t)    =L= x_invest_storage;

res_therm(t)..  x_term(t)        =L= length(t)*x_invest_thermal_cap;

option limrow =100;

model mint/
          all
/;

scalar starttime; starttime = jnow;

SOLVE mint using LP minimizing totalCost;
scalar elapsed; elapsed = (jnow - starttime)*24*3600;

results("r1","thermal")=SUM(t,x_term.l(t));
results("r1","w1")=SUM((t),x_renew.l("l1",t));
results("r1","w2")=SUM((t),x_renew.l("l2",t));
results("r1","w3")=SUM((t),x_renew.l("l3",t));
results("r1","w4")=SUM((t),x_renew.l("l4",t));
results("r1","hydro")=SUM(t,x_hydro.l(t));
results("r1","stor_out")=SUM(t,x_stor_out.l(t));
results("r1","curtail")=SUM(t,x_curtail.l(t));
results("r1","investment_thermal")=x_invest_thermal_cap.l;
results("r1","investment_storage")=x_invest_storage.l;
results("r1","total cost")=totalCost.l;
results("r1","elapsed")=elapsed;
results("r1","load")=SUM(t,load(t));  ;

results("r1","loss")=SUM(t,x_loss.l(t));

display  results;

Execute_Unload 'results_time_resolution.gdx',x_term,x_stor_out,x_renew,load,intermittent,hydro,x_stor_in,x_curtail;

*$offtext
