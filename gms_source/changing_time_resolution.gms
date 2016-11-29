set t;
set runs/r1*r4/;
set p/P1*P5/;
set hp/HP1*HP5/;
set reg/SE1*SE4/;
set ws/RS1/;
set technology/TEC1*TEC2/;
set cParam/C1*C10/;

alias(p,p1);
alias(hp,hp1);
alias(reg,reg1);
alias(ws,ws1);

scalar scale;
scale=1;

*****load parameters*****
parameter load(reg,t);
parameter length(t);
parameter hydro(reg,t,ws,hp);
parameter intermittent(reg,t,p);
parameter hydroConvFact(reg,ws,hp);
parameter runOffDelay(reg,ws,hp);
parameter minFlow(reg,ws,hp);
parameter maxFlow(reg,ws,hp);
parameter maxReservoir(reg,ws,hp);
parameter maxHydPower(reg,ws,hp);
parameter upRiver(reg,ws,hp,hp1);
parameter investOptions(reg,p,technology);
parameter transmissionCap(reg,reg1);
parameter costs(cParam);




$GDXIN "input_tr.gdx"
$LOAD t,hydro,intermittent,minFlow,maxFlow,maxReservoir,upRiver
$LOAD hydroConvFact,runOffDelay,load,investOptions,length,transmissionCap
$load costs,maxHydPower
$GDXIN

parameter results(runs,reg,*);
parameter invest_thermal_cap;

*****helper variables*****

parameter existsIntermittent(reg,p);
existsIntermittent(reg,p)=sign(SUM((t),(intermittent(reg,t,p))));
parameter existsHydro(reg,ws,hp);
existsHydro(reg,ws,hp)=sign(SUM((t),(hydro(reg,t,ws,hp))));
parameter hasLoad(reg,t);
hasLoad(reg,t)=sign(load(reg,t));



display existsIntermittent,
        existsHydro,
        intermittent,
        investOptions
         ;

*****Scaling*****
load(reg,t)=load(reg,t)/scale;
hydro(reg,t,ws,hp)=hydro(reg,t,ws,hp)/scale;
minFlow(reg,ws,hp)=minFlow(reg,ws,hp)/scale;
maxFlow(reg,ws,hp)=maxFlow(reg,ws,hp)/scale;
maxReservoir(reg,ws,hp)=maxReservoir(reg,ws,hp)/scale;
transmissionCap(reg,reg1)=transmissionCap(reg,reg1)/scale;

parameter loadsum;
loadsum(reg)=SUM((t),load(reg,t));
display loadsum;

display load;

display costs;

positive variables

***thermal power plants
x_term(reg,t,p),

***intermittent renewables
x_renew(reg,t,p),

***hydropower plants
x_hydro(reg,t,ws,hp),
x_spill(reg,t,ws,hp),
x_h_stor_in(reg,t,ws,hp),
x_h_stor_out(reg,t,ws,hp),
x_h_stor_lv(reg,t,ws,hp),
x_hyd_up(reg,t,ws,hp)

***other storage plants
x_stor_in(reg,t,p),
x_stor_out(reg,t,p),
x_stor_lev(reg,t,p),

***curtailment & losses
x_curtail(reg,t),
x_loss(reg,t),

***investments
x_invest_thermal_cap(reg,p),
x_invest_storage(reg,p),
x_invest_intermittent(reg,p)

;

variables
totalCost,

***transmission
x_transfer(reg,reg1,t),

x_slack(t)



;

equations
objective

bal,

renewable,

hydro_bal,

hydro_up,

storage_h_lev,

storage_h_max,

min_flow,

max_flow,

max_hp_power,

storage_lev,

storage_max,

res_therm,

transmission_cap,

transmission_bal
;


objective             ..totalCost            =E= SUM((reg,p,t)$investOptions(reg,p,"TEC1"),costs("C1")*x_term(reg,t,p)*length(t))  +
                                                 SUM((reg,t  )                            ,costs("C2")*x_loss(reg,t)*length(t))    +
                                                 SUM((reg,p  )$investOptions(reg,p,"TEC1"),costs("C3")*x_invest_thermal_cap(reg,p))+
                                                 SUM((reg,p  )$investOptions(reg,p,"TEC2"),costs("C4")*x_invest_storage(reg,p))    +
                                                 SUM((reg,p  )$existsIntermittent(reg,p)  ,costs("C5")*x_invest_intermittent(reg,p))
                                                 ;

*balances supply and demand in the region
bal(reg,t)$hasLoad(reg,t)
                      ..load(reg,t)          =E= SUM(p$investOptions(reg,p,"TEC1"),x_term(reg,t,p))                +
                                                 SUM((ws,hp)$existsHydro(reg,ws,hp),0.99*x_h_stor_out(reg,t,ws,hp))+
                                                 SUM((p)$investOptions(reg,p,"TEC2"),0.9*x_stor_out(reg,t,p))      +
                                                 SUM(p$existsIntermittent(reg,p),x_renew(reg,t,p))                 +
                                                 SUM((ws,hp)$existsHydro(reg,ws,hp),x_hydro(reg,t,ws,hp))          +
                                                 x_loss(reg,t)                                                     -
                                                 SUM(p$investOptions(reg,p,"TEC2"),x_stor_in(reg,t,p))             -
                                                 x_curtail(reg,t)                                                  +
                                                 SUM(reg1$transmissionCap(reg,reg1),x_transfer(reg1,reg,t))        -
                                                 SUM(reg1$transmissionCap(reg1,reg),x_transfer(reg,reg1,t))
                                                ;

*********Intermittent renewables*********

*renewable generation at each locatio
renewable(reg,t,p)$existsIntermittent(reg,p)
                    ..x_renew(reg,t,p)       =E= intermittent(reg,t,p) *  x_invest_intermittent(reg,p);


*********hydropower production*********

*balance hydropower production at each plant, depending on upper reservoir production
hydro_bal(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                     ..hydro(reg,t,ws,hp)    =E= x_hydro(reg,t,ws,hp)    +
                                                 x_spill(reg,t,ws,hp)    +
                                                 x_h_stor_in(reg,t,ws,hp)-
                                                 x_hyd_up(reg,t,ws,hp);
*calculate up-river production
hydro_up(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                     ..x_hyd_up(reg,t,ws,hp) =E= SUM((hp1)$upRiver(reg,ws,hp,hp1),hydroConvFact(reg,ws,hp1)*
                                                           (x_hydro(reg,t-runOffDelay(reg,ws,hp1),ws,hp1) +
                                                            x_spill(reg,t-runOffDelay(reg,ws,hp1),ws,hp1) +
                                                            x_h_stor_out(reg,t-runOffDelay(reg,ws,hp1),ws,hp1))
                                                 );

*level of storage in hydropower plants
storage_h_lev(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                   ..x_h_stor_lv(reg,t,ws,hp)=E= x_h_stor_lv(reg,t-1,ws,hp)+
                                                 x_h_stor_in(reg,t,ws,hp)-
                                                 x_h_stor_out(reg,t,ws,hp);
*maximum level of storage
storage_h_max(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                   ..x_h_stor_lv(reg,t,ws,hp)=L= maxReservoir(reg,ws,hp);

*miminum flow constraint
min_flow(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                    ..minFlow(reg,ws,hp)     =L= x_hydro(reg,t,ws,hp)     +
                                                 x_spill(reg,t,ws,hp)     +
                                                 x_h_stor_out(reg,t,ws,hp)
                                                 ;

*maximum flow constraint
max_flow(reg,ws,hp,t)$existsHydro(reg,ws,hp)
                    ..maxFlow(reg,ws,hp)     =G= x_hydro(reg,t,ws,hp)     +
                                                 x_spill(reg,t,ws,hp)     +
                                                 x_h_stor_out(reg,t,ws,hp);

*maximum hydropower production, as limited by turbines
max_hp_power(reg,ws,hp,t)$existsHydro(reg,ws,hp)
                    ..maxHydPower(reg,ws,hp) =L=x_hydro(reg,t,ws,hp) +
                                                 x_h_stor_out(reg,t,ws,hp);

*********battery storage*********

*level of storage in batteries
storage_lev(reg,t,p)$investOptions(reg,p,"TEC2")
                    ..x_stor_lev(reg,t,p)    =E= x_stor_lev(reg,t-1,p)+
                                                 x_stor_in(reg,t,p)-
                                                 x_stor_out(reg,t,p);
*maximum level of storage
storage_max(reg,t,p)$investOptions(reg,p,"TEC2")
                    ..x_stor_lev(reg,t,p)    =L= x_invest_storage(reg,p);


*********thermal power production*********

*constraints production to maximal capacity
res_therm(reg,t,p)$investOptions(reg,p,"TEC1")
                    ..x_term(reg,t,p)       =L= length(t)*x_invest_thermal_cap(reg,p);

*********transmission*********

transmission_cap(reg,reg1,t)$transmissionCap(reg,reg1)
                    ..x_transfer(reg,reg1,t) =L= transmissionCap(reg,reg1);

transmission_bal(reg,reg1,t)$transmissionCap(reg,reg1)
                    ..x_transfer(reg,reg1,t) =E= -1*x_transfer(reg1,reg,t);

option limrow =100;

model mint/
          objective
          ,bal
          ,renewable
          ,hydro_bal
          ,hydro_up
          ,storage_h_lev
          ,storage_h_max
          ,min_flow
          ,max_flow
          ,storage_lev
          ,storage_max
          ,res_therm
          ,transmission_cap
          ,transmission_bal
/;

scalar starttime; starttime = jnow;

SOLVE mint using LP minimizing totalCost;
scalar elapsed; elapsed = (jnow - starttime)*24*3600;
$ontext
results("r1",reg,"thermal")           =SUM((p,t),x_term.l(reg,p,t));
results("r1",reg,"renewable")         =SUM((p,t),x_renew.l(reg,p,t));
results("r1",reg,"spill")             =SUM((ws,hp,t),x_spill.l(reg,ws,hp,t));
results("r1",reg,"hydro")             =SUM((ws,hp,t),x_hydro.l(reg,ws,hp,t));
results("r1",reg,"stor_in")           =SUM((p,t),x_stor_out.l(reg,p,t));
results("r1",reg,"stor_out")          =SUM((p,t),x_stor_out.l(reg,p,t));
results("r1",reg,"curtail")           =SUM((t),x_curtail.l(reg,t));
results("r1",reg,"i_thermal")         =SUM((p),x_invest_thermal_cap.l(reg,p));
results("r1",reg,"i_storage")         =SUM((p),x_invest_storage.l(reg,p));
results("r1",reg,"i_renewable")       =SUM((p),x_invest_intermittent.l(reg,p));
results("r1","SE1","total cost")      =totalCost.l;
results("r1","SE1","elapsed")         =elapsed;
results("r1",reg,"load")              =SUM((t),load(reg,t));  ;
results("r1",reg,"loss")              =SUM((t),x_loss.l(reg,t));
results("r1",reg,"imports")           =SUM((reg1,t),x_transfer.l(reg1,reg,t));
results("r1",reg,"exports")           =SUM((reg1,t),x_transfer.l(reg,reg1,t));
results("r1",reg,"iflowQuantTotal")   =SUM((ws,hp,t),hydro(reg,ws,hp,t));

parameter transfers(reg,reg1,t);
transfers(reg,reg1,t)=x_transfer.l(reg,reg1,t);


display  results,
         transfers;
$offtext
option gdxUELs=full;

Execute_Unload 'results_time_resolution.gdx';
*,x_term,x_stor_out,x_renew,load,intermittent,hydro,x_stor_in,x_curtail,elapsed;


