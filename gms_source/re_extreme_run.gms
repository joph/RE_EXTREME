*AX: Time
set t;

*AX: Time resolution changes between runs.
set runs/r1*r4/;

*AX: What is p? Are these different price scenarios?
set p/P1*P5/;

*AX: Different hydropower (hp) stations along a river (ws). Always 5?
set hp/HP1*HP5/;

*AX: Different regions / zones of Swedish grid.
set reg/SE1*SE4/;

*AX: Different rivers along which hyrdopower stations are located. Only one
*river?
set ws/RS1/;

*AX: Different types of technology?
set technology/TEC1*TEC2/;

$ontext
AX: What are these cost parameters for? Based on the objective function below,
I assume:

C1 = cost to produce electricity from thermal plants
C2 = cost of loss of load. How has this been calculated?
C3 = cost of investment for thermal generation capacity.
C4 = cost of investment for storage capacity (battery or hydro?).
C5 = cost of investment for intermittent generation.

What about /C6*C10/? Are hydro and wind assumed to have zero generation costs?
$offtext

set cParam/C1*C10/;

alias(p,p1);
alias(hp,hp1);
alias(reg,reg1);
alias(ws,ws1);

scalar scale;

scale=1;

*****load parameters*****
*AX: Load (demand) in each region, for each time period.
parameter load(reg,t);

*AX: Length between time periods? Used for different time resolution.
parameter length(t);

*AX: Available hydropower in a particular region (reg), at a particular time
*(t), on a particular river (ws), at a particular station on that river (hp).
*Is this derived from climate data (i.e. rainfall)?
parameter hydro(reg,t,ws,hp);

*AX: Intermittent capacity / generation for each region in each time period,
*under different p scenarios.
parameter intermittent(reg,t,p);

*AX: Hydropower conversion factor. Need to clarify what this represents.
parameter hydroConvFact(reg,ws,hp);

*AX: Time between water travelling from upstream to downstream hydro plant?
parameter runOffDelay(reg,ws,hp);

*AX: For hydropower. What are the units here?
parameter minFlow(reg,ws,hp);
parameter maxFlow(reg,ws,hp);
parameter maxReservoir(reg,ws,hp);
parameter maxHydPower(reg,ws,hp);

*AX: I think this represents up-river production. Need to check.
parameter upRiver(reg,ws,hp,hp1);

*AX: For each region, and each p scenario, there are different technology
*investment options.
parameter investOptions(reg,p,technology);

*AX: Transmission capacity limit between regions.
parameter transmissionCap(reg,reg1);

*AX: What are these costs? I think I understand what /C1*C5/ represent. Not
*sure about /C6*C10/
parameter costs(cParam);

*AX: Loading in the data.
$GDXIN "input_tr.gdx"

$LOAD t,hydro,intermittent,minFlow,maxFlow,maxReservoir,upRiver
$LOAD hydroConvFact,runOffDelay,load,investOptions,length,transmissionCap
$load costs,maxHydPower
$GDXIN

*AX: Used to store results from each run.
parameter results(runs,reg,*);

*AX: Investment in thermal capacity. Why is it not defined over different
*regions?
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
*AX: Load (demand) in each region for each time period, after scaling.
load(reg,t)=load(reg,t)/scale;

*AX: Scaling the available hydropower available in each region, at different
*times, from different river systems (ws), and hydropower stations (hp) along
*that river. This is then scaled. How is the scaling factor determined?
hydro(reg,t,ws,hp)=hydro(reg,t,ws,hp)/scale;

*AX: Min and max flow constraints for hydropower in each region (time
*invariant), for different rivers (ws), and different hydropower plants (hp)
*along that river.
minFlow(reg,ws,hp)=minFlow(reg,ws,hp)/scale;
maxFlow(reg,ws,hp)=maxFlow(reg,ws,hp)/scale;

*AX: Max reservoir capacity in each region (time invariant), from different
*rivers (ws), and hydropower stations (hp) along that river. This is then
*scaled.
maxReservoir(reg,ws,hp)=maxReservoir(reg,ws,hp)/scale;

*AX: Transmission capacities (for power) between different regions (also
*scaled).
transmissionCap(reg,reg1)=transmissionCap(reg,reg1)/scale;

*AX: Sum of all loads, for each region, over all time periods.
parameter loadsum;
loadsum(reg)=SUM((t),load(reg,t));
display loadsum;

display load;

*AX: Need to double check what these costs are.
display costs;

positive variables

***thermal power plants
*AX: I think the x_... variables represent generation from a particular
*type of technology. E.g. thermal, hydro, intermittent, in each region,
*at each time period, under different p scenarios. Need to check
*if this interpretation is correct.

*AX: Thermal power plant generation. Check units?
x_term(reg,t,p),

***intermittent renewables
*AX: Intermittent renewable generation for each region, at each time period,
*under different p scenarios.
x_renew(reg,t,p),

***hydropower plants
*AX: Hydropower generation for each period (t), for each plant (hp) along a
*river (ws).
x_hydro(reg,t,ws,hp),

*AX: Amount of energy(?) spilled at hydro plants (e.g. it rains but the dam
*is already full)?
x_spill(reg,t,ws,hp),

*AX: Amount of energy going into hydro storage. In each reg, at each t, for
*hydro plants (hp) on a particular river (ws).
x_h_stor_in(reg,t,ws,hp),

*AX: Amount of energy coming out of hydro storage in each region, at each t,
*from different hydro plants (hp) on a paricular river (ws).
x_h_stor_out(reg,t,ws,hp),

*AX: Keeping track of the level of energy in hydro storage in each region,
*at each t, from each hydroplant (hp) along a particular river (ws).
x_h_stor_lv(reg,t,ws,hp),

*AX: Not sure what this is. Hydropower generated from plants upstream?
x_hyd_up(reg,t,ws,hp)


***other storage plants
*AX: Amount of energy going into (battery?) storage in each region, at each
*time period 't', under price scenario 'p'?
x_stor_in(reg,t,p),

*AX: Amount of energy going out of storage in each region (being utilised),
*at each time period 't', under different p scenarios? How is electricity
*from storage priced? Is it like hydro where the opportunity cost of
*supply is considered?
x_stor_out(reg,t,p),
x_stor_lev(reg,t,p),

***curtailment & losses
*AX: I'm guessing curtailment is calculated in one of the equations below. If
*supply of power exceeds demand (and it cannot be absorbed by storage) then
*intermittent generation is curtailed.
x_curtail(reg,t),

*AX: Is this loss due to load shedding in a particular region at a particular
*time? I.e. if demand > supply then x_loss = demand - supply.
x_loss(reg,t),

***investments
*AX: Guessing this is the investment in thermal capacity in each region,
*under different p scenarios. This is time invariant, so maybe
*it's the static optimal level of investment in thermal capacity over the whole
*modelling period?
x_invest_thermal_cap(reg,p),

*AX: Is this investment in storage and intermittent generators in each
*region, under different p scenarios? This level of investment is time
*invariant - giving the optimal level of investment in these
*technologies over the whole planning horizon.
x_invest_storage(reg,p),
x_invest_intermittent(reg,p)

;

variables

*AX: Total cost of investment and generation (operation) over the whole time
*horizon.
totalCost,

***transmission
*AX: Energy transfer between regions.
x_transfer(reg,reg1,t),

*AX: Slack variable for LP program?
x_slack(t)

;

equations

*AX: Minimise total cost. Investment + cost of generation?
objective

*AX: Power balance between regions.
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

$ontext
AX: Objective is to minimise total investment and production cost over all
time periods, t. I'm guessing that C1 is the cost to produce electricity from
thermal plants. These costs look constant. How have they been selected? C2
seems to be the cost of losses. How has this been selected / defined
(how are losses calculated)? C3 looks to be the cost of investing in thermal
generation capacity. This investment in thermal capacity is time invariant,
so the timing of the investment isn't considered. C4 seems to be the
cost to invest in storage capacity. Again, the investment in storage is also
time invariant. C5 seems to be the investment in intermittent generation
(also time invariant). So, the control variables are investment in thermal
capacity, investment in storage capacity, investment in intermittent
generation capacity. What happened to /C6*C10/? Are hydro and wind assumed
to have zero generation costs? What is length(t)'s function here?
$offtext

objective             ..totalCost            =E= SUM((reg,p,t)$investOptions(reg,p,"TEC1"),costs("C1")*x_term(reg,t,p)*length(t))  +
                                                 SUM((reg,t  )                            ,costs("C2")*x_loss(reg,t)*length(t))    +
                                                 SUM((reg,p  )$investOptions(reg,p,"TEC1"),costs("C3")*x_invest_thermal_cap(reg,p))+
                                                 SUM((reg,p  )$investOptions(reg,p,"TEC2"),costs("C4")*x_invest_storage(reg,p))    +
                                                 SUM((reg,p  )$existsIntermittent(reg,p)  ,costs("C5")*x_invest_intermittent(reg,p))
                                                 ;
$ontext
AX: For all regions for which there is demand for electricity, ensure that
supply and demand balance. What is the difference between x_h_stor_out and
x_stor_out? Is x_h_stor hydro storage, while x_stor_out is some other type
of storage? Where do the 0.99 and 0.9 factors come from? Are these
efficiencies of the storage technologies? Also, are variables such as x_term
(thermal) and x_renew (intermittent) production values in each
period? How are capacity factors included? How are losses and curtailment
*calculated?
$offtext

*balances supply and demand in the region
*AX: Will the last two terms for x_transfer cancel themselves out?
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

$ontext
AX: Intermittent(reg,t,p) is a parameter loaded into the model (based on
some data). Guess that it is the amount of the resource (e.g. wind / solar
energy) available in a paricular region, at a particular time. Not sure what
'p' represents. So, the total output from renewables in a particular period
is the amount of the resource available multiplied by the capacity? Is a
capacity factor for wind already factored into these parameters?

AX: Investment control variables are trying to determine the optimal level
of investment (for each type of generation) over the 30 year period.
I.e. capacity for each type of generation is constant across all time periods.
$offtext

renewable(reg,t,p)$existsIntermittent(reg,p)


                    ..x_renew(reg,t,p)       =E= intermittent(reg,t,p) *  x_invest_intermittent(reg,p);


*********hydropower production*********
*balance hydropower production at each plant, depending on upper reservoir production

$ontext
AX: hydro(...) is data from the .gdx file. Is this something like available
energy coming from rain (i.e. this is based on climate data)? So, the maximum
amount of energy that can be utilised (from rainfall) is equal to the total
amount of hydro power produced (x_hydro) + the amount that must be spilled
(x_spill) + the amount that goes into storage (x_h_stor_in) - the amount that
comes from up-river (x_hyd_up).
$offtext

hydro_bal(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                     ..hydro(reg,t,ws,hp)    =E= x_hydro(reg,t,ws,hp)    +
                                                 x_spill(reg,t,ws,hp)    +
                                                 x_h_stor_in(reg,t,ws,hp)-
                                                 x_hyd_up(reg,t,ws,hp);
*calculate up-river production
*AX: Not sure what this represents. What is hydroConvFact? Some conversion
*factor?
hydro_up(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                     ..x_hyd_up(reg,t,ws,hp) =E= SUM((hp1)$upRiver(reg,ws,hp,hp1),hydroConvFact(reg,ws,hp1)*
                                                           (x_hydro(reg,t-runOffDelay(reg,ws,hp1),ws,hp1) +
                                                            x_spill(reg,t-runOffDelay(reg,ws,hp1),ws,hp1) +
                                                            x_h_stor_out(reg,t-runOffDelay(reg,ws,hp1),ws,hp1))
                                                 );

*level of storage in hydropower plants
*AX: Storage level in current period is equal to the storage level in the
*previous period, + the net storage in (i.e. x_h_stor_in - x_h_stor_out).
storage_h_lev(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                   ..x_h_stor_lv(reg,t,ws,hp)=E= x_h_stor_lv(reg,t-1,ws,hp)+
                                                 x_h_stor_in(reg,t,ws,hp)-
                                                 x_h_stor_out(reg,t,ws,hp);
*maximum level of storage
*AX: Ensure that the storage level in each region, at each time period is less
*than or equal to maximum reservoir capacity.
storage_h_max(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                   ..x_h_stor_lv(reg,t,ws,hp)=L= maxReservoir(reg,ws,hp);

*miminum flow constraint
*AX: What are the units here? Is minFlow in units volume/time? What about
*x_hydro? Initially I thought x_hydro was power produced from hydro,
*but now I'm not sure. Same for x_spill and x_h_stor_out.
min_flow(reg,t,ws,hp)$existsHydro(reg,ws,hp)
                    ..minFlow(reg,ws,hp)     =L= x_hydro(reg,t,ws,hp)     +
                                                 x_spill(reg,t,ws,hp)     +
                                                 x_h_stor_out(reg,t,ws,hp)
                                                 ;

*maximum flow constraint
*AX: Same comment for minFlow constraint above.
max_flow(reg,ws,hp,t)$existsHydro(reg,ws,hp)
                    ..maxFlow(reg,ws,hp)     =G= x_hydro(reg,t,ws,hp)     +
                                                 x_spill(reg,t,ws,hp)     +
                                                 x_h_stor_out(reg,t,ws,hp);

*maximum hydropower production, as limited by turbines
*AX: Here x_hydro seems to be in units of power (e.g. MW). Max power from hydro
*must be less than the amount produced by hydro + the amount of power generated
*from drawing on storage. I think the units here are energy (MWh).
max_hp_power(reg,ws,hp,t)$existsHydro(reg,ws,hp)
                    ..maxHydPower(reg,ws,hp) =L=x_hydro(reg,t,ws,hp) +
                                                 x_h_stor_out(reg,t,ws,hp);

*********battery storage*********

*level of storage in batteries
*AX: Level of battery storage equals the amount of storage in the previous
*period + net amount of energy added to batteries (x_stor_in - x_stor_out).
storage_lev(reg,t,p)$investOptions(reg,p,"TEC2")
                    ..x_stor_lev(reg,t,p)    =E= x_stor_lev(reg,t-1,p)+
                                                 x_stor_in(reg,t,p)-
                                                 x_stor_out(reg,t,p);
*maximum level of storage
*AX: The level of battery storage (x_stor_lev) must be less than or equal
*to the amount of storage capacity (x_invest_storage).
storage_max(reg,t,p)$investOptions(reg,p,"TEC2")
                    ..x_stor_lev(reg,t,p)    =L= x_invest_storage(reg,p);


*********thermal power production*********

*constraints production to maximal capacity
$ontext
AX: Generation from thermal plants (x_term) in each region, at each time
period, must be less than or equal to the length of the time period (length(t))
multiplied by the generation capacity. E.g. plant with 10MW capacity
operating for 1.5hrs generates 15MWh. So that must mean all energy terms
related to energy production / storage are in MWh? Also, may want to consider
putting some safety factor here. E.g. ensure generation does not exceed 80% of
installed capacity.
$offtext

res_therm(reg,t,p)$investOptions(reg,p,"TEC1")
                    ..x_term(reg,t,p)       =L= length(t)*x_invest_thermal_cap(reg,p);

*********transmission*********
*AX: Amount of electricity transfered from one region to another (reg to reg1)
*must be less than or equal to the maximum transmission capacity
*(transmissionCap).

transmission_cap(reg,reg1,t)$transmissionCap(reg,reg1)
                    ..x_transfer(reg,reg1,t) =L= transmissionCap(reg,reg1);

*AX: Active power flow must balance between regions. Flow from region1 to
*region2 must equal -flow region2 to region1.

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

*AX: Is this to figure out the amount of time it takes to run the model?
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
