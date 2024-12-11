program electricity
implicit none
integer :: customer_id, previous_reading, current_reading, units_used
real :: charge, total_charge,meter_demand=60.0
real::calculate_industrial_charge,calculate_bulk_industrial_charge,calculate_domestic_charge
real,parameter::vat_rate=0.05
character(20) :: category
print *, "Enter customer ID:"
read *, customer_id
print *, "Enter category (Industrial, BulkIndustrial, Domestic):"
read *, category
print *, "Enter current meter reading:"
read *, current_reading
print *, "Enter previous meter reading:"
read *, previous_reading
units_used = current_reading - previous_reading

if(category=='Industrial') then
charge = calculate_industrial_charge(units_used)
else if(category=='BulkIndustrial') then
charge = calculate_bulk_industrial_charge(units_used)
else if(category=='Domestic') then
 charge = calculate_domestic_charge(units_used)
 else 
 print*,"Invalid Category"
 end if
 charge=charge*(1.0+vat_rate)
 total_charge=charge+meter_demand
 print*,"Total Charge=",total_charge
 end program electricity

 real function calculate_industrial_charge(units)
 implicit none
 integer,intent(in)::units
 real::cost,x,p,q,r
 print*,"Enter the value of rate par unit x,p,q,r"
 read*,x,p,q,r
 cost=0.0
 if(units<=150) then
 cost=x
 else if(units<=300) then
 cost=x+(units-150)*p
 else if(units<=500) then
 cost=x+150*p+(units-300)*q
 else
 cost=x+150*p+200*q+(units-500)*r
 end if
 calculate_industrial_charge=cost
 end function calculate_industrial_charge

 real function calculate_bulk_industrial_charge(units)
 implicit none
 integer,intent(in)::units
 real::cost,y,u,v,w
 print*,"Enter the value of rate par unit y,u,v,w"
 read*,y,u,v,w
 cost=0.0
 if(units<=100) then
 cost=y
 else if(units<=200) then
 cost=y+(units-100)*u
 else if(units<=400) then
 cost=y+100*u+(units-200)*v
 else
 cost=y+100*u+200*v+(units-400)*w
 end if
 calculate_bulk_industrial_charge=cost
 end function calculate_bulk_industrial_charge

 real function calculate_domestic_charge(units)
 implicit none
 integer,intent(in)::units
 real::cost,A,B,C,D,G
 print*,"Enter the value of rate par unit A,B,C,D,G"
 read*,A,B,C,D,G
 cost=0.0
 if(units<=50) then
 cost=units*A
 else if(units<=75) then
 cost=50*A+(units-50)*B
 else if(units<=200) then
 cost=50*A+25*B+(units-75)*C
 else if(units<=400) then
 cost=50*A+25*B+125*C+(units-200)*D
 else 
 cost=50*A+25*B+125*C+200*D+(units-400)*G
 end if
 calculate_domestic_charge=cost
 end function calculate_domestic_charge

