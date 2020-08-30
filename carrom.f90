program ellipticalcarrom
implicit none

integer::i,n1,n2,q1,q2
real::x,y,theta1,thetarad1,t1,t2,xp,yp,d1,dt,d2,slope1,slope2,t,theta2,thetarad2,vx1,vy1,vx2,vy2
real,parameter::pi=acos(-1.0),xor=0.0,yor=0.0,v=0.5,f1x=-4,f1y=0,f2x=4,f2y=0,t0=0.0

print*,"this is for a carrom board of a=5,b=3,foci=+-4 centerred at (0,0)"

print*,"choose a point on the perimeter of the ellipse x0,y0"
read*,xp,yp

print*,"give step size of time"
read*,dt

!============before reflection========================================================
d1=sqrt((xp-f1x)**2+(yp-f1y)**2)
t1=d1/v
n1=abs(int((t1-t0)/dt))
slope1=(yp-f1y)/(xp-f1x)
thetarad1=atan(slope1)
vx1=abs(v*cos(thetarad1))
vy1=abs(v*sin(thetarad1))
!===============after reflection======================================================
d2=sqrt((xp-f2x)**2+(yp-f2y)**2)
t2=d2/v
t2=t2+t1
n2=abs(int((t2-t0)/dt))
slope2=(f2y-yp)/(f2x-xp)
thetarad2=atan(slope2)
vx2=abs(v*cos(thetarad2))
vy2=abs(v*sin(thetarad2))
!======================================================================================

if(xp>f1x .and. yp>f1y)then
q1=1
else if (xp<f1x .and. yp>f1y)then
q1=2
else if (xp<f1x .and. yp<f1y)then
q1=3
else if (xp>f1x .and. yp<f1y)then
q1=4
end if


if(f2x>xp .and. f2y>yp)then
q2=1
else if (f2x<xp .and. f2y>yp)then
q2=2
else if (f2x<xp .and. f2y<yp)then
q2=3
else if (f2x>xp .and. f2y<yp)then
q2=4
end if


print*,"slope1= ",slope1,"slope2= ",slope2
print*,"q1= ",q1,"q2= ",q2,"n1= ",n1,"n2= ",n2

open(1,file="carrom20.dat")
t=t0
do i=0,n2

if(i<=n1 .and. q1==1)then
t=i*dt+t0
x=f1x+vx1*t
y=f1y+vy1*t
else if(i<=n1 .and. q1==2)then
t=i*dt+t0
x=f1x-vx1*t
y=f1y+vy1*t
else if(i<=n1 .and. q1==3)then
t=i*dt+t0
x=f1x-vx1*t
y=f1y-vy1*t
else if(i<=n1 .and. q1==4)then
t=i*dt+t0
x=f1x+vx1*t
y=f1y-vy1*t
!=========================================================
else if(i>n1 .and. q2==1)then
t=(i-n1)*dt+t0
x=xp+vx2*t
y=yp+vy2*t
else if(i>n1 .and. q2==2)then
t=(i-n1)*dt+t0
x=xp-vx2*t
y=yp+vy2*t
else if(i>n1 .and. q2==3)then
t=(i-n1)*dt+t0
x=xp-vx2*t
y=yp-vy2*t
else if(i>n1 .and. q2==4)then
t=(i-n1)*dt+t0
x=xp+vx2*t
y=yp-vy2*t
!============================================================
end if
write(1,*)x,y

end do

end program
