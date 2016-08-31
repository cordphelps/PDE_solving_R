PDE_Heat_Equation <- function()
{
  require(ReacTran)
  require(deSolve)
  N <- 100
  xgrid <- setup.grid.1D(x.up=0,x.down=1,N=N)
  x <- xgrid$x.mid
  D.coeff <- 0.01
  Diffusion <- function(t,Y,parms)
  {
    tran <- tran.1D(C=Y,C.up=0,C.down=1,D=D.coeff,dx=xgrid)
    list(dY=tran$dC,flux.up=tran$flux.up,flux.down=tran$flux.down)
  }
  ## initial condition:
  Yini <- sin(pi*x)
  times <- seq(from=0,to=5,by=0.01)
  print(system.time(out <- ode.1D(y=Yini,times=times,func=Diffusion,parms=NULL,dimens=N)))
  
  ## plot results:
  par(mfrow=c(1,2))
  plot(out[1,2:(N+1)],x,type="l",lwd=2,xlab="Variable, Y",ylab="Distance, x")
  for(i in seq(2,length(times),by=50))
    lines(out[i,2:(N+1)],x)
  image(out,grid=x,mfrow=NULL,ylab="Distance, x",main="Y")
}

