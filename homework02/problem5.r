fluid = read.csv("fluid.txt")
plot(voltage,time,main="Voltage Applied vs. Fluid Breakdown Time")
plot(voltage,log(time),main="Voltage vs. Log Breakdown Time")


