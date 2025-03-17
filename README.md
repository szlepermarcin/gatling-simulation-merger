# Scaling out with Gatling open-source

Gatling open-source doesn't have a cluster mode, but you can achieve similar results manually. You will have to configure all your load injectors, and aggregate the results manually. The steps are:

* deploy Gatling on several machines along with the Simulation classes and the associated resources (data, bodies, etc...)
* launch them remotely from a script, with the `-nr` (no reports) option
* retrieve all the simulation.log files
* rename them so they don't clash
* place them into one folder
* run simulation merger (this script) `scala-cli . -- -id /path/to/simulations -of path/to/result/simulation.log`
* generate the reports with Gatling with the `-ro path/to/result/simulation/folder  -rf path/to/result/report/folder` (reports only)


tested on Gatling `3.13.4`
