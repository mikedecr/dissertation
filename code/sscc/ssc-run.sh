
# --- sign into Linstat ---
ssh -Y decrescenzo@linstat.ssc.wisc.edu

# enter pw

# --- Working in Linstat interactively

zsh

cd dissertation

git status
git pull

git log
q

# R

# to do
# save out and error to different places
# map drive to mac (KB, remote computing, map network disk space from a mac) 
# run short, short in background, long in background

# runs simulation in batch, saves output to log file
# R < code/02-dgirt/22-sim/sim-static.R > lkj-test-2k.log --no-save &
# R < code/02-dgirt/24-estimate/241-run-static.R > lkj-real-data.log --no-save &
R < code/04-positioning/42-model/421_sequential-g.R > logs/sequential-g_slimmer.log --no-save &
# Rscript --no-save code/02-dgirt/24-estimate/241-run-static.R > lkj-real-data.log &
# 195403 (2020-01-13, 12:55pm)

# condor_R code/02-dgirt/22-sim/sim-static.R lkj-test-output.log &


# log file
vim logs/sequential-g.log


# checking open processes
ps
ps aux | grep decres

top

# if testing
# Rscript code/02-dgirt/22-sim/long-data-sim.R
