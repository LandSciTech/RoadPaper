#!/bin/bash
#############################################
#####Login etc##################

az login
az batch account login -g EcDc-WLS-rg -n ecdcwlsbatch
az batch pool list  \
--query "[].{id:id, curNodes: currentDedicatedNodes,tarNodes: targetDedicatedNodes, allocState:allocationState}" \
--output yaml

#### Set ids and names ##############
subnetid=$(az network vnet subnet list --resource-group EcDc-WLS-rg --vnet-name EcDc-WLS-vnet \
--query "[?name=='EcDc-WLS_compute-cluster-snet'].id" --output tsv)

jobName="sendicott_job_roads"

poolName="sendicott_roads"

end=`date -u -d "7 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`

sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

ghpat=`Rscript -e "cat(gh::gh_token())"`

### Prepare files for each task

# Replace placeholders for secrets in json file
# Using , as delimter for sed to work with url
sed 's,<subnetId>,'${subnetid//&/\\&}',g' analysis/cloud/pool_roads.json\
| sed 's,<id>,'${poolName}',g'> analysis/cloud/pool_to_use.json

# make a different json for each task
mkdir -p analysis/cloud/task_jsons

for rowi in {25..27}
do
  sed 's,<SASURL>,'${sasurl//&/\\&}',g' analysis/cloud/task_roads.json\
  | sed 's,<row>,'$rowi',g' > analysis/cloud/task_jsons/task_roads_$rowi.json
done

sed 's,<pat>,'$ghpat',g' analysis/cloud/make.R > analysis/cloud/make_to_use.R

# files for combine
sed 's,<subnetId>,'${subnetid//&/\\&}',g' analysis/cloud/pool_combine.json\
| sed 's,<id>,'${poolName}',g'> analysis/cloud/pool_to_use.json

for rowi in real notreal real1000
do
  sed 's,<SASURL>,'${sasurl//&/\\&}',g' analysis/cloud/task_combine.json\
  | sed 's,<row>,'$rowi',g' > analysis/cloud/task_jsons/task_combine_$rowi.json
done

sed 's,<pat>,'$ghpat',g' analysis/cloud/make_combine.R > analysis/cloud/make_to_use.R

#### Move files to container ##############
# First check container is empty. All files in container will be copied to nodes
az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken\
 --query "[].{name:name}" --output yaml

az storage copy -d $sasurl -s analysis/cloud/make_to_use.R

az storage copy -d $sasurl -s analysis/data/derived_data/cutblocks_revelstoke_real.gpkg
az storage copy -d $sasurl -s analysis/data/raw_data/cutblocks_revelstoke.gpkg
az storage copy -d $sasurl -s analysis/data/derived_data/combined_revelstoke_roads.gpkg
az storage copy -d $sasurl -s analysis/data/raw_data/tsa27_boundaries.gpkg
az storage copy -d $sasurl -s analysis/data/derived_data/TSA27/dem_revelstoke_10.tif

az storage copy -d $sasurl -s analysis/scripts/6_benchmark_methods.R
az storage copy -d $sasurl -s R/functionsSource.R

# files for combine task
az storage copy -d $sasurl -s analysis/data/derived_data/TSA27/dem_revelstokeCoarse.tif

az storage copy -d $sasurl -s analysis/scripts/7_compile_results_cloud_notreal.R
az storage copy -d $sasurl -s analysis/scripts/7_compile_results_cloud_real.R
az storage copy -d $sasurl -s analysis/scripts/7_compile_results_cloud_real1000.R

az storage copy -d $sasurl -s analysis/data/derived_data/TSA27_real_cuts/klementProjection.shp
az storage copy -d $sasurl -s analysis/data/derived_data/TSA27_real_cuts/klementProjection.shx
az storage copy -d $sasurl -s analysis/data/derived_data/TSA27_real_cuts/klementProjection.dbf
az storage copy -d $sasurl -s analysis/data/derived_data/TSA27_real_cuts/klementProjection.prj
#### Create pool, job, tasks ##########################
az batch pool create --json-file analysis/cloud/pool_to_use.json
az batch job create --pool-id $poolName --id $jobName


for rowi in {25..27}
do
  az batch task create --json-file analysis/cloud/task_jsons/task_roads_$rowi.json --job-id $jobName
done

# tasks for combine
for rowi in real1000 real notreal
do
  az batch task create --json-file analysis/cloud/task_jsons/task_combine_$rowi.json --job-id $jobName
done

# az batch task delete --task-id connectivity-combine --job-id $jobName --yes
#
# for rowi in real1000 real notreal
# do
#   az batch task delete --job-id $jobName --task-id roads-benchmark-$rowi --yes
# done

# set target dedicated nodes
az batch pool resize --pool-id $poolName --target-dedicated-nodes 3

# prompt auto scaleing of pool by changing time interval
# enabling this as soon as the tasks are created seems to make it think there are no tasks
az batch pool autoscale enable --pool-id $poolName --auto-scale-evaluation-interval "PT5M"\
 --auto-scale-formula 'percentage = 70;
 span = TimeInterval_Second * 15;
 $samples = $ActiveTasks.GetSamplePercent(span);
 $tasks = $samples < percentage ? max(0,$ActiveTasks.GetSample(1)) : max( $ActiveTasks.GetSample(1), avg($ActiveTasks.GetSample(span)));
 multiplier = 1;
 $cores = $TargetDedicatedNodes;
 $extraVMs = (($tasks - $cores) + 0) * multiplier;
 $targetVMs = ($TargetDedicatedNodes + $extraVMs);
 $TargetDedicatedNodes = max(0, min($targetVMs, 50));
 $NodeDeallocationOption = taskcompletion;'

# check pool node counts
az batch pool list  \
--query "[].{id:id, curNodes: currentDedicatedNodes,tarNodes: targetDedicatedNodes, allocState:allocationState}" \
--output yaml

# check node state
az batch node list --pool-id $poolName --query "{nodes: [].[id, state][]}" --output json

#### Monitor tasks ############################

# details for a single task filtered by query
az batch task show --job-id $jobName --task-id roads-benchmark-14 \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# az batch task delete --job-id $jobName --task-id roads-benchmark-1 --yes
# az batch task reactivate --job-id $jobName --task-id roads-benchmark-1

# download output file for a task
az batch task file download --task-id roads-benchmark-14 --job-id $jobName --file-path "stdout.txt" --destination "analysis/cloud/stdout.txt"

# List of all tasks and their state
# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id $jobName --query "{tasks: [].[id, state][]}" --output json

# list running tasks only
az batch task list --job-id $jobName --query "{tasks: [].[id, state][]}" --output json --filter "state eq 'running'"

# Summary of task counts by state
az batch job task-counts show --job-id $jobName

# Check what results have been added to the storage container
az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --output yaml

#### Download results and remove from storage ################################
az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/*?$sastoken \
-d "analysis/data/derived_data/bench_results" --include-pattern "*.rds"

az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/\$AZ_BATCH_TASK_ID?$sastoken \
-d "analysis/data/derived_data/cloud_combine_results" --recursive

az storage remove -c sendicott -n \$AZ_BATCH_TASK_ID --account-name ecdcwls --sas-token $sastoken --recursive

# Remove just files matching pattern
az storage remove -c sendicott --include-pattern "*.txt" --account-name ecdcwls --sas-token $sastoken --recursive

# NOTE removes ***everything*** from the storage container
az storage remove -c sendicott --exclude-pattern "*.gpkg" --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName
az batch pool delete --pool-id $poolName

# remove temporary cloud files
rm analysis/cloud/make_to_use.R
rm analysis/cloud/pool_to_use.json
rm analysis/cloud/task_jsons --recursive
