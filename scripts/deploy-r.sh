#!/bin/bash
# @author Ashish Sahoo (ashissah@in.ibm.com)
############################################################################
#Environment Variables                                                     #
############################################################################
#
export port_range=1240,8000
############################################################################
#                 DOCKER HUB IMAGE PUSH                                    #
############################################################################
echo "Docker Push Images"
docker push "$DOCKER_USERNAME"/$git_repo:$TRAVIS_BRANCH-$DEPLOY_TIMESTAMP-$TRAVIS_BUILD_NUMBER
#docker push "$DOCKER_USERNAME"/$git_repo:latest
############################################################################
# Log into the IBM Cloud environment using apikey                          #
############################################################################
echo "Login to IBM Cloud using apikey"
ibmcloud login -a https://cloud.ibm.com --apikey @./scripts/apikey.json -r us-south
if [ $? -ne 0 ]; then
  echo "Failed to authenticate to IBM Cloud"
  exit 1
fi
############################################################################
# Log into the IBM Cloud container registry                                #
############################################################################
#SGecho "Logging into IBM Cloud container registry"
#SGibmcloud cr login
#SGif [ $? -ne 0 ]; then
#SG  echo "Failed to authenticate to IBM Cloud container registry"
#SG  exit 1
#SGfi
############################################################################
# If the image exists in the container registry then delete it             #
# then recreate it                                                         #
############################################################################
#SGecho "looking to see if the name-space exists"
#SGibmcloud cr namespace-list | grep "$icp_name"
#SGif [ $? -ne 0 ]; then
#SG  echo "Name-space not exist in IBM Cloud container registry, Adding them now"
#SG  ibmcloud cr namespace-add "$icp_name"
#SGelse
#SG  echo "Name-space exist in IBM Cloud container registry, Deleting and Adding them now"
#SG  ibmcloud cr namespace-rm "$icp_name" -f
#SG  ibmcloud cr namespace-add "$icp_name"
#SGfi
############################################################################
# Build image with dockerfile in Cloud Registry                            #
# It can be either from IBM Cloud or Docker, use accordingly               #
############################################################################
# ibmcloud cr image-rm us.icr.io/"$icp_name"/"$git_repo"
# ibmcloud cr image-list
############################################################################
# Log into the IBM Cloud container registry                                #
############################################################################
#SGecho "Logging into IBM Cloud container registry"
#SGibmcloud cr login
#SGif [ $? -ne 0 ]; then
#SG  echo "Failed to authenticate to IBM Cloud container registry"
#SG  exit 1
#SGfi
#ibmcloud cr build --tag us.icr.io/"$icp_name"/"$git_repo" ./
#SGdocker tag $git_repo us.icr.io/"$icp_name"/"$git_repo"
#SGdocker push us.icr.io/"$icp_name"/"$git_repo"
############################################################################
# Start the deployment details using kubectl                               #
############################################################################
ibmcloud ks cluster config --cluster zcluster1
#kubectl config current-context
#ibmcloud ks cluster config zcluster1

echo 'Deleting the deployment' $git_repo
kubectl delete deployment "$git_repo" 
kubectl delete service "$git_repo"
#kubectl delete pod "$git_repo"
echo 'Delete Old Deployment End'

#
#SGif [ $? -ne 0 ]; then
#SG  echo "Deployment does not exist in IBM Cloud container registry, Adding them now"
#SG  kubectl create deployment $git_repo --image=us.icr.io/"$icp_name"/"$git_repo" 
#SG  kubectl run $git_repo --image=us.icr.io/"$icp_name"/"$git_repo"
#SGelse
#SG  echo "Deployment does exist in IBM Cloud container registry, Adding them now as deleted"
#SG  kubectl create deployment $git_repo --image=us.icr.io/"$icp_name"/"$git_repo" 
#SG  echo "Run Deployment Start"
#SG  kubectl run $git_repo --image=us.icr.io/"$icp_name"/"$git_repo"
#SG  echo "Run Deployment Ends"
#SGfi
############################################################################
# Start the Service deployment details using kubectl                       #
############################################################################

   
   echo 'Create Deployment Start docker.io/'"$DOCKER_USERNAME"'/'$git_repo':'$TRAVIS_BRANCH'-'$DEPLOY_TIMESTAMP'-'$TRAVIS_BUILD_NUMBER
   #kubectl create deployment $git_repo --image="$DOCKER_USERNAME"/$git_repo:latest
   #kubectl run $git_repo --image="$DOCKER_USERNAME"/$git_repo:latest
   kubectl create deployment $git_repo --image="$DOCKER_USERNAME"/$git_repo:$TRAVIS_BRANCH-$DEPLOY_TIMESTAMP-$TRAVIS_BUILD_NUMBER
   kubectl run $git_repo --image="$DOCKER_USERNAME"/$git_repo:$TRAVIS_BRANCH-$DEPLOY_TIMESTAMP-$TRAVIS_BUILD_NUMBER

   echo 'Create Deployment End'
#
  echo "Run Deployment/Service Start"
  kubectl expose deployment/"$git_repo" --type=NodePort --name="$git_repo" --port="$port_range"
  echo "Run Deployment/Service End"
############################################################################
# Get details of the Service deployment  kubectl                           #
############################################################################
  #ibmcloud ks cluster ls
  #kubectl describe deployments x86-r-z-appln
  ibmcloud ks worker ls --cluster zcluster1
  kubectl describe service "$git_repo"
############################################################################
# Create Short url from Node/Node-port                                     #
############################################################################
  # NODEPORT=$(kubectl get -o jsonpath="{.spec.ports[0].nodePort}" services "$git_repo"-node)
  # NODES=$(kubectl get nodes -o jsonpath='{ $.items[*].status.addresses[?(@.type=="ExternalIP")].address }')
  # echo $NODES
  # echo $NODEPORT
  # url=http://"$NODES":"$NODEPORT"
  # short_url=$(curl -s http://tinyurl.com/api-create.php?url=${url})
  # echo "Short URL is : ${short_url}"
#
