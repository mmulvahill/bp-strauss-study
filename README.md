# bp-strauss-study

A refactored, more reproducible version of my thesis work for reference in published paper.

```
sudo apt-get remove docker docker-engine docker.io
sudo apt-get update
sudo apt-get install \
       apt-transport-https \
       ca-certificates \
       curl \
       software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

# Note, if using Linux Mint, need the Ubuntu lsb_release name not LM's
sudo add-apt-repository \
       "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
       $(lsb_release -cs) \
       stable"
sudo apt-get update
sudo apt-get install docker-ce
# View versions available
apt-cache madison docker-ce
# Using 17.06.0~ce-0~ubuntu for this project
sudo apt-get install docker-ce=17.06.0~ce-0~ubuntu
sudo docker run hello-world

docker build -t bp-strauss-study .
```
