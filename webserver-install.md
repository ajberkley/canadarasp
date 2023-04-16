### Setting up web server

Create a t3.micro with a 12GB boot disk (gp3), and a 140 GB storage volume (gp3) using latest Ubuntu name it WEB PROD V10 or some such.
You will need to update canadarasp/continental-test/webserver-ip.sh with that name.

The 140 GB storage volume should be mounted on /mnt with an XFS filesystem (lots of small files, XFS works well)

```
sudo mkfs.xfs /dev/`lsblk | grep 140G | awk '{print $1}'`
sudo mount /dev/nvme1n1 /mnt
```

add a line like
```
sudo mount /dev/disk/by-id/nvme-Amazon_Elastic_Block_Store_vol08852e6f4295be9e1 /mnt
```
to /etc/rc.local

Grab the canadarasp repo

```
git clone https://github.com/ajberkley/canadarasp
```

I like to make sure that my prompt looks different from the normal production system, so I modify ~/.bashrc to have
```
PS1='\[\033[01;32m\]\u@dev-webserver\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
```

I use apache2 as the base web server for static files.  No particular reason except I have a historical connection.

```
sudo apt-get update
sudo apt install apache2
```

The main configuration file is in canadarasp/config-files/webserver-apache2.conf.txt.  Copy that to /etc/apache2/apache2.conf
Enable proxy and proxy\_http and ssl:
```
sudo cp ~/canadarasp/config-files/webserver-apache2.conf.txt /etc/apache2/apache2.conf
sudo cp ~/canadarasp/config-files/000*.conf /etc/apache2/sites-enabled/
sudo cp ~/canadarasp/config-files/001*.conf /etc/apache2/sites-enabled/
sudo a2enmod proxy proxy_http ssl header
sudo a2ensite 000-default.conf
sudo a2ensite 001-airspace.conf
ln -s /home/ubuntu/canadarasp/web-server /home/ubuntu/html
ln -s /home/ubuntu/canadarasp/web-server/RASPtable.html /home/ubuntu/html/RASP.html
mkdir /mnt/map-pngs
mkdir /mnt/windgram-tiles
mkdir /mnt/windgrams-data
ln -s /mnt/windgram-tiles /home/ubuntu/canadarasp/web-server/windgram-tiles
ln -s /mnt/windgrams-data /home/ubuntu/canadarasp/web-server/windgrams-data
ln -s /mnt/map-pngs /home/ubuntu/canadarasp/web-server/map-pngs
ln -s /home/ubuntu/canadarasp/web-server/RASPtable.html /home/ubuntu/html/RASP.html
sudo chmod o+x /home/ubuntu
sudo systemctl restart apache2
```
To get our certificates we will want certbot installed.
```
sudo apt install certbot python3-certbot-apache
sudo certbot --apache
```

We need some swap space on the web server because of dynamic windgram generation.  6 GB is more than enough.
```
sudo dd if=/dev/zero of=/mnt/swapfile bs=65536 count=100000
sudo chmod 0600 /mnt/swapfile
sudo mkswap /mnt/swapfile
sudo swapon /mnt/swapfile
```

Copy the start-up files (THIS IS NOT COMPLETE, NEED PERMS)
```
sudo cp ~/canadarasp/config-files/webserver-rc.local /etc/rc.local
```

Install some libraries... not necessary except for debugging
```
sudo apt install libgdal-dev
sudo apt install gdal-bin
sudo ln -s /usr/bin/gdalwarp /usr/local/bin/gdalwarp
sudo ln -s /usr/bin/gdal_translate /usr/local/bin/gdal_translate
# NEED WGRIB2 ALSO ... 
```

Install all our lisp stuff
```
sudo apt install sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
canadarasp/setup-lisp.sh
sudo apt install emacs
sudo apt install slime
```

```
sudo apt install parallel # used by our shell scripts a lot
sudo apt install default-jre # used by our timezone server
cd ~; ln -s canadarasp/continental-test .
sudo apt install unzip
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install
sudo apt install jq
```
You should now be able to run canadarasp/continental-test/run-timezone-server.sh and continental-test/plot-generation/start-webserver.sh
You need to configure AWS credentials for the AWS command line tool
```
aws configure # I use a user with EC2 full access, obviously it would be nice to trim this, but...
```
You should now be able to run canadarasp/continental-test/webserver-ip.sh

Copy canadarasp/config-files/webserver-rc.local to /etc/rc.local and modify the disk (120G or so) you want to put on /mnt

Install wgrib2 (just copy it from the old webserver or the computer server, or compile it with openjpeg or jasper).  Put in /usr/bin (only needed if serving surface-winds)

Also you want to setup git so you can easily push / pull from the repo

Now you want to test and see if it can download data from ECCC.  We will use the GDPS because it's the smallest data set for testing.  Looking at
canadarasp/config-files/webserver-crontab.txt we find:
```
/home/ubuntu/canadarasp/aws-utils/download-data-and-start-server.sh gdps
```

This will not upload the files back to this test webserver as it is set by name to talk back to the original webserver (see canadrasp/continental-test/webserver-ip.sh).
Keep in mind the compute server and the webserver have to live in the same sub-region (eg. ca-central-1a)

### Other

Finish certbot install instructions
You need to share a key between the compute server and the webserver so the compute server can upload data.
You need to setup the airspace user and viewer for Peter Spear and get a let's encrypt key for airspace.canadarasp.com

