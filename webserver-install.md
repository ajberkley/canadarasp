### Setting up web server

Create a t3.micro with a 12GB boot disk (gp3), and a 140 GB storage volume (gp3) using latest Ubuntu name it WEB PROD V10 or some such.
You will need to update canadarasp/continental-test/webserver-ip.sh with that name.

The 140 GB storage volume should be mounted on /mnt with an XFS filesystem (lots of small files, XFS works well)

```
sudo mkfs.xfs /dev/`lsblk | grep 140G | awk '{print $1}'`
sudo mount /dev/nvme1n1 /mnt
```

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
sudo a2enmod proxy proxy_http ssl
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
### Other

Finish certbot install instructions
You need to share a key between the compute server and the webserver so the compute server can upload data.

