;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
(in-package :cl-user)

(consfigurator:defpackage-consfig :project-isidore/infrastructure
  (:use #:cl #:alexandria #:consfigurator)
  (:export #:deploy-to-production))

(in-package :project-isidore/infrastructure)

(in-consfig "project-isidore/infrastructure")

(named-readtables:in-readtable :consfigurator)

(defun deploy-to-production ()
  "In the future, do some validation that hosts are configured properly."
  (oci-a1-flex))

;;; =================================
;;; 0. CONSFIGURATOR DATA
;;; =================================
(defparameter *linux-production-dir* "/usr/local/src/"
  "Project Isidore parent pathname on GNU/Linux hosts.")

(defparameter *prod-ql-dist-ver* "2022-04-01"
  "Production Quicklisp Distribution Version passed as an ENV variable
'QL_DIST_VER', which is defined in MAKE.LISP")

;;; Snapshot our local copy of Project Isidore as a tar archive...
(try-register-data-source :git-snapshot
                          :name (asdf:primary-system-name "project-isidore")
                          :repo (asdf:system-source-directory "project-isidore")
                          :depth 1 :branch "master")

(defhost oci-a1-flex (:deploy ((:ssh :user "root") :sbcl))
  "Web and file server. Consfigurator, while a general implementation, only works
with Debian GNU/Linux.

You need three things.

1. Remote Root SSH login access as defined locally in ~/.ssh/config

Host oci-a1-flex
  User root
  HostName 140.923.943.123
  IdentityFile ~/.ssh/oracle-vm-key
  ControlPath ~/.ssh/%r@%h:%p
  ControlMaster auto
  ControlPersist yes

2. Origin Certificate Authority private key.

Instead of let's encrypt we generate a certificate and certificate signing
request with Cloudflare. See Nginx directive ssl_certificate_key as configured
below. Now Cloudflare can verify our host's identity.

https://developers.cloudflare.com/ssl/origin-configuration/origin-ca

This host is running on VM.Standard.A1.Flex
https://amperecomputing.com/processors/ampere-altra/ compute as provided by
Oracle Cloud Infrastructure. Can SSH into server after deployment for quick
sanity check. systemctl list-units --type=service --state=running

Server specifications:

$ lscpu

Architecture:                    aarch64
CPU(s):                          4
Vendor ID:                       ARM
Model name:                      Neoverse-N1

$ lsmem

Total online memory:      24G

$ lsblk

NAME    MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
sda       8:0    0   200G  0 disk
├─sda1    8:1    0 199.9G  0 part /
└─sda15   8:15   0   127M  0 part /boot/efi "

  ;;; =================================
  ;;; I. DEBIAN GNU/LINUX CONFIGURATION
  ;;; =================================

  (os:debian-stable "bullseye" :aarch64)
  (timezone:configured "America/Toronto")

  ;; Debian uses the Advanced Package Tool (apt) to manage software.
  (apt:standard-sources.list)
  (apt:updated)
  (apt:upgraded)

  ;; https://cloud.google.com/blog/products/networking/tcp-bbr-congestion-
  ;; control-comes-to-gcp-your-internet-just-got-faster

  ;; https://dl.acm.org/doi/pdf/10.1145/3012426.3022184
  (file:contains-lines
   "/etc/sysctl.conf"
   "net.core.default_qdisc=fq"
   "net.ipv4.tcp_congestion_control=bbr"
   "net.core.somaxconn=65536"
   "net.core.net_dev_max_backlog=250000"
   "net.ipv4.ip_local_port_range=\"32768 60999\""
   "net.ipv4.tcp_rmem=\"4096 87380 4194304\""
   "net.core.rmem_max=4194304"
   "net.ipv4.tcp_wmem=\"4096 65536 4194304\""
   "net.core.wmem_max=4194304"
   "net.ipv4.tcp_timestamps=0"
   "net.ipv4.tcp_tw_reuse=1"
   "net.ipv4.tcp_slow_start_after_idle=0"
   "net.ipv4.tcp_mtu_probing=1")

  ;; For filtering of IP packets in general on Linux, know that iptables have
  ;; been deprecated in favour of nftables. firewalld is a client that uses the
  ;; latter as the backend. Allow incoming traffic through the default HTTPS
  ;; port 443.
  (firewalld:zone-has-service "public" "https")

  ;; Bootstrap production compiler.
  (apt:installed "build-essential" "libzstd-dev")
  ;; `git:cloned' applies `delete-remote-trees' without apparent effect.
  (cmd:single "rm -rf /usr/local/src/sbcl/")
  (git:cloned "https://github.com/sbcl/sbcl.git"
              (pathname "/usr/local/src/sbcl/")
              *prod-sbcl-ver*)
  ;; TODO Validate successful build.
  (cmd:single (concatenate 'string
                           "cd /usr/local/src/sbcl/ "
                           "&& "
                           "sh make.sh "
                           "--xc-host='/usr/bin/sbcl' "
                           "--dynamic-space-size='2Gb' "
                           ;; Turn on SB-CORE-COMPRESSION, adds zstd as
                           ;; build-dependency.
                           "--fancy "
                           "&& "
                           "sh install.sh"))

  ;;; ===========================
  ;;; II. APPLICATION COMPILATION
  ;;; ===========================
  ;; Extract git snapshot on production to `*linux-production-dir*'.
  (git:snapshot-extracted *linux-production-dir*
                          (asdf:primary-system-name "project-isidore") :replace t)

  ;; Because the remote Lisp uses the root user, know that $ sbcl and $ sudo
  ;; sbcl have different initialization files. This is because sudo sbcl uses
  ;; the /root/ instead of /home/debian.
  ;; http://www.sbcl.org/manual/#Initialization-Files
  (cmd:single :env (list :BUILD_DIR
                         (concatenate 'string
                                      *linux-production-dir*
                                      (asdf:primary-system-name "project-isidore"))
                         :QL_DIST_VER *prod-ql-dist-ver*)
              (concatenate 'string
                           ;; NOTE /usr/local/bin/sbcl used instead of
                           ;; /usr/bin/sbcl "sudo apt install sbcl" binary is
                           ;; located at /usr/bin/sbcl. SBCL compiled from
                           ;; source is located at /usr/local/bin/sbcl.
                           "/usr/local/bin/sbcl"
                           " "
                           ;; NOTE Use --script and not --load.
                           ;; http://www.sbcl.org/manual/#Toplevel-Options
                           "--dynamic-space-size '2048' --control-stack-size '10' --script '"
                           *linux-production-dir*
                           (asdf:primary-system-name "project-isidore")
                           "/make.lisp'"))

  ;;; ===========================================================
  ;;; III. APPLICATION INTEGRATION IN OS SYSTEM & SERVICE MANAGER
  ;;; ===========================================================

  ;; Define Project Isidore as a systemd service. The file describes a generic
  ;; unit service that can be instantiated multiple times. Include a systemd
  ;; specifier "%i" to represent the environment variable PORT that a particular
  ;; web server will be bound to.
  ;;
  ;; https://www.linode.com/docs/guides/what-is-systemd/

  ;; https://systemd-by-example.com/

  ;; https://stackoverflow.com/questions/38570366/can-i-run-multiple-processes-
  ;; each-with-different-port-using-systemd

  ;; https://unix.stackexchange.com/questions/323914/dynamic-variables-in-
  ;; systemd-service-unit-files

  (file:has-content
   "/etc/systemd/system/project-isidore@.service"
   "
[Unit]
Description=P.I. web application on port %i.
After=network.target

[Service]
Type=simple
User=root
Group=root
WorkingDirectory=/usr/local/src/project-isidore/
Environment=PORT=%i
ExecStart=/usr/local/src/project-isidore/bin/ProjectIsidore
Restart=always

[Install]
WantedBy=multi-user.target")
  ;; Tell systemd about the file we just created.
  (systemd:daemon-reloaded)
  ;; Check how much RAM is availiable on VM and deploy that many processes?
  (systemd:enabled "project-isidore@8080.service")
  (systemd:enabled "project-isidore@8081.service")
  (systemd:enabled "project-isidore@8082.service")
  (systemd:enabled "project-isidore@8083.service")
  (systemd:enabled "project-isidore@8084.service")
  (systemd:enabled "project-isidore@8085.service")
  (systemd:enabled "project-isidore@8086.service")
  (systemd:enabled "project-isidore@8087.service")
  (systemd:enabled "project-isidore@8088.service")
  (systemd:enabled "project-isidore@8089.service")
  (systemd:enabled "project-isidore@8090.service")
  (systemd:enabled "project-isidore@8091.service")

  ;;; =======================
  ;;; IV. NGINX REVERSE PROXY
  ;;; =======================

  ;; Nginx functions as,

  ;; - Web server for static content: Hunchentoot is a thread-per-request web
  ;; server. Allocate the more expensive Hunchentoot threads for serving dynamic
  ;; content.

  ;; - SSL termination Nginx will be listening on the default HTTPS port, 443.

  ;; - Compression (gzip): reduce bandwidth of server responses to give faster
  ;; network transit. Use Brotli when in the future when if it is included in
  ;; Nginx in the future and does not require a recompile.

  ;; - Reverse Proxy (Load balancing): Nginx will communicate with the
  ;; Hunchentoot server(s) locally. forward requests from
  ;; https://140.291.294.154:443 to http://127.0.0.1:8080 where Hunchentoot
  ;; accepts HTTP requests. Obviate need to distribute CL+SSL (openssl) when
  ;; providing release binaries. Gain service redundancy by having multiple
  ;; Hunchentoot processes.

  ;; https://www.nginx.com/resources/glossary/reverse-proxy-vs-load-balancer/

  ;; https://www.linode.com/docs/guides/getting-started-with-nginx-part-1-
  ;; installation-and-basic-setup/

  ;; https://github.com/denji/nginx-tuning

  (apt:installed "nginx")
  (file:has-content
   "/etc/nginx/conf.d/benedicthanshenwang.com.conf"
   "
# /etc/nginx/sites-available/default can be deleted.

upstream hunchentoot_http_backend {
    server 127.0.0.1:8080;
    server 127.0.0.1:8081;
    server 127.0.0.1:8082;
    server 127.0.0.1:8083;
    server 127.0.0.1:8084;
    server 127.0.0.1:8085;
    server 127.0.0.1:8086;
    server 127.0.0.1:8087;
    server 127.0.0.1:8088;
    server 127.0.0.1:8089;
    server 127.0.0.1:8090;
    server 127.0.0.1:8091;
    keepalive 200;
    keepalive_requests 10000000;
    keepalive_timeout 15m;
}

server {
  ##
  # Basic Settings
  ##
  reset_timedout_connection on;
  server_tokens off;
  access_log off;

  ##
  # Ports
  ##
  listen 443 ssl http2 default_server backlog=65536;
  listen [::]:443 ssl http2 default_server backlog=65536;
  server_name benedicthanshenwang.com www.benedicthanshenwang.com;
  root /home/debian/quicklisp/local-projects/project-isidore;

  ##
  # SSL Settings
  ##
  ssl_certificate        /usr/local/src/project-isidore/assets/benedicthanshenwang.crt;
  ssl_certificate_key    /etc/ssl/private/benedicthanshenwang.key;
  ssl_dhparam            /usr/local/src/project-isidore/assets/dhparam4096.pem;
  ssl_client_certificate /usr/local/src/project-isidore/assets/authenticated_origin_pull_ca.pem;
  ssl_verify_client   on;
  ssl_protocols       TLSv1.2 TLSv1.3;
  ssl_ciphers         EECDH+AESGCM:EDH+AESGCM:AES256+EECDH:AES256+EDH;
  ssl_session_cache   shared:SSL:10m;
  ssl_session_timeout 10m;

  ##
  # Gzip Settings
  ##
  # No universal gzip compression due to CRIME and BREACH exploits.
  gzip          on;
  gzip_types    text/plain text/css image/*;

  ##
  # HTTP Response Header Fields
  ##
  add_header X-Content-Type-Options nosniff;
  add_header X-Frame-Options SAMEORIGIN;
  add_header X-XSS-Protection \"1; mode=block\";
  add_header Referrer-Policy strict-origin-when-cross-origin;
  add_header Content-Security-Policy \"upgrade-insecure-requests;\";
  add_header Feature-Policy \"encrypted-media 'self'; autoplay 'none'\";

  sendfile on;
  tcp_nopush on;
  tcp_nodelay on;
  aio threads;
  open_file_cache max=1000000 inactive=3d;
  open_file_cache_valid 1d;

  ##
  # Hunchentoot threads are expensive; use Nginx to serve static assets.
  ##
  location /assets/ {
    # Use the root defined under # Ports.
    add_header Cache-Control \"max-age=86400, must-revalidate\";
  }

  location / {
    proxy_pass http://hunchentoot_http_backend;
    proxy_http_version 1.1;
    proxy_set_header \"Connection\" \"\";
    # Cloudflare Edge Free TTL is 2 hours.
    add_header Cache-Control \"max-age=7200, must-revalidate\";
  }
}
")
  (systemd:daemon-reloaded)
  (systemd:enabled "nginx.service")
  (reboot:rebooted-at-end))
