;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
(in-package :cl-user)

;;; If (ql:quickload "consfigurator") fails with CFFI unable to find C libraries,
;;; sudo apt install packages listed in
;;; `consfigurator.property.package:*consfigurator-system-dependencies*'.

(consfigurator:defpackage-consfig :project-isidore/infrastructure
  (:use #:cl #:consfigurator)
  (:export #:deploy-to-production))

(in-package :project-isidore/infrastructure)

(in-consfig "project-isidore/infrastructure")

(named-readtables:in-readtable :consfigurator)

(defun deploy-to-production ()
  " In the future, do some validation that hosts are configured properly. Until
then, read the docstring of the defined host oci-a1-flex.

Inside a LISP REPL,

(ql:quickload \"project-isidore\")
(ql:quickload \"project-isidore/infrastructure\")
(in-package \"project-isidore/infrastructure\")
(deploy-to-production)"
  (oci-a1-flex))

;;; =================================
;;; 0. CONSFIGURATOR DATA
;;; =================================

;;; TIME = `*prod-ql-dist-ver*',`*prod-sbcl-ver*', Project Isidore master
;;; branch, Debian GNU/Linux.

;;; SPACE = `*linux-production-dir*'

;;; MATTER = `oci-a1-flex' server specification.

(defparameter *linux-production-dir*
  (concatenate 'string
               "/usr/local/src/"
               (asdf:primary-system-name "project-isidore")
               "/")
  "Project Isidore production directory STRING on GNU/Linux hosts. See the Linux
Filesystem Hierarchy Standard.")

(defparameter *prod-ql-dist-ver* "2022-07-08"
  "Production Quicklisp Distribution Version STRING passed as an ENV variable
'QL_DIST_VER', which is utilized in MAKE.LISP. Date must have the same format as
given in (ql::available-versions (ql::dist \"quicklisp\"))")

(defparameter *prod-sbcl-ver* "sbcl-2.2.6"
  "Production SBCL Compiler Version STRING. Must have the same format as canonical
SBCL git tags. Use Debian packaged SBCL to bootstrap.")

;;; Snapshot our local copy of Project Isidore and package as tar archive.
(try-register-data-source :git-snapshot
                          :name (asdf:primary-system-name "project-isidore")
                          :repo (asdf:system-source-directory "project-isidore")
                          :depth 1 :branch "master")

;;; Implicit Data - SBCL source code repository is located at
;;; https://github.com/sbcl/sbcl.git

;;; Implicit Data - Quicklisp Library Manager is situation normal.

;;; Because the remote Lisp uses the root user, know that $ sbcl and $ sudo
;;; sbcl have different initialization files. This is because sudo sbcl uses
;;; the /root/ instead of /home/debian.
;;; http://www.sbcl.org/manual/#Initialization-Files
(defhost oci-a1-flex (:deploy ((:ssh :user "root") :sbcl))
  " Currently the only production web and file server. Consfigurator, while easily
extensible, works best with Debian GNU/Linux. It also takes great pains to be
portable (such as using the agnostic lizard library) but this host utilizes SBCL
to compile application code. Consfigurator can produce both disk images and
containers, although in this case I assume the server is initialized with Debian
GNU/Linux as the operating system. Two pieces of information are unique to this
host,

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

  ;; Customize OS settings to better function as a webserver.

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

  ;; Move git snapshot to remote machine.
  (git:snapshot-extracted (asdf:primary-system-name "project-isidore")
                          (uiop:pathname-parent-directory-pathname
                           (pathname *linux-production-dir*))
                          :replace t)

  ;; FIXME This shell command may fail silently and be reported as "done". Add
  ;; better condition handling here.
  (cmd:single :env (list :BUILD_DIR *linux-production-dir*
                         :QL_DIST_VER *prod-ql-dist-ver*)
              (concatenate 'string
                           "cd "
                           *linux-production-dir*
                           " && "
                           ;; NOTE /usr/local/bin/sbcl used instead of
                           ;; /usr/bin/sbcl "sudo apt install sbcl" binary is
                           ;; located at /usr/bin/sbcl. SBCL compiled from
                           ;; source is located at /usr/local/bin/sbcl.
                           "/usr/local/bin/sbcl "
                           ;; KLUDGE "--control-stack-size='4Mb' breaks."
                           "--control-stack-size '4Mb' "
                           ;; NOTE Use --script and not --load.
                           ;; http://www.sbcl.org/manual/#Toplevel-Options
                           "--script '"
                           *linux-production-dir*
                           "make.lisp'"))

  ;;; ===========================================================
  ;;; III. APPLICATION INTEGRATION IN OS SYSTEM & SERVICE MANAGER
  ;;; ===========================================================

  ;; Define Project Isidore as a systemd service. The file describes a generic
  ;; unit service that can be instantiated multiple times. Include a systemd
  ;; specifier "%i" to represent the environment variable PORT that a particular
  ;; Project Isidore web server process will be bound to.
  ;;
  ;; https://www.linode.com/docs/guides/what-is-systemd/

  ;; https://systemd-by-example.com/

  ;; https://stackoverflow.com/questions/38570366/can-i-run-multiple-processes-
  ;; each-with-different-port-using-systemd

  ;; https://unix.stackexchange.com/questions/323914/dynamic-variables-in-
  ;; systemd-service-unit-files

  (file:has-content
   "/etc/systemd/system/project-isidore@.service"
   (concatenate 'string
   "
[Unit]
Description=P.I. web application on port %i.
After=network.target

[Service]
Type=simple
User=root
Group=root
WorkingDirectory="
*linux-production-dir*
"
Environment=PORT=%i
ExecStart="
*linux-production-dir*
;; NOTE Hardcoded binary name.
"bin/ProjectIsidore"
"
Restart=always

[Install]
WantedBy=multi-user.target"))
  ;; Tell systemd about the file we just created.
  (systemd:daemon-reloaded)
  ;; Check how much RAM is available on VM and deploy that many processes?
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
  (systemd:enabled "project-isidore@8092.service")
  (systemd:enabled "project-isidore@8093.service")
  (systemd:enabled "project-isidore@8094.service")
  (systemd:enabled "project-isidore@8095.service")
  (systemd:enabled "project-isidore@8096.service")
  (systemd:enabled "project-isidore@8097.service")
  (systemd:enabled "project-isidore@8098.service")
  (systemd:enabled "project-isidore@8099.service")
  (systemd:enabled "project-isidore@8100.service")
  (systemd:enabled "project-isidore@8101.service")
  (systemd:enabled "project-isidore@8102.service")
  (systemd:enabled "project-isidore@8103.service")
  (systemd:enabled "project-isidore@8104.service")
  (systemd:enabled "project-isidore@8105.service")
  (systemd:enabled "project-isidore@8106.service")
  (systemd:enabled "project-isidore@8107.service")
  (systemd:enabled "project-isidore@8108.service")
  (systemd:enabled "project-isidore@8109.service")
  (systemd:enabled "project-isidore@8110.service")
  (systemd:enabled "project-isidore@8111.service")
  (systemd:enabled "project-isidore@8112.service")
  (systemd:enabled "project-isidore@8113.service")
  (systemd:enabled "project-isidore@8114.service")
  (systemd:enabled "project-isidore@8115.service")
  (systemd:enabled "project-isidore@8116.service")
  (systemd:enabled "project-isidore@8117.service")
  (systemd:enabled "project-isidore@8118.service")
  (systemd:enabled "project-isidore@8119.service")
  (systemd:enabled "project-isidore@8120.service")

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
   "/etc/nginx/conf.d/bhw.name.conf"
   (concatenate 'string
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
    server 127.0.0.1:8092;
    server 127.0.0.1:8093;
    server 127.0.0.1:8094;
    server 127.0.0.1:8095;
    server 127.0.0.1:8096;
    server 127.0.0.1:8097;
    server 127.0.0.1:8098;
    server 127.0.0.1:8099;
    server 127.0.0.1:8100;
    server 127.0.0.1:8101;
    server 127.0.0.1:8102;
    server 127.0.0.1:8103;
    server 127.0.0.1:8104;
    server 127.0.0.1:8105;
    server 127.0.0.1:8106;
    server 127.0.0.1:8107;
    server 127.0.0.1:8108;
    server 127.0.0.1:8109;
    server 127.0.0.1:8110;
    server 127.0.0.1:8111;
    server 127.0.0.1:8112;
    server 127.0.0.1:8113;
    server 127.0.0.1:8114;
    server 127.0.0.1:8115;
    server 127.0.0.1:8116;
    server 127.0.0.1:8117;
    server 127.0.0.1:8118;
    server 127.0.0.1:8119;
    server 127.0.0.1:8120;
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

  ##
  # Ports
  ##
  listen 443 ssl http2 default_server backlog=65536;
  listen [::]:443 ssl http2 default_server backlog=65536;
  # NOTE Hardcoded domain name.
  server_name bhw.name www.bhw.name;
  root "
  *linux-production-dir* ";"
"

  ##
  # SSL Settings
  ##
  ssl_certificate        /etc/ssl/private/bhw.name.pem;
  ssl_certificate_key    /etc/ssl/private/bhw.name.key;
  ssl_dhparam            /etc/ssl/private/dhparam4096.pem;
  ssl_client_certificate /etc/ssl/private/authenticated_origin_pull_ca.pem;
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
"))
  (systemd:daemon-reloaded)
  (systemd:enabled "nginx.service")
  (reboot:at-end))
