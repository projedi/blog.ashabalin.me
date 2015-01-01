## A server setup

* I use rsync to copy the _site directory directly to the server. Exact command is in deploy.sh
* Relevant nginx.conf snippet:
```
server {
   listen 80;
   server_name blog.ashabalin.me;
   root /srv/http/blog;

   location / {
      autoindex on;
   }
}
```
