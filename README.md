## A server setup

* A regular (not bare) git repository to which I push from my laptop
* `receive.denyCurrentBranch = ignore` so I can actually push to it
* post-receive git hook:
```
#!/bin/sh
 
# Pretty much stolen from http://chrisdone.com/posts/hakyll-and-git-for-you-blog
 
export LANG=en_US.UTF-8
 
if [ -n $GIT_DIR ]; then
   unset GIT_DIR
   cd ..
fi
git checkout -f
cabal run -- rebuild
```
* Relevant nginx.conf snippet:
```
server {
   listen 80;
   server_name blog.ashabalin.me;
   root /srv/http/blog/_site;

   location / {
      autoindex on;
   }
}
```
