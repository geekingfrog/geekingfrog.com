---
title: SVN server
status: published
tags:
- svn
---

Here is a memo of how to set up and use an svn server with apache and Eclipse. The main instructions I followed are <a href="http://www.howtoforge.com/debian_subversion_websvn">there</a>.

The goal is to have a svn repository on my server, with an url so that anyone with proper access can see the content of the project. SVN is used over SSH for security and confidentiality reasons. The last step is to be able to use the Eclipse SVN plugin, this part was the trickiest one.

First, set up an ssh agent. See the previous post for a quick how-to. This will avoid the hassle to type the password (at least) twice evry time one wants to commit/checkout/update etc etc...

Then, follow <a href="http://www.howtoforge.com/debian_subversion_websvn">these instructions</a> to set up the svn repository, set the right permission, manage the users and install dav_svn. A <a href="http://svn.haxx.se/dev/archive-2004-03/0253.shtml">nice addition</a> is to change the visible root directory so users cannot guess where is the repos on the server. A key point here is to create a wrapper script to start svnserve. This script change the root for the user and set the right umask (002).
From here, one should be able to list, commit and so on using a command line like :
<pre class="brush:shell">svn list svn+ssh://user_name@url.to.server/repos_folder</pre>
The last step is to install and configure the Eclipse SVN plugin. The <a href="http://www.ibm.com/developerworks/opensource/library/os-ecl-subversion/">main part</a> is not complicated. The problem is (again) to set the right permissions. Because the plugin uses the protocol http, one must give apache the right permissions to access the repos. Ususally, the apache user is www-data. So one has to add this user to the group which owns the repos (subversion in my case) :
<pre class="brush:shell">usermod -a -G subversion www-data</pre>
Then, set the right umask. The apache umask can be added in the /etc/apache2/envvars file (I'm running Debian Lenny 5.0.8). In the plugin eclipse, when one has to add a repos, use the url defined in the /etc/apache2/mods-available/dav_svn.conf.

And that's all. To allow a user to browse the project and participate, add this user to the group subversion and everything worrks just fine. Still to do: allow anonymous access for anyone who wants to read the sources of the project.