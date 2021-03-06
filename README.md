Introduction
============

This is a fairly sophisticated (compared to others) "qauth" script. It
keeps your eggdrop authed to Q on Quakenet, retrieves the bots auth' 
chanlevs from Q & L and requests op/invite/unban himself. Ideally, the 
only thing you'll need to do is set Username + Password and load the script,
then (re)start the bot. 

It will NOT work on networks other than Quakenet. 


Installation Instructions
=========================

1. Download/git-clone qnet.tcl to your scripts directory.

	Only qnet.tcl is needed. If you want easy upgrades, use git to clone the
	repository (optional!):

		$ cd scripts
		$ git-clone git://github.com/elven/eggdrop-quakenet-auth.git

	You do not have to use git. You can just as well download it manually.

2. Edit your eggdrop.conf to include the following:

		set qnet(user) "authname"
		set qnet(pass) "your-password"
		set qnet(usex) 0
		set qnet(usechallenge) 0
		source eggdrop-quakenet-auth/qnet.tcl

	Set usex to 1 to make the eggdrop set +x BEFORE joining any channel.

	Set usechallenge to 1 to use Quakenets CHALLENGEAUTH mechanism. Note
	that this requires the tcl package 'sha1' >= 2.0.0 to be installed.

	On most systems, this is included in the 'tcllib' package; e.g.
	apt-get/aptitude install tcllib

3. Rehash and/or restart the bot.

	If you're rehashing, you might need to do '.qnet auth' on your partyline.
	A restart, however, is recommended.

4. Keeping up-to-date (only applies to git, not for manual download):

		$ cd scripts/qnet
		$ git pull


That should get you started just nicely. If you're having trouble, do not hesitate
to mail me.


Features
========

### Channel Flags

#### +noserviceprotect (default: -)

Stops eggdrop from protecting its channels through Q/L (recover et al)

#### +noserviceop (default: -)

Stops Q from requesting op through the services.

#### +noservicevoice (default: -)

Stops Q from requesting voice through the services.

### Advanced Features

For all advanced features, please have a look at the script itself; there are 
various additional settings you can set in your eggdrop.conf; those are documented
there as well.


FAQ
===

### My bot always keeps messing about with Q and L and moaning about ops/deops.

Make sure Q & L are added as global friends & global ops! Its not necessary,
but recommended to avoid clashes in general.

### I am connecting through a bouncer, and eggdrop keeps trying to authenticate all the time, even though the server connection is already authenticated to Q.

This is a known issue. There is no workaround available, except
setting qnet(authed) to 1, and running qnet:update manually every
time eggdrop connects:

	set qnet(user) ".."
	set qnet(pass) ".."
	set qnet(usex) ".."
	set qnet(authed) 1
	source scripts/qnet.tcl
	qnet:update

### I receive "You're already opped on #channel" on login.

This is a issue with eggdrops need-handling. There is no known fix, ignore it.
