JazzScheme is a programming language and development platform based on Scheme

***
*** This is an alpha release.
***
*** Note: Jazz is just emerging from a year long operation to have its C++ core replaced
*** by a Scheme / Gambit core. This changed the language radically. Consequently, a lot
*** of modules and tools are still broken. Also, the source code is currently a mixup of
*** 'old style' and 'new style'. We are actively refactoring and cleaning up all the source
*** code to use the new features like libraries, tail-call recursion, continuations, generics,
*** type system, ...
***

Jazz is the programming language
Jedi is the IDE


Platforms
=========

Windows, Mac OS X, Linux / X11


Distribution
============

Readme.txt               :  Readme file
Install.txt              :  Installation instructions

bin                      :  Architecture directories
dev                      :  For developers of JazzScheme
doc                      :  Preliminary documentation
include                  :  Include directory for C code
kernel                   :  Scheme code to bootstrap the module system
lib                      :  Lib directory for C code
packages                 :  Installed packages


Architecture
============

JazzScheme uses a directory structure to support cross-compilation accross multiple architectures.
The architecture is determined by 4 features :

- System : the underlying Scheme system (Gambit, Chicken, ...)
- Platform : the GUI platform (Mac, X11, Windows)
- Processor : the target processor (Intel, ...)
- Safety : the desired safety level
  - Core : core debug mode for debugging jazzscheme itself with tests to make the core safe
  - Debug : standard debug mode with tests to make user code safe (recommanded safety for development)
  - Release : release mode for stable user code with no safety tests

Each architecture has an associated subdirectory in the bin directory. This directory will contain
any architecture specific files: executables, compilation result files, libraries, ...


Launching
=========

At this time there is no executable to launch. Launching the underlying Scheme system from the desired
architecture directory plays that role. In Gambit's case, we rely on a Gambit feature where Gambit will
load any gambcini.scm file located in the current directory. Each architecture directory then contains
a gambcini.scm file that defines all the features of the architecture.

For example, using Gambit on Windows / Intel in Debug mode, just start gsc in the bin/GambitWinIntelDebug
directory. You can then issue the various build commands described in the next section or load modules
interpreted.


Building
========

Build commands :
- (build) : core + language + platform modules
- (bjedi) : jedi critical modules

Note that you must restart gsc to use the newly compiled files.

Example
-------
From a new Jazz installation, the optimal build sequence is :

gsc
Gambit v4.1.0
> (build)
...
> (exit)

;; will have all core + language + platform modules compiled
gsc
Gambit v4.1.0
> (bjedi)
...
> (exit)

;; will have all jedi critical modules compiled
gsc
> (j)
...

Note that restarting gsc is entirely optional as most JazzScheme modules can run interpreted.


Jedi IDE
========

Launch
------
To launch Jedi, the Jazz IDE, you will first need to build the core and all platform modules using (build). If
it's the first time you build the core, it is recommended for better performance that you quit Gambit and relaunch
so the newly compiled files get loaded.

The (j) command will then launch the IDE.

Debug
-----
At the current time, the IDE relies totally on the underlying Scheme for debugging. This is not as painfull as would
appear as JazzScheme code is very close to the generated Scheme code so that inspecting a Jazz stack inside the Gambit
debugger is very easy. Having a lower level debugger is even a very nice feature as it is virtually impossible to crash
the IDE even when modifying a critical piece of code (like inserting a bug in the view system for instance) as the Gambit
debugger is completely self contained.

If you get an error you will find the IDE totally unresponsive as Gambit's debugger has now taken over. Just switch to the
Gambit console. You can now use all of Gambit's debugging command to debug your Jazz code and when you want to restart the
IDE's message loop, just clear the error level using Gambit's ,d command and then execute the restart command (r).


Documentation
=============

JazzScheme documentation is available in the doc directory and online at www.jazzscheme.org


------------------------------------------
Send comments, suggestions, bugs,... to:
gcartier@jazzscheme.org
