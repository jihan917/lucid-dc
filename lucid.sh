#! /bin/sh
# deployment and configuration of ubuntu (lucid) server-nox on a virtualbox.
# this is a recipe of interactive commands, not an automated script.
# *nondistributable*. kept here for my reference only.
# subject to changes (addition, deletion, or removal).
# you may use it at your own discretion and at your own risk.
# author: Ji Han (jihan917<at>yahoo<dot>com).

exit

##############################################################################
# the following assumes a minimal installation of lucid server without x,
# and root privilege.
sudo su

##############################################################################
# update grub.

cd /etc/default/grub
cp grub grub.orig
vim grub
# put down the changes for our record, once and for all.
diff -u grub.orig  grub > grub.patch
# the next time I setup a linux box, I'd:
#   $ wget http://github.com/jihan917/lucid-dc/raw/master/grub.patch
# and apply the patch.
cp grub.orig grub
patch grub < grub.patch
# reboot after updating grub.
update-grub
init 0

##############################################################################
# update sources list.

cd /etc/apt
cp sources.list sources.list.orig
vim sources.list
# the next time I could do:
#   $ mv sources.list sources.list.orig
#   $ wget http://github.com/jihan917/lucid-dc/raw/master/sources.list
aptitude update
aptitude safe-upgrade
init 0

##############################################################################
# install ssh server
#
# for NAT, do something like (suppose the vm is named `lucid-amd64'):
#   C:> VBoxManage modifyvm "lucid-amd64" --natpf1 "ssh,tcp,,2222,,22"
# to forward port 2222 of the host to port 22 of the guest vm.
#
# for bridged network and dhcp,
# edit /etc/network/interfaces and add the following two lines:
#   auto eth0
#   iface eth0 inet dhcp
#
# once we have the ssh service up running, we can start the vm headless:
#   C:> VBoxManage startvm "lucid-amd64" --type headless
# and ssh to the vm from the host.

aptitude install openssh-server
init 0

##############################################################################
# install http server
#
# to forward port 8080 of the host to port 80 of NAT guest vm:
#   C:> VBoxManage modifyvm "lucid-amd64" --natpf1 "http,tcp,,8080,,80"
#
# once we have the http server up running,
# we can use it to exchange files with the host (instead of psftp).

aptitude install nginx spawn-fcgi
# `update-rc.d -f nginx remove', if you use another http daemon(e.g. apache).
init 0

##############################################################################
# install essential software.

aptitude install p7zip-full
aptitude install cvs subversion git-core git-doc mercurial darcs
aptitude install autoconf automake build-essential flex bison
aptitude install gdb valgrind cmake doxygen
aptitude install vim-nox emacs23-nox

# the following is not needed but sometimes helpful.
aptitude install ctags cscope cogre cedet-common cedet-contrib ecb elib
aptitude install w3m-el-snapshot w3m-img

##############################################################################
# oracle (sun) jdk--the default jdk will pull in X11, which we don't want.

./jdk-6u21-linux-x64.bin
mv jdk1.6.0_21 /opt/
# need to setup $JAVA_HOME and $PATH before using it.
# cd /opt/jdk1.6.0_21/
# export JAVA_HOME=`pwd`
# export PATH=$PATH:$JAVA_HOME/bin

##############################################################################
# mingw32 cross compiler

# the default awk on debian is mawk; mingw32 requires gawk.
aptitude install gawk
aptitude install mingw32
i586-mingw32msvc-gcc -v

##############################################################################
# haskell
aptitude install ghc6 ghc6-doc ghc6-prof

# o'caml
aptitude install ocaml-nox ocaml-doc ocaml-native-compilers

# clozure cl
tar zxvf ccl-1.5-linuxx86.tar.gz --no-same-owner
mv ccl /opt
find /opt -type d -name '*.svn' -exec rm -fr {} \;

##############################################################################
# haskell mode, tuareg, and slime
# use the latest vanilla versions, not debian-patched ones.

mv haskell-mode-2.8.0.tar.gz /usr/share/emacs/site-lisp/
mv tuareg-2.0.1.tgz /usr/share/emacs/site-lisp/
cd /usr/share/emacs/site-lisp/
tar zxvf haskell-mode-2.8.0.tar.gz --no-same-owner
tar zxvf tuareg-2.0.1.tgz --no-same-owner
rm haskell-mode-2.8.0.tar.gz tuareg-2.0.1.tgz
cvs -d:pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime

##############################################################################
# create symlinks for haskell mode, tuareg, and slime.
# (debian installs emacs packages at /usr/share/emacs/site-lisp,
# which the load-path does not include.
# symlinks are created at /usr/share/emacs23/site-lisp,
# and emacs23 would load packages from there.)

cd /usr/share/emacs/site-lisp/
mkdir ../../emacs23/site-lisp/haskell-mode-2.8.0
cd haskell-mode-2.8.0/
for target in *; do
    ln -s ../../../emacs/site-lisp/haskell-mode-2.8.0/$target \
        ../../../emacs23/site-lisp/haskell-mode-2.8.0/$target;
done
cd ..

mkdir ../../emacs23/site-lisp/tuareg-2.0.1
cd tuareg-2.0.1/
for target in *; do
    ln -s ../../../emacs/site-lisp/tuareg-2.0.1/$target \
        ../../../emacs23/site-lisp/tuareg-2.0.1/$target;
done
cd ..

mkdir -p ../../emacs23/site-lisp/slime/contrib
cd slime/
for target in *; do
    ln -s ../../../emacs/site-lisp/slime/$target \
        ../../../emacs23/site-lisp/slime/$target;
done
cd contrib/
for target in *; do
    ln -s ../../../../emacs/site-lisp/slime/contrib/$target \
        ../../../../emacs23/site-lisp/slime/contrib/$target;
done

##############################################################################
