# Guix package repository for use at the BIMSB

This repository provides Guix package definitions for use at the BIMSB
in addition to package definitions that come with GNU Guix.  Some of
these package definitions won't be added to GNU Guix upstream as they
are variants of packages that are of limited interest outside of the
BIMSB.


# How to use

See [Specifying Additional
Channels](https://guix.gnu.org/manual/en/guix.html#Specifying-Additional-Channels)
in the Guix manual for instructions on how to add it to your installation
or simply add the following snippet to your `channels.scm`:

(channel
  (name 'guix-bimsb)
  (url "https://github.com/BIMSBbioinfo/guix-bimsb")
  (introduction
   (make-channel-introduction
    "574d4ac1889af550837476dc11c843312e75d93b"
    (openpgp-fingerprint
     "BCA6 89B6 3655 3801 C3C6  2150 197A 5888 235F ACAC"))))


# On Free Software

The GNU operating system has been developed so that users can have
freedom in their computing.  GNU is "free software", meaning that
users have the
[four essential freedoms](http://www.gnu.org/philosophy/free-sw.html):

0. to run the program
1. to study and change the program in source code form
2. to redistribute exact copies, and
3. to distribute modified versions.

Packages found in the Guix System Distribution provide only software
that conveys these four freedoms and that are permitted by the
[free software distribution guidelines](http://www.gnu.org/distros/free-system-distribution-guidelines.html).

Examples of non-free licenses include the original Artistic License or
open source licenses that forbid commercial usage.

If you consider releasing software, please avoid non-free licenses.  A
list of licenses that permit the above four freedoms and are
compatible with the GNU General Public License can be found on the
[Free Software Foundation's list of licenses](http://www.gnu.org/licenses/license-list.html#GPLCompatibleLicenses).
