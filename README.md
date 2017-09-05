# Guix package repository for use at the BIMSB

This repository provides Guix package definitions for use at the BIMSB
in addition to package definitions that come with GNU Guix.  Some of
these package definitions won't be added to GNU Guix upstream as they
are variants of packages that are of limited interest outside of the
BIMSB.


# How to use

GNU Guix respects the `GUIX_PACKAGE_PATH` environment variable and
will prefer packages specified in the directories listed in this
variable over those that come with GNU Guix.

Here is an example of how to install a package defined in this
project, assuming that the contents of this repository are located in
`~/code/guix-bimsb`:

    export GUIX_PACKAGE_PATH="~/code/guix-bimsb"
    guix package -i dinup


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
