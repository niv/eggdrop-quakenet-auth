##########################################
#
# maskhost.tcl
#  by Elven <elven@elven.de>
#  under the GNU/GPL
#  http://www.gnu.org/copyleft/gpl.html
#
#  Overwrites "maskhost" to support
#  efficient Quakenet-Mask bans.
#
#  To remove the script without restart-
#  ing the bot, run 'maskhost_remove'.
#
#  You can easily add support for more
#  masks/hosts/networks by adding more
#  if {}s.
#
##########################################

if {"" == [info commands maskhost_old]} { ;#only rename if its not already done
  rename maskhost maskhost_org
}


proc maskhost {host} {

  #Quakenet
  if {[string match -nocase "*.users.quakenet.org" $host]} {
    set rhost [lindex [split $host @] 1]
    if {$rhost == ""} { return [maskhost_org $host] } ;#let the original thing worry about that
    return "*!*@${rhost}"
  }
  
  return [maskhost_org $host]
}


proc maskhost_remove {} {
  catch { info body maskhost } res_mask
  if {"" == [info commands maskhost_old]} { return }
  rename maskhost ""
  rename maskhost_org maskhost
  rename maskhost_remove ""
}
